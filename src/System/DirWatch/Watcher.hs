{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.DirWatch.Watcher (
    StopCond
  , runWatchLoop
  , mkStopCond
  , endLoop
  , processWatcher
  , waitForJobsToComplete
) where

import Control.Applicative (Applicative, (<$>), (<*>), pure)
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Exception (try, finally)
import Control.Monad (forM_, forM, void, when, join)
import Control.Monad.Base (MonadBase)
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader(ask), asks, ReaderT, runReaderT)
import Control.Monad.State.Strict (
  MonadState(get,put), modify, gets, StateT, runStateT)
import Data.Conduit.Binary (sourceFile)
import Data.Default (def)
import Data.Either (partitionEithers)
import Data.Function (on)
import Data.Monoid (Monoid(..))
import Data.List (intercalate, foldl', find)
import Data.Fixed (Fixed, E2)
import Data.HashSet (HashSet)
import Data.Hashable (Hashable(hashWithSalt))
import qualified Data.HashSet as HS (
  union, empty, singleton, toList, fromList, filter, null, member, map)
import Data.HashMap.Strict as HM (
    HashMap, insertWith, unionWith, empty, elems, toList, fromList
  , fromListWith, lookup)
import Data.Maybe (catMaybes, fromMaybe)
import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (
  NominalDiffTime, UTCTime, getCurrentTime, utctDay, utctDayTime)
import System.INotify (
    INotify, Event (..), EventVariety (..), withINotify, addWatch)
import System.Directory (createDirectoryIfMissing, renameFile, doesFileExist)
import System.FilePath.Posix (takeDirectory)
import System.Posix.Files (getFileStatus, modificationTime)
import System.IO.Error (tryIOError)
import System.DirWatch.Config (
    RunnableConfig, RunnableWatcher, Config(..), Watcher(..), WatchedPath (..))
import System.DirWatch.Util (
    AbsPath, joinAbsPath, globMatch, takePatternDirectory, archiveDestination
  , toFilePath, absPathsMatching)

import System.DirWatch.Threading (
    SomeThreadHandle, toSomeThreadHandle, forkChild, waitChild
  , tryWaitSomeChild)
import System.DirWatch.Logging (logDebug, logInfo, logError, logWarn)
import System.DirWatch.Processor (
    ProcessorM, ProcessorConfig (..), ProcessorError, runProcessorM)
import System.DirWatch.PreProcessor (runPreProcessor, yieldFilePath)

type StopCond = MVar ()
type ThreadMap = HashMap AbsPath ThreadsPerPath
type DirMap = HashMap AbsPath (HashSet RunnableWatcher)

data RunningWatcher
  = RunningWatcher {
      rwHandle   :: !(SomeThreadHandle ProcessorError)
    , rwWatcher  :: !RunnableWatcher
    , rwStart    :: !POSIXTime
    }

isHandling :: ThreadMap -> RunnableWatcher -> AbsPath -> Bool
isHandling tmap w p
  = case HM.lookup p tmap of
      Just ThreadsPerPath{..} -> w `elem` (map rwWatcher (HS.toList tRunning))
      Nothing                 -> False

data ThreadsPerPath
  = ThreadsPerPath {
      tRunning  :: !(HashSet RunningWatcher)
    , tFinished :: !(HashSet RunningWatcher)
    , tFailed   :: !(HashSet RunningWatcher)
    }
initTPP :: [RunningWatcher] -> ThreadsPerPath
initTPP running = mempty {tRunning=HS.fromList running}

instance Monoid ThreadsPerPath where
  mempty = ThreadsPerPath HS.empty HS.empty HS.empty
  ThreadsPerPath a b c `mappend` ThreadsPerPath a' b' c'
    = ThreadsPerPath (HS.union a a') (HS.union b b') (HS.union c c')

instance Hashable RunningWatcher where
  hashWithSalt n = hashWithSalt n . rwWatcher

instance Eq RunningWatcher where
  (==) = (==) `on` rwWatcher

watchedEvents :: [EventVariety]
watchedEvents = [MoveIn, CloseWrite]

data ChanMessage
  = Work !(HashSet RunnableWatcher) !AbsPath
  | WakeUp
  | Finish

data WatcherEnv
  = WatcherEnv {
      wConfig   :: !RunnableConfig
    , wChan     :: !(Chan ChanMessage)
    , wInotify  :: !INotify
    , wDirMap   :: !DirMap
  }

askConfig :: (RunnableConfig -> a) -> WatcherM a
askConfig f = asks (f . wConfig) 


data WatcherState
  = WatcherState {
      wThreads :: !ThreadMap
  }
instance Monoid WatcherState where
  mempty  = WatcherState mempty
  mappend a b = WatcherState (unionWith mappend (wThreads a) (wThreads b))

newtype WatcherM a
  = WatcherM {
      unWatcherM :: ReaderT WatcherEnv (StateT WatcherState IO) a
      }
  deriving ( Functor, Applicative, Monad, MonadReader WatcherEnv
           , MonadState WatcherState, MonadIO, MonadBase IO)

instance MonadBaseControl IO WatcherM where
   type StM WatcherM a = (a, WatcherState)
   liftBaseWith f = do
     env <- ask
     state <- get
     liftIO $ f (runWatcherEnv env state)
   restoreM (a, state) = put state >> return a

runWatcherEnv
  :: WatcherEnv -> WatcherState -> WatcherM a -> IO (a, WatcherState)
runWatcherEnv env state
  = flip runStateT state
  . flip runReaderT env
  . unWatcherM

runWatchLoop
  :: RunnableConfig -> Maybe WatcherState -> StopCond -> IO WatcherState
runWatchLoop cfg mState stopCond = withINotify $ \ino -> do
  let dirMap = mkDirMap (cfgWatchers cfg)
  env <- WatcherEnv <$> pure cfg <*> newChan <*> pure ino <*> pure dirMap
  let state = fromMaybe mempty mState
  loopth <- forkChild $ runWatcherEnv env state
                        (setupWatches >> checkExistingFiles >> loop)
  takeMVar stopCond
  writeChan (wChan env) Finish
  fmap snd $ waitChild loopth

mkDirMap :: [RunnableWatcher] -> DirMap
mkDirMap ws = foldl' go empty $ [(w,p) | w<-ws, p<-wGlobs w]
  where
    go m (w,p) = insertWith HS.union (takePatternDirectory p) (HS.singleton w) m

endLoop :: StopCond -> IO ()
endLoop mvar = putMVar mvar ()

mkStopCond :: IO StopCond
mkStopCond = newEmptyMVar

setupWatches :: WatcherM ()
setupWatches = do
  dir_watchers <- asks (toList . wDirMap)
  chan <- asks wChan
  forM_ dir_watchers $ \(baseDir, watchers) -> do
    let names = intercalate ", " $ map wName (HS.toList watchers)
        handleFile filePath = do
          let matched = HS.filter (any (p `globMatch`) . wGlobs) watchers
              p = joinAbsPath baseDir [filePath]
          when (not (HS.null matched)) $ do
            $(logInfo) $ concat [show p," has arrived"]
            writeChan chan $ Work matched p
    mError <- addIWatch baseDir $ \case
                MovedIn{isDirectory=False, ..}       -> handleFile filePath
                Closed{ wasWriteable=True, isDirectory=False
                      , maybeFilePath=Just filePath} -> handleFile filePath
                _ -> return ()
    case mError of
      Just e ->
        $(logWarn) $ concat ["Could not watch ", show baseDir, " for "
                                 , names, ": ", show e]
      Nothing ->
        $(logInfo) $ concat ["Watching ", show baseDir, " for ", names]

loop :: WatcherM ()
loop = do
  handleFinishedFiles
  msg <- getMessage
  case msg of
    Work ws file -> runWatchersOnFile (HS.toList ws) file >> loop
    WakeUp       -> loop
    Finish       -> return ()

checkExistingFiles :: WatcherM ()
checkExistingFiles = do
  dir_watchers <- asks wDirMap
  waitSecs <- askConfig cfgStableTime
  chan <- asks wChan
  tmap <- gets wThreads
  let filterPaths w = filter (not . isHandling tmap w)
  void $ liftIO $ forkChild $ do
    existingMap <- fmap (fromListWith HS.union . concat . concat . concat) $
      forM (elems dir_watchers) $ \watchers -> do
        forM (HS.toList watchers) $ \watcher -> do
          forM (wGlobs watcher) $ \globPattern ->  do
            paths <- fmap (filterPaths watcher) (absPathsMatching globPattern)
            forM paths $ \abspath -> return (abspath, HS.singleton watcher)
    existing <- forM (toList existingMap) $ \(abspath, watchers) -> do
      mt <- fmap modificationTime $ getFileStatus (toFilePath abspath)
      return (mt, watchers, abspath)
    sleep waitSecs
    forM_ existing $ \(modTime, watchers, abspath) -> do
      eModTime <- tryIOError $ -- File might have been handled while we were
                               -- waiting and not be there anymore
                  fmap modificationTime (getFileStatus (toFilePath abspath))
      when (eModTime == Right modTime) $ do
        -- file has not been modified, assume it is stable and work on it.
        -- If it has been modified we can ignore it assuming we'll be notified
        -- when it has been closed
        $(logInfo) $ concat ["File ", show abspath, " has been stable"]
        writeChan chan $ Work watchers abspath
      

addIWatch :: AbsPath -> (Event -> IO ()) -> WatcherM (Maybe IOError)
addIWatch dir wch = do
  ino <- asks wInotify
  result <- liftIO . try $ addWatch ino watchedEvents (toFilePath dir) wch
  either (return . Just) (const (return Nothing)) result


getMessage :: WatcherM ChanMessage
getMessage = asks wChan >>= liftIO . readChan

handleFinishedFiles :: WatcherM ()
handleFinishedFiles = do
  tsL <- gets (HM.toList . wThreads)
  remaining <- fmap catMaybes $ forM tsL $ \(fname,tmap) -> do
    batch <- forM (HS.toList (tRunning tmap)) $ \fp -> do
      result <- liftIO $ tryWaitSomeChild (rwHandle fp)
      case result of
        Nothing      -> return $ Right (Right fp) -- still running
        Just Nothing -> return $ Right (Left fp)  -- finished
        Just _       -> return $ Left fp          -- finished with error
    let tmap' =
          ThreadsPerPath {
              tRunning  = HS.fromList stillRunning
            , tFailed   = HS.union (tFailed tmap) (HS.fromList failedNow)
            , tFinished = HS.union (tFinished tmap) (HS.fromList finishedNow)
            }
        (finishedNow, stillRunning) = partitionEithers nonFailures
        (failedNow, nonFailures)    = partitionEithers batch
        hadFailures  = not . HS.null . tFailed   $ tmap'
        someFinished = not . HS.null . tFinished $ tmap'
        anyRunning   = not . HS.null . tRunning  $ tmap'
    case (hadFailures, someFinished, anyRunning) of
      (False, _, False) -> do
        $(logInfo) $ concat [ "Finished processing ", show fname
                            , " without errors"]
        archiveFile fname
        return Nothing
      (True, True, False) -> do
        $(logWarn) $ concat [ "Finished processing ", show fname
                            , " with some errors"]
        archiveFile fname
        return Nothing
      (True,False,False) -> do
        $(logError) $ concat [ "All watchers failed on ", show fname]
        return Nothing
      (_,_,True) -> return $ Just (fname,tmap')
  modify $ \s -> s {wThreads=HM.fromList remaining}

sleep :: NominalDiffTime -> IO ()
sleep = threadDelay . floor . (*1000000)

retryText :: Int -> String
retryText num
  | num == 0  = ""
  | otherwise = concat [prefix, show num, numSuffix, suffix]
  where numSuffix
          | num /= 11
          , num `mod` 10 == 1 = "st"
          | num /= 12
          , num `mod` 10 == 2 = "nd"
          | num /= 13
          , num `mod` 10 == 3 = "rd"
          | otherwise         = "th"
        prefix = "for the "
        suffix = " time"

archiveFile :: AbsPath -> WatcherM ()
archiveFile fname = do
  noArchives <- askConfig cfgNoArchives
  let shouldArchive = all (not . (fname `globMatch`)) noArchives
  when shouldArchive $ do
    mArchiveDir <- askConfig cfgArchiveDir
    case mArchiveDir of
      Just archiveDir -> do
        time <- liftIO getCurrentTime
        let dest = toFilePath
                     (archiveDestination archiveDir (utctDay time) fname)
            destDir = takeDirectory dest
        exists <- liftIO $ doesFileExist dest
        let finalDest
              | exists    = dest ++ "." ++ show secs
              | otherwise = dest
            secs = realToFrac (utctDayTime time) :: Fixed E2
        result <- liftIO . tryIOError $ do
          createDirectoryIfMissing True destDir
          renameFile (toFilePath fname) finalDest
        case result of
          Left e -> do
            $(logWarn) $ concat [ "Could not archive ", show fname, ": "
                                , show e]
          Right () ->
            $(logInfo) $ concat ["Archived ", show fname, " -> ", finalDest]
      Nothing -> return ()



              
runWatchersOnFile :: [RunnableWatcher] -> AbsPath -> WatcherM ()
runWatchersOnFile toAdd filename = do
  ts <- gets wThreads
  let mRunning = fmap (HS.map rwWatcher . tRunning) (HM.lookup filename ts)  
      watchers = case mRunning of
        Just runSet -> filter (not . (`HS.member` runSet)) toAdd
        Nothing     -> toAdd
  if null watchers
    then $(logWarn) $
            concat ["File ", show filename, " is already being processed"]
    else do
      cfg <- processorConfig
      chan <- asks wChan
      start <- liftIO getPOSIXTime
      let time = posixSecondsToUTCTime start
      numRetries <- askConfig cfgNumRetries
      retryInterval <- askConfig cfgRetryInterval
      fps <- forM watchers $ \w -> do
        let process n = do
              result <- runProcessorM cfg time $
                processWatcher startUTC filename w
              case (result, n<=numRetries) of
                (Left e, True) -> do
                  $(logWarn) $ concat $
                    errMsg e n ++ [" Will retry in ", show retryInterval]
                  sleep retryInterval
                  process (n+1)
                (Left e, False) -> do
                  $(logError) $ concat $ errMsg e n
                  return result
                (Right _, _) -> return result
            startUTC = posixSecondsToUTCTime start
            errMsg e n = ["Watcher ", show (wName w), " failed"] ++
                         (if numRetries>0 then [" ",retryText n, ": ", show e]
                                          else [])
        liftIO $ do
          th <- forkChild $ finally (process 1) (writeChan chan WakeUp)
          return RunningWatcher { rwHandle   = toSomeThreadHandle th
                                , rwStart    = start
                                , rwWatcher  = w}
      addToRunning filename fps

processWatcher :: UTCTime -> AbsPath -> RunnableWatcher -> ProcessorM ()
processWatcher now abspath Watcher{..} = do
  $(logDebug) $ concat ["Running \"", wName, "\" on ", show abspath]
  case wProcessor of
    Just processor  -> do
      let pathPreproc  = join $ fmap wpPreprocessor $
                           find (globMatch abspath . wpGlob) wPaths
          preprocessor = fromMaybe yieldFilePath pathPreproc
          preprocess   = runPreProcessor source now (preprocessor filepath)
          process      = uncurry processor
          source       = sourceFile filepath
          filepath     = toFilePath abspath
      preprocess >>= mapM_ process
      $(logDebug) $ concat ["Finished \"", wName, "\" on ", show abspath]
    Nothing -> $(logInfo) $ concat [wName, " has no processor"]


wGlobs :: Watcher a b -> [AbsPath]
wGlobs = map wpGlob . wPaths

processorConfig :: WatcherM ProcessorConfig
processorConfig = do
  env <- askConfig cfgShellEnv
  return $ def {pShellEnv = env}


addToRunning
  :: AbsPath
  -> [RunningWatcher]
  -> WatcherM ()
addToRunning path fps = do
  ts <- gets wThreads
  modify $ \s -> s {wThreads = insertWith mappend path (initTPP fps) ts}

waitForJobsToComplete :: WatcherState -> IO ()
waitForJobsToComplete state = do
  when (length runningThreads > 0) $ do
    $(logInfo) $ concat [ "Waiting at most ", show maxWait, "s for "
                        , show (length runningThreads), " threads to complete"]
    go runningThreads 0
  where
    maxWait = 30
    go :: [SomeThreadHandle a] -> Int -> IO ()
    go [] _        = return ()
    go xs  n | n>maxWait = do
      $(logError) $ concat [show (length xs), " threads have not completed"]
      return ()
    go threads n = do
      when (length threads > 0) $ do
        stillRunning <- fmap catMaybes $ forM threads $ \t ->
          tryWaitSomeChild t >>= return . maybe (Just t) (const Nothing)
        sleep 1
        go stillRunning (n+1)
    runningThreads = map rwHandle . concat . map (HS.toList . tRunning) 
                   . HM.elems . wThreads $ state

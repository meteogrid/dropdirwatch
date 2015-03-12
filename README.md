Dropdirwatch
============

[![Build Status](https://travis-ci.org/meteogrid/dropdirwatch.svg?branch=master)](https://travis-ci.org/meteogrid/dropdirwatch)

Perform action when files are created on a filesystem

Dropdirwatch is can be configured to perform actions when files are created in
certain directories.

Actions can be shell commands, Haskell code written inline in the YAML config
file or Haskell plugins stored in the file system.

The config file is automatically reloaded when modified (if it compiles
correctly) as well as the plugins.

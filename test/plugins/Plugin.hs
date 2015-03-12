
newtype Prefix = Prefix String

addPrefix (Prefix prefix) path contents
  = [(API.replaceBaseName path prefix, contents)]

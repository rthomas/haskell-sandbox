module Class.Types.Interfaces where

import Data.Word

data Interfaces = Interfaces {
  interfacesCount :: Word16,
  interfaces :: [Word16]
  } deriving (Show)

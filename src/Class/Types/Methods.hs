module Class.Types.Methods where

import Class.Types.Attributes
import Data.Word

data Methods = Methods {
  methodsCount :: Word16,
  methods :: [MethodInfo]
  } deriving (Show)

data MethodInfo = MethodInfo {
  accessFlags :: Word16,
  nameIndex :: Word16,
  descriptorIndex :: Word16,
  attributes :: Attributes
  } deriving (Show)

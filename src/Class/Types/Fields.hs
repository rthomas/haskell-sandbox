module Class.Types.Fields where

import Class.Types.Attributes
import Data.Word

data Fields = Fields {
  fieldCount :: Word16,
  fieldInfo :: [FieldInfo]
  } deriving (Show)

data FieldInfo = FieldInfo {
  accessFlags :: Word16,
  nameIndex :: Word16,
  descriptorIndex :: Word16,
  attributess :: Attributes
  } deriving (Show)

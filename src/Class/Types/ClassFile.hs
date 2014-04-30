module Class.Types.ClassFile where

import Class.Types.Attributes
import Class.Types.ConstantPool
import Class.Types.Fields
import Class.Types.Interfaces
import Class.Types.Methods
import Data.Word

data ClassFile = ClassFile {
  header :: Header,
  constantPool :: ConstantPool,
  classInfo :: ClassInfo,
  interfaces :: Interfaces,
  fields :: Fields,
  methods :: Methods,
  attributes :: Attributes
  } deriving (Show)

data Header = Header {
  magic :: Word32,
  minor :: Word16,
  major :: Word16
  } deriving (Show)

data ClassInfo = ClassInfo {
  classAccessFlags :: Word16,
  thisClass :: Word16,
  superClass :: Word16
  } deriving (Show)



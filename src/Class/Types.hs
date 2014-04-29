module Class.Types where

import Data.Word
import Data.ByteString

data ClassFile = ClassFile {
  header :: Header,
  constantPool :: ConstantPool,
  classInfo :: ClassInfo
  } deriving (Show)

data Header = Header {
  magic :: Word32,
  minor :: Word16,
  major :: Word16
  } deriving (Show)

data ConstantPool = ConstantPool {
  count :: Word16,
  cpInfo :: [ConstantPoolInfo]
  } deriving (Show)

data ClassInfo = ClassInfo {
  accessFlags :: Word16,
  thisClass :: Word16,
  superClass :: Word16,
  interfacesCount :: Word16,
  interfaces :: ByteString
  } deriving (Show)

data ConstantPoolInfo = ConstantClass {
  -- Class: 7
  tag :: Word8,
  nameIndex :: Word16
  } | ConstantFieldRef {
  -- FiledRef: 9
  tag :: Word8,
  classIndex :: Word16,
  nameAndTypeIndex :: Word16
  } | ConstantMethodRef {
  -- MethodRef: 10
  tag :: Word8,
  classIndex :: Word16,
  nameAndTypeIndex :: Word16
  } | ConstantInterfaceMethodRef {
  -- InterfaceMethodRef: 11
  tag :: Word8,
  classIndex :: Word16,
  nameAndTypeIndex :: Word16
  } | ConstantString {
  -- String: 8
  tag :: Word8,
  stringIndex :: Word16
  } | ConstantInteger {
  -- Integer: 3
  tag :: Word8,
  bytes :: Word32
  } | ConstantFloat {
  -- Float: 4
  tag :: Word8,
  bytes :: Word32
  } | ConstantLong {
  -- Long: 5
  tag :: Word8,
  highBytes :: Word32,
  lowBytes :: Word32
  } | ConstantDouble {
  -- Double: 6
  tag :: Word8,
  highBytes :: Word32,
  lowBytes :: Word32
  } | ConstantNameAndType {
  -- NameAndType: 12
  tag :: Word8,
  nameIndex :: Word16,
  descriptorIndex :: Word16
  } | ConstantUtf8 {
  -- Utf8: 1
  tag :: Word8,
  length :: Word16,
  bytesString :: ByteString
  } | ConstantMethodHandle {
  -- MethodHandle: 15
  tag :: Word8,
  referenceKind :: Word8,
  referenceIndex :: Word16
  } | ConstantMethodType {
  -- MethodType: 16
  tag :: Word8,
  descriptorIndex :: Word16
  } | ConstantInvokeDynamic {
  -- InvokeDynamic: 18
  tag :: Word8,
  bootstrapMethodAttrIndex :: Word16,
  nameAndTypeIndex :: Word16
  } deriving (Show)

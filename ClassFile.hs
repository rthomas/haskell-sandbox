module ClassFile where

import Data.Binary.Get
import Data.Word
import ClassTypes

readClass :: Get ClassFile

readClass = do
  header <- readHeader
  constantPool <- readConstantPool
  classInfo <- readClassInfo
  return $ ClassFile header constantPool classInfo

readHeader :: Get Header

readHeader = do
  magic <- getWord32be
  minor <- getWord16be
  major <- getWord16be
  return $ Header magic minor major

readConstantPool :: Get ConstantPool

readConstantPool = do
  count <- getWord16be
  cpList <- readConstantPoolInfos $ (fromIntegral (count) :: Int) - 1
  return $ ConstantPool count cpList

readClassInfo :: Get ClassInfo

readClassInfo = do
  accessFlags <- getWord16be
  thisClass <- getWord16be
  superClass <- getWord16be
  interfacesCount <- getWord16be
  interfaces <- getByteString (fromIntegral (interfacesCount) :: Int)
  return $ ClassInfo accessFlags thisClass superClass interfacesCount interfaces

readConstantPoolInfos :: Int -> Get [ConstantPoolInfo]

readConstantPoolInfos 0 = do
  return []

readConstantPoolInfos i = do
  tag <- readConstantPoolTag
  cpInfo <- readConstantPoolInfo tag
  cpList <- readConstantPoolInfos (i-1)
  return $ cpInfo : cpList

readConstantPoolTag :: Get Word8

readConstantPoolTag = do
  tag <- getWord8
  return tag

readConstantPoolInfo :: Word8 -> Get ConstantPoolInfo

readConstantPoolInfo 1 = do
  length <- getWord16be
  byteString <- getByteString (fromIntegral (length) :: Int)
  return $ ConstantUtf8 1 length byteString

readConstantPoolInfo 3 = do
  bytes <- getWord32be
  return $ ConstantInteger 3 bytes

readConstantPoolInfo 4 = do
  bytes <- getWord32be
  return $ ConstantFloat 4 bytes

readConstantPoolInfo 5 = do
  highBytes <- getWord32be
  lowBytes <- getWord32be
  return $ ConstantLong 5 highBytes lowBytes

readConstantPoolInfo 6 = do
  highBytes <- getWord32be
  lowBytes <- getWord32be
  return $ ConstantDouble 6 highBytes lowBytes

readConstantPoolInfo 7 = do
  nameIndex <- getWord16be
  return $ ConstantClass 7 nameIndex

readConstantPoolInfo 8 = do
  stringIndex <- getWord16be
  return $ ConstantString 8 stringIndex

readConstantPoolInfo 9 = do
  classIndex <- getWord16be
  nameAndTypeIndex <- getWord16be
  return $ ConstantFieldRef 9 classIndex nameAndTypeIndex

readConstantPoolInfo 10 = do
  classIndex <- getWord16be
  nameAndTypeIndex <- getWord16be
  return $ ConstantMethodRef 10 classIndex nameAndTypeIndex

readConstantPoolInfo 11 = do
  classIndex <- getWord16be
  nameAndTypeIndex <- getWord16be
  return $ ConstantInterfaceMethodRef 11 classIndex nameAndTypeIndex

readConstantPoolInfo 12 = do
  nameIndex <- getWord16be
  descriptorIndex <- getWord16be
  return $ ConstantNameAndType 12 nameIndex descriptorIndex

readConstantPoolInfo 15 = do
  referenceKind <- getWord8
  referenceInfo <- getWord16be
  return $ ConstantMethodHandle 15 referenceKind referenceInfo

readConstantPoolInfo 16 = do
  descriptorIndex <- getWord16be
  return $ ConstantMethodType 16 descriptorIndex

readConstantPoolInfo 18 = do
  bootstrapMethodAttrIndex <- getWord16be
  nameAndTypeIndex <- getWord16be
  return $ ConstantInvokeDynamic 18 bootstrapMethodAttrIndex nameAndTypeIndex

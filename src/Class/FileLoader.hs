module Class.FileLoader where

import Class.Types.Attributes
import Class.Types.ClassFile
import Class.Types.ConstantPool
import Class.Types.Fields
import Class.Types.Interfaces
import Class.Types.Methods
import Data.Binary.Get
import Data.ByteString
import Data.Word

readClass :: Get ClassFile

readClass = do
  header <- readHeader
  constantPool <- readConstantPool
  classInfo <- readClassInfo
  interfaces <- readInterfaces
  fields <- readFields
  methods <- readMethods
  attributes <- readAttributes
  transformedAttributes <- transformAttributes attributes constantPool
  return $ ClassFile header constantPool classInfo interfaces fields methods transformedAttributes

transformAttributes :: Attributes -> ConstantPool -> Get Attributes

transformAttributes attributes constantPool = do
  return $ Attributes (attributesCount attributes) $ transformAttributeInfos (attributesInfo attributes) constantPool

transformAttributeInfos :: [AttributeInfo] -> ConstantPool -> [AttributeInfo]

transformAttributeInfos [] _ = []

transformAttributeInfos (x:xs) constantPool = do
  x : xs

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
  return $ ClassInfo accessFlags thisClass superClass

readInterfaces :: Get Interfaces

readInterfaces = do
  count <- getWord16be
  interfaces <- readInterfacesEntries $ (fromIntegral (count) :: Int)
  return $ Interfaces count interfaces

readInterfacesEntries :: Int -> Get [Word16]

readInterfacesEntries 0 = do return []

readInterfacesEntries i = do
  interfaceEntry <- getWord16be
  interfaceList <- readInterfacesEntries (i-1)
  return $ interfaceEntry : interfaceList

readFields :: Get Fields

readFields = do
  count <- getWord16be
  fields <- readFieldsInfos $ (fromIntegral (count) :: Int)
  return $ Fields count fields

readFieldsInfos :: Int -> Get [FieldInfo]

readFieldsInfos 0 = do return []

readFieldsInfos i = do
  fieldEntry <- readFieldInfo
  fieldEntriesList <- readFieldsInfos (i-1)
  return $ fieldEntry : fieldEntriesList

readFieldInfo :: Get FieldInfo

readFieldInfo = do
  accessFlags <- getWord16be
  nameIndex <- getWord16be
  descriptorIndex <- getWord16be
  attributes <- readAttributes
  return $ FieldInfo accessFlags nameIndex descriptorIndex attributes

readMethods :: Get Methods

readMethods = do
  count <- getWord16be
  methods <- readMethodInfos $ (fromIntegral (count) :: Int)
  return $ Methods count methods

readMethodInfos :: Int -> Get [MethodInfo]

readMethodInfos 0 = do return []

readMethodInfos i = do
  methodInfo <- readMethodInfo
  methodInfos <- readMethodInfos (i-1)
  return $ methodInfo : methodInfos

readMethodInfo :: Get MethodInfo

readMethodInfo = do
  accessFlags <- getWord16be
  nameIndex <- getWord16be
  descriptorIndex <- getWord16be
  attributes <- readAttributes
  return $ MethodInfo accessFlags nameIndex descriptorIndex attributes

readAttributes :: Get Attributes

readAttributes = do
  count <- getWord16be
  attributes <- readAttributesInfos $ (fromIntegral (count) :: Int)
  return $ Attributes count attributes

readAttributesInfos :: Int -> Get [AttributeInfo]

readAttributesInfos 0 = do return []

readAttributesInfos i = do
  attributeInfo <- readAttributeInfo
  attributeInfos <- readAttributesInfos (i-1)
  return $ attributeInfo : attributeInfos

readAttributeInfo :: Get AttributeInfo

readAttributeInfo = do
  attributeName <- getWord16be
  attributeLength <- getWord32be
  info <- getByteString $ (fromIntegral (attributeLength) :: Int)
  return $ AttributeInfo attributeName attributeLength (unpack (info))

readConstantPoolInfos :: Int -> Get [ConstantPoolInfo]

readConstantPoolInfos 0 = do return []

readConstantPoolInfos i = do
  tag <- readConstantPoolTag
  cpInfo <- readConstantPoolInfo tag
  cpList <- readConstantPoolInfos (i-1)
  return $ cpInfo : cpList

readConstantPoolTag :: Get Word8

readConstantPoolTag = getWord8

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

readConstantPoolInfo i = fail $ "Unknown tag: " ++ show (i)

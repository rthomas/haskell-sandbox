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
  classAccessFlags :: Word16,
  thisClass :: Word16,
  superClass :: Word16,
  interfacesCount :: Word16,
  interfaces :: ByteString
  } deriving (Show)

data Fields = Fields {
  fieldCount :: Word16,
  fieldInfo :: [FieldInfo]
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

data FieldInfo = FieldInfo {
  fieldAccessFlags :: Word16,
  fieldNameIndex :: Word16,
  fieldDescriptorIndex :: Word16,
  fieldAttributesCount :: Word16,
  fieldAttributeInfo :: [AttributeInfo]
  } deriving (Show)

data AttributeInfo = ConstantValueAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  constantValueIndex :: Word16
  } | CodeAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  maxStack :: Word16,
  maxLocals :: Word16,
  codeLength :: Word32,
  code :: ByteString,
  exceptionTableLength :: Word16,
  exceptionTable :: [ExceptionTableEntry],
  attributesCount :: Word16,
  attributeInfo :: [AttributeInfo]
  } | StackMapTableAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  numberOfEntries :: Word16,
  entries :: [StackMapFrame]
  } | ExceptionsAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  numberOfExceptions :: Word32,
  exceptionIndexTable :: [Word16]
  } | InnerClassesAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  numberOfClasses :: Word16,
  classes :: [InnerClassInfo]
  } | EnclosingMethodAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  attributeClassIndex :: Word16,
  methodIndex :: Word16
  } deriving (Show)

data ExceptionTableEntry = ExceptionTableEntry {
  startPc :: Word16,
  endPc :: Word16,
  handlerPc :: Word16,
  catchType :: Word16
  } deriving (Show)

data InnerClassInfo = InnerClassInfo {
  innerClassInfoIndex :: Word16,
  outerClassInfoIndex :: Word16,
  innerNameIndex :: Word16,
  innerClassAccessFlags :: Word16
  } deriving (Show)

data StackMapFrame = SameFrame {
  frameType :: Word8 -- 0-63
  } | SameLocals1StackItemFrame {
  frameType :: Word8, -- 64-127
  stack :: VerificationTypeInfo
  } | SameLocals1StackItemFrameExtended {
  frameType :: Word8, -- 247
  offsetDelta :: Word16,
  stack :: VerificationTypeInfo
  } | ChopFrame {
  frameType :: Word8, -- 248-250
  offsetDelta :: Word16
  } | SameFrameExtended {
  frameType :: Word8, -- 251
  offsetDelta :: Word16
  } | AppendFrame {
  frameType :: Word8, -- 252-254
  offsetDelta :: Word16,
  locals :: [VerificationTypeInfo] -- length: frameType - 251
  } | FullFrame {
  frameType :: Word8, -- 255
  offsetDelta :: Word16,
  numberOfLocals :: Word16,
  locals :: [VerificationTypeInfo],
  numberOfStackItems :: Word16,
  stackItems :: [VerificationTypeInfo]
  } deriving (Show)

data VerificationTypeInfo = TopVariableInfo {
  infoTag :: Word8 -- 0
  } | IntegerVariableInfo {
  infoTag :: Word8 -- 1
  } | FloatVariableInfo {
  infoTag :: Word8 -- 2
  } | LongVariableInfo {
  infoTag :: Word8 -- 4
  } | DoubleVariableInfo {
  infoTag :: Word8 -- 3
  } | NullVariableInfo {
  infoTag :: Word8 -- 5
  } | UninitializedThisVariableInfo {
  infoTag :: Word8 -- 6
  } | ObjectVariableInfo {
  infoTag :: Word8, -- 7
  cPoolIndex :: Word16
  } | UninitializedVariableInfo {
  infoTag :: Word8, -- 8
  offset :: Word16
  } deriving (Show)

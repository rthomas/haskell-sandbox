module Class.Types.Attributes where

import Data.ByteString
import Data.Word

data Attributes = Attributes {
  attributesCount :: Word16,
  attributesInfo :: [AttributeInfo]
  } deriving (Show)

data AttributeInfo = AttributeInfo {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  attributeInfo :: ByteString
  } | ConstantValueAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  constantValueIndex :: Word16
  } | CodeAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  maxStack :: Word16,
  maxLocals :: Word16,
  codeLength :: Word32,
  code :: [Word8],
  exceptionTableLength :: Word16,
  exceptionTable :: [ExceptionTableEntry],
  attributes :: Attributes
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
  } | SynthenticAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32
  } | SignatureAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  signatureIndex :: Word16
  } | SourceFileAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  sourceFileIndex :: Word16
  } | SourceDebugExtension {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  debugExtension :: [Word8]
  } | LineNumberTableAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  lineNumberTableLength :: Word16,
  lineNumberTable :: [LineNumberTableEntry]
  } | LocalVariableTableAttribute {
  attributeNameIndex :: Word16,
  attributeLength :: Word32,
  localVariableTableLength :: Word16,
  localVariableTable :: [LocalVariableTableEntry]
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

data LineNumberTableEntry = LineNumberTableEntry {
  lineNumberStartPc :: Word16,
  lineNumber :: Word16
  } deriving (Show)

data LocalVariableTableEntry = LocalVariableTableEntry {
  localVarStartPc :: Word16,
  localVarLength :: Word16,
  localVarNameIndex :: Word16,
  localVarDescriptorIndex :: Word16,
  localVarIndex :: Word16
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

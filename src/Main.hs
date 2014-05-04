import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Class.FileLoader
import Class.Types.ClassFile

main :: IO()
main = do
  input <- BL.getContents
  let classFile = runGet readClass input
  -- print $ length $ show classFile
  print $ (attributes classFile)

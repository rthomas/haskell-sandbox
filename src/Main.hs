import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import Class.FileLoader

main :: IO()
main = do
  input <- BL.getContents
  print $ runGet readClass input



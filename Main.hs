import qualified Data.ByteString.Lazy as BL
import Data.Binary.Get
import ClassFileLoader

main :: IO()
main = do
  input <- BL.getContents
  print $ runGet readClass input



import Control.Monad
import Control.Concurrent
import Data.Serialize
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Network.Info
import System.Environment
import Network.OpenFlow

import Data.ByteString as BS hiding (putStrLn)
import Data.Word
import Data.Bits

fileToWordList :: String -> IO [Word8]
fileToWordList f = do
  dat <- BS.readFile f
  return $ unpack dat

main :: IO ()
main = do
  let file = "LAG_secchan.pcap"
  dat <- fileToWordList file
  putStrLn "done"
  --putStrLn $ "file: " ++ show dat

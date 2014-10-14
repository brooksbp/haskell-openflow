module Main where

import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Data.Binary.Get (runGet)
import Data.Binary.Put (runPut)
import qualified Data.ByteString.Lazy as BS
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Prelude hiding (getContents)
import Network.OpenFlow
import GHC.Int

main :: IO ()
main = do
  addrinfos <- getAddrInfo
               (Just (defaultHints { addrFlags = [AI_PASSIVE] }))
               Nothing (Just "6633")
  let serveaddr = head addrinfos
  sock <- socket (addrFamily serveaddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress serveaddr)
  listen sock 4
  forever $ do
    (csock, _) <- accept sock
    handshake csock
    _ <- forkIO $ handleSwitch csock
    return ()

handshake :: Socket -> IO ()
handshake sock = do
  sendAll sock $ runPut (putOfpFrame (OfpFrame (OfpHeader 1 OfptHello 0 0) (Body [])))
  frame <- readOfpFrame sock
  putStrLn $ "RX: " ++ show frame

handleSwitch :: Socket -> IO ()
handleSwitch sock =
  forever $ do
    frame <- readOfpFrame sock
    putStrLn $ "RX: " ++ show frame
    case frame of
      _ -> putStrLn "UNHANDLED"

ofpHdrLen :: Int64
ofpHdrLen = 8

readOfpFrame :: Socket -> IO OfpFrame
readOfpFrame sock = do
  dat <- recvExact ofpHdrLen
  let hdrLen = fromIntegral (len (runGet getOfpHeader dat))
      bytesLeft = hdrLen - ofpHdrLen
  if bytesLeft /= 0
    then do dat' <- recvExact bytesLeft
            return $ runGet getOfpFrame (BS.append dat dat')
    else error "only hdr recvd"
  where
    recvExact bytes = do
      b <- recvExact' bytes BS.empty
      if BS.length b /= fromIntegral bytes
        then error "expected.."
        else return b
    recvExact' bytes buf = do
      dat <- recv sock bytes
      let datLen = BS.length dat
      if datLen == 0
        then error "peer closed connection"
        else do let buf' = BS.append buf dat
                if datLen >= bytes
                  then return buf'
                  else recvExact' (bytes - datLen) buf'

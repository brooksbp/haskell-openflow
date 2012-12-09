import Control.Monad
import Control.Concurrent
import Data.Serialize
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Network.OpenFlow
import Network.Info
import System.Environment

main :: IO ()
main = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "6633")
  let serveaddr = head addrinfos
  sock <- socket (addrFamily serveaddr) Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bindSocket sock (addrAddress serveaddr)
  listen sock 4
  serveConns sock
  
serveConns :: Socket -> IO ()
serveConns sock =
  forever $ do
    (csock, caddr) <- accept sock
    handshake csock
    _ <- forkIO $ handleSwitch csock caddr
    return ()


emptyMac = MAC 0x00 0x00 0x00 0x00 0x00 0x00

inp  = 1
outp = 6

wc = [--OfpfwInPort,
  OfpfwDlVlan, OfpfwDlSrc
  , OfpfwDlDst, OfpfwDlType, OfpfwNwProto
  , OfpfwTpSrc, OfpfwTpDst, OfpfwNwSrcShift
  , OfpfwNwSrcBits, OfpfwNwSrcMask, OfpfwNwSrcAll
  , OfpfwDlVlanPcp, OfpfwNwTos, OfpfwAll
  ]
m = OfpMatch wc inp emptyMac emptyMac 0 0 0 0 0 0 0 0 0

act1 = OfpOutput 5 256
act2 = OfpOutput 6 256
uint32max = 0xffffffff
pri = 1
flowMod = OfpFlowMod m 0 OfpfcAdd 360 360 pri uint32max outp OfpffSendFlowRem [act1, act2]


handshake :: Socket -> IO ()
handshake sock = do
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 0) (OfptHello []))
  frame <- readOfpFrame sock
  putStrLn $ "frame: " ++ show frame
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 1) (OfptFeaturesRequest))
  frame <- readOfpFrame sock
  putStrLn $ "frame: " ++ show frame
  
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 2) (OfptFlowMod flowMod))
  

handleSwitch :: Socket -> SockAddr -> IO ()  
handleSwitch sock caddr =
  forever $ do
    frame <- readOfpFrame sock
    putStrLn $ "frame: " ++ show frame
    case frame of
      (OfpFrame (OfpHeader _ _ _ xid) (OfptEchoRequest dat)) -> do
        sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 xid) (OfptEchoReply dat))
        putStrLn "sent echo reply"
      _ -> putStrLn "unhandled frame"

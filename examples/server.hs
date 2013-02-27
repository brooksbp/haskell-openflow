import Control.Monad
import Control.Concurrent
import Data.Serialize
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Network.OpenFlow
import Network.Info
import System.Environment
import Data.Bits
import GHC.Word

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
    flood csock
    _ <- forkIO $ handleSwitch csock caddr
    return ()

handshake :: Socket -> IO ()
handshake sock = do
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 0) (OfptHello []))
  frame <- readOfpFrame sock
  putStrLn $ "RX: " ++ show frame
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 1) (OfptGetConfigRequest))
  frame <- readOfpFrame sock
  putStrLn $ "RX: " ++ show frame  
  sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 2) (OfptFeaturesRequest))
  frame <- readOfpFrame sock
  putStrLn $ "RX: " ++ show frame  

handleSwitch :: Socket -> SockAddr -> IO ()  
handleSwitch sock caddr =
  forever $ do
    frame <- readOfpFrame sock
    putStrLn $ "RX: " ++ show frame
    case frame of
      (OfpFrame (OfpHeader _ _ _ xid) (OfptEchoRequest dat)) -> do
        let resp = (OfpFrame (OfpHeader 1 0 0 xid) (OfptEchoReply dat))
        sendAll sock $ encode resp
        putStrLn "TX: " ++ show resp
      (OfpFrame (OfpHeader _ _ _ xid) (OfptPacketIn (OfpPacketIn bid len inp reason dat))) ->
        let resp = (OfpFrame (OfpHeader 1 0 0 xid) (OfptPacketOut (OfpPacketOut bid inp [(OfpOutput 6 0)] dat)))
        sendAll sock $ encode resp
        putStrLn "TX: " ++ show resp
      _ -> putStrLn " UNHANDLED"


inp  = 1
outp = 6
ofppController = 0xfffd
ofppNone = 0xffff

pri = 1
uint32max = 0xffffffff
emptyMac = MAC 0x00 0x00 0x00 0x00 0x00 0x00
dlVlan' = 10

wc = [
  OfpfwInPort,
  OfpfwDlVlan,
  OfpfwDlSrc,
  OfpfwDlDst,
  OfpfwDlType,
  OfpfwNwProto,
  OfpfwTpSrc,
  OfpfwTpDst,
  OfpfwNwSrc,
  OfpfwNwDst,
  OfpfwDlVlanPcp,
  OfpfwNwTos
  ]


incrMac :: MAC -> Word64 -> MAC
incrMac (MAC a b c d e f) v =
  MAC (fromIntegral $ (m `shiftR` 40) .&. 0xFF)
      (fromIntegral $ (m `shiftR` 32) .&. 0xFF)
      (fromIntegral $ (m `shiftR` 24) .&. 0xFF)
      (fromIntegral $ (m `shiftR` 16) .&. 0xFF)
      (fromIntegral $ (m `shiftR`  8) .&. 0xFF)
      (fromIntegral $ m .&. 0xFF)
  where
    m = v + fromIntegral ((a `shiftL` 40) .|.
                          (b `shiftL` 32) .|.
                          (c `shiftL` 24) .|.
                          (d `shiftL` 16) .|.
                          (e `shiftL`  8) .|. f)

flood :: Socket -> IO()
flood sock = flood' 4 0
  where
    flood' 0 xid = putStrLn $ "done! xid=" ++ show xid
    flood' n xid = do
      let m  = OfpMatch wc inp emptyMac (incrMac emptyMac n) dlVlan' 0 0 0 0 0 0 0 0
      let fm = OfpFlowMod m 0 OfpfcAdd 65535 65535 pri uint32max outp OfpffSendFlowRem [(OfpOutput outp 0)]
      sendAll sock $ encode (OfpFrame (OfpHeader 1 0 0 xid) (OfptFlowMod fm))
      putStrLn $ "TX: " ++ show n
      flood' (n-1) (xid+1)

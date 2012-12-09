import Data.Serialize
import Network.Socket hiding (recv)
import Network.Socket.ByteString
import Network.OpenFlow
import Network.Info

mac1 = MAC 1 2 3 4 5 6
mac2 = MAC 7 8 9 0 1 2

phyPort1 = OfpPhyPort 0 mac1 "port1" [OfppcPortDown] [OfppsLinkDown] [OfppfFiber] [OfppfFiber] [OfppfFiber] [OfppfFiber]
phyPort2 = OfpPhyPort 1 mac2 "port2" [OfppcNoStp, OfppcNoFlood] [OfppsLinkDown] [OfppfFiber, OfppfAutoneg] [OfppfFiber, OfppfPause] [OfppfFiber] [OfppfFiber]


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

act1 = OfpOutput outp 256
flowMod = OfpFlowMod m 0 OfpfcAdd 360 360 1 100 outp OfpffSendFlowRem [act1]


main :: IO ()
main = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "9999")
  let serveaddr = head addrinfos
  sock <- socket (addrFamily serveaddr) Stream defaultProtocol
  connect sock (addrAddress serveaddr)
  
  bounce sock (OfpFrame (OfpHeader 1 0 0 42) (OfptHello []))
  bounce sock (OfpFrame (OfpHeader 1 0 0 42) (OfptError (OfpetHelloFailed OfphfcIncompatible "incompatible! foo bar_ 43")))
  bounce sock (OfpFrame (OfpHeader 1 0 0 42) (OfptFeaturesReply (OfpSwitchFeatures 0 0 0 [OfpcFlowStats, OfpcQueueStats] [OfpatOutput, OfpatEnqueue] [phyPort1, phyPort2])))
  bounce sock (OfpFrame (OfpHeader 1 0 0 42) (OfptGetConfigReply (OfpSwitchConfig [OfpcFragNormal, OfpcFragMask] 1234)))
  bounce sock (OfpFrame (OfpHeader 1 0 0 42) (OfptFlowMod flowMod))
  bounce sock $ mkFrame (OfptPacketIn (OfpPacketIn 1 1 1 OfprNoMatch []))
  bounce sock $ mkFrame (OfptBarrierRequest)

  sClose sock

  where
    mkFrame msg = (OfpFrame (OfpHeader 1 0 0 42) msg)
    bounce sock' frame = do
      sendAll sock' $ encode frame
      frame <- readOfpFrame sock'
      putStrLn $ "recv: " ++ show frame
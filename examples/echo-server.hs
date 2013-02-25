import Control.Monad
import qualified Data.ByteString as BS
import Network.Socket hiding (recv)
import Network.Socket.ByteString

main :: IO ()
main = do
  addrinfos <- getAddrInfo
               (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
               Nothing (Just "9999")
  let serveaddr = head addrinfos
  sock <- socket (addrFamily serveaddr) Stream defaultProtocol
  bindSocket sock (addrAddress serveaddr)
  listen sock 4
  (conn, caddr) <- accept sock
  talk conn
  sClose conn
  sClose sock
  main
    where
      talk :: Socket -> IO ()
      talk conn = do
        msg <- recv conn 4096
        unless (BS.null msg) $ sendAll conn msg >> talk conn


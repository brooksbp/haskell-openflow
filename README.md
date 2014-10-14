NOTE: `new` branch undergoing development. Use `master` branch if you want stable.

---

## haskell-openflow

[![Build Status](https://travis-ci.org/brooksbp/haskell-openflow.png?branch=master)](https://travis-ci.org/brooksbp/haskell-openflow)

OpenFlow v1.0.0 protocol implementation in Haskell.  https://www.opennetworking.org/images/stories/downloads/sdn-resources/onf-specifications/openflow/openflow-spec-v1.0.0.pdf

Example OpenFlow server:

```haskell
handleSwitch :: Socket -> SockAddr -> IO ()
handleSwitch sock caddr = 
  forever $ do
    frame <- readOfpFrame sock
    case frame of
      (OfpFrame (OfpHeader _ _ _ xid) (OfptEchoRequest dat)) -> do
        let resp = (OfpFrame (OfpHeader 1 0 0 xid) (OfptEchoReply dat))
        sendAll sock $ encode resp
      (OfpFrame (OfpHeader _ _ _ xid) (OfptPacketIn (OfpPacketIn bid len inp reason dat))) -> do
        let resp = (OfpFrame (OfpHeader 1 0 0 xid) (OfptPacketOut (OfpPacketOut bid inp [(OfpOutput ofppAll 0)] dat)))
        sendAll sock $ encode resp
      _ -> putStrLn "unhandled packet"
```

Quickstart:

```bash
$ cabal sandbox init
$ cabal install --only-dependencies --enable-tests
$ cabal build
```

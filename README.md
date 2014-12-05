> NOTE: Undergoing development. See `master` branch for stable.

## haskell-openflow [![Build Status](https://travis-ci.org/brooksbp/haskell-openflow.svg?branch=new)](https://travis-ci.org/brooksbp/haskell-openflow)

Example server:

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

References:

* [OpenFlow Switch Specification 1.3.4](https://www.opennetworking.org/images/stories/downloads/sdn-resources/onf-specifications/openflow/openflow-switch-v1.3.4.pdf) (Mar. 27, 2014)
* [OpenFlow Switch Specification 1.0.0](https://www.opennetworking.org/images/stories/downloads/sdn-resources/onf-specifications/openflow/openflow-spec-v1.0.0.pdf) (Dec. 31, 2009)

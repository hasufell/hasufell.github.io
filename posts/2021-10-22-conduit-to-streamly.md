---
title: From conduit to streamly
author: Julian Ospald
tags: haskell, conduit, streamly, streaming
---

## Yeah

Yes.


```hs
encodeWith :: (MonadCatch m, MonadAsync m, MonadMask m)
           => FormatOptions
           -> SerialT m Event
           -> m ByteString
```

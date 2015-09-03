#Haskell library for the Docker Remote API
##Example usage

```haskell
let daddr = DaemonAddress "192.168.1.2" 2375
newManager tlsManagerSettings >>=
    \mgr -> runReaderT getInfo (mgr,daddr)
```

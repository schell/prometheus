# Prometheus Haskell Client

A simple and modern, type safe, haskell
[Prometheus](http://prometheus.io) client. Specifically there is no
use of unsafe IO or manual ByteString construction from lists of
bytes. Batteries-included web server.

- [Hackage Package](https://hackage.haskell.org/package/prometheus)

## Tasks

- [ ] Implement help docstrings.
- [ ] Implement timestamps on samples.
- [ ] Implement GHC-specific metrics.
- [ ] Encode name and labels on register.
- [ ] Implement ReaderT for GlobalRegistry.
- [ ] Library documentation and example.

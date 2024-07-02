# The `ref-fd` Package  [![Hackage](https://img.shields.io/hackage/v/ref-fd.svg)](https://hackage.haskell.org/package/ref-fd) [![Actions Status: haskell-ci](https://github.com/mainland/ref-fd/actions/workflows/haskell-ci.yml/badge.svg)](https://github.com/mainland/ref-fd/actions?query=workflow%3Ahaskell-ci)

Provides a 'MonadRef' type class that abstracts over the details of manipulating references, allowing one to write code that can operate in either the `ST` monad or the `IO` monad.

# Pure bindings to libclang

A Haskell library for pure C++ code analysis with some light `lens` support

```haskell
gotoCount = lengthOf (allNodes . filtered (\c -> cursorKind c == GotoStmt)) root
```

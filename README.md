# Pure Haskell bindings to [libclang]

A Haskell library for pure C++ code analysis with some light `lens` support

## API examples

### Get count of `goto` statements in program

```haskell
gotoCount = lengthOf (allNodes . filtered (\c -> cursorKind c == GotoStmt)) root
```

### Enumerate all function declarations in `main.cpp`

```haskell
module Main (main) where

import Control.Monad
import Language.C.Clang

main :: IO ()
main = do
    idx <- createIndex
    tu <- parseTranslationUnit idx "main.cpp" ["-I/usr/local/include"]
    let root = translationUnitCursor tu
        children = cursorChildren root
        functionDecls = filter (\c -> cursorKind c == FunctionDecl) children
    forM_ functionDecls $ \f -> print $ cursorSpelling f
```

[libclang]: http://clang.llvm.org/doxygen/group__CINDEX.html

module Clang.Context where

import qualified Data.Map as M
import Data.Monoid
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Types as C
import qualified Language.Haskell.TH as TH

import Clang.Types

clangTypesTable :: M.Map C.TypeSpecifier TH.TypeQ
clangTypesTable = M.fromList
  [ (C.TypeName "CXIndex", [t| CXIndex |])
  , (C.TypeName "CXTranslationUnit", [t| CXTranslationUnit |])
  , (C.TypeName "CXCursor", [t| CXCursor |])
  , (C.TypeName "CXString", [t| CXString |])
  , (C.TypeName "CXSourceRange", [t| CXSourceRange |])
  , (C.TypeName "CXSourceLocation", [t| CXSourceLocation |])
  , (C.TypeName "CXFile", [t| CXFile |])
  , (C.TypeName "CXType", [t| CXType |])
  , (C.TypeName "CXToken", [t| CXToken |])
  ]

clangCtx :: C.Context
clangCtx = C.baseCtx <> C.funCtx <> C.vecCtx <> ctx
  where
    ctx = mempty { C.ctxTypesTable = clangTypesTable }

module Clang.FFI where

import Clang.Context
import Clang.Refs
import Clang.Types
import Data.Foldable
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import System.IO.Unsafe

C.context clangCtx
C.include "stdlib.h"
C.include "clang-c/Index.h"

foreign import ccall "clang_disposeIndex"
  clang_disposeIndex :: Ptr CXIndexImpl -> Finalizer

createIndex :: IO ClangIndex
createIndex = do
  idxp <- [C.exp| CXIndex { clang_createIndex(0, 1) } |]
  ClangIndex <$> root (clang_disposeIndex idxp) idxp

foreign import ccall "clang_disposeTranslationUnit"
  clang_disposeTranslationUnit :: Ptr CXTranslationUnitImpl -> Finalizer

parseTranslationUnit :: ClangIndex -> String -> [ String ] -> IO TranslationUnit
parseTranslationUnit idx path args = do
  tun <- child idx $ \idxp -> 
    withCString path $ \cPath -> do
      cArgs <- VS.fromList <$> traverse newCString args
      tup <- [C.exp| CXTranslationUnit {
        clang_parseTranslationUnit(
          $(CXIndex idxp),
          $(char* cPath),
          $vec-ptr:(const char * const * cArgs), $vec-len:cArgs,
          NULL, 0,
          0)
        } |]
      traverse_ free $ VS.toList cArgs
      return ( clang_disposeTranslationUnit tup, tup )
  return $ TranslationUnitRef tun

translationUnitCursor :: TranslationUnit -> Cursor
translationUnitCursor tu = unsafePerformIO $ do
  cn <- child tu $ \tup -> do
    cp <- [C.block| CXCursor* {
      CXCursor *cp = malloc(sizeof(CXCursor));
      *cp = clang_getTranslationUnitCursor($(CXTranslationUnit tup));
      return cp;
      } |]
    return ( free cp, cp )
  return $ Cursor cn
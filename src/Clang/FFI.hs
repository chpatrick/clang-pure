module Clang.FFI where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Clang.Context
import Clang.Refs
import Clang.Types
import Control.Lens
import Control.Lens.Internal (noEffect)
import Data.Foldable
import Data.IORef
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import System.IO.Unsafe
import Text.RawString.QQ

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

cursorTranslationUnit :: Cursor -> TranslationUnit
cursorTranslationUnit (Cursor c) = parent c

cursorKind :: Cursor -> CInt
cursorKind c = uderef c $ \cp -> [C.exp| int { clang_getCursorKind(*$(CXCursor *cp)) } |]

C.verbatim [r|
  typedef void (*haskell_visitor)(CXCursor*);

  // Traverse children using a haskell_visitor passed in as client_data.
  static enum CXChildVisitResult visit_haskell(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    ((haskell_visitor) client_data)(&cursor);
    return CXChildVisit_Continue;
  };
|]

cursorChildren :: Fold Cursor Cursor
cursorChildren f c = uderef c $ \cp -> do
  fRef <- newIORef noEffect
  let 
    visitChild chp = do
      hchp <- [C.block| CXCursor* {
        CXCursor *heapCopy = malloc(sizeof(CXCursor));
        *heapCopy = *$(CXCursor *chp);
        return heapCopy;
        } |]
      ch <- child (cursorTranslationUnit c) $ \_ -> return ( free hchp, hchp )
      modifyIORef fRef (*> f (Cursor ch))
  [C.exp| void {
    clang_visitChildren(
      *$(CXCursor *cp),
      visit_haskell,
      $fun:(void (*visitChild)(CXCursor*)))
    } |]
  readIORef fRef

cursorSpelling :: Cursor -> ByteString
cursorSpelling c = uderef c $ \cp -> do
  cxsp <- [C.block| CXString* {
    CXString *cxsp = malloc(sizeof(CXString));
    *cxsp = clang_getCursorSpelling(*$(CXCursor *cp));
    return cxsp;
    } |]
  processCXString cxsp

-- | Marshal a CXString pointer to a ByteString and free it.
processCXString :: Ptr CXString -> IO ByteString
processCXString cxsp = do
  csp <- [C.exp| const char * { clang_getCString(*$(CXString *cxsp)) } |]
  s <- BS.packCString csp
  [C.exp| void { clang_disposeString(*$(CXString *cxsp)) } |]
  free cxsp
  return s
module Clang.FFI where

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Clang.Context
import Clang.Refs
import Clang.Types
import Control.Exception
import Control.Lens
import Control.Lens.Internal (noEffect)
import Control.Monad
import Data.Foldable
import Data.IORef
import qualified Data.Vector.Storable as VS
import Foreign
import Foreign.C
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Unsafe as CU
import System.IO.Unsafe
import qualified Text.RawString.QQ as R

C.context clangCtx
C.include "stdlib.h"
C.include "clang-c/Index.h"

C.verbatim [R.r|
  typedef void (*haskell_visitor)(CXCursor*);

  // Traverse children using a haskell_visitor passed in as client_data.
  // The visitor gets a copy of the cursor on the heap and is responsible
  // for freeing it.
  static enum CXChildVisitResult visit_haskell(CXCursor cursor, CXCursor parent, CXClientData client_data) {
    CXCursor *heapCursor = malloc(sizeof(CXCursor));
    *heapCursor = cursor;
    ((haskell_visitor) client_data)(heapCursor);
    return CXChildVisit_Continue;
  };

  // Macro that makes a copy of the result of a given expression on the heap and returns it.
  #define ALLOC(__ALLOC_TYPE__, __ALLOC_EXPR__) {\
    __ALLOC_TYPE__ *__alloc_ptr__ = malloc(sizeof(__ALLOC_TYPE__));\
    *__alloc_ptr__ = (__ALLOC_EXPR__);\
    return __alloc_ptr__;\
  }
|]

foreign import ccall "clang_disposeIndex"
  clang_disposeIndex :: Ptr CXIndexImpl -> Finalizer

createIndex :: IO ClangIndex
createIndex = do
  idxp <- [CU.exp| CXIndex { clang_createIndex(0, 1) } |]
  ClangIndex <$> root (clang_disposeIndex idxp) idxp

foreign import ccall "clang_disposeTranslationUnit"
  clang_disposeTranslationUnit :: Ptr CXTranslationUnitImpl -> Finalizer

newtype ClangException = ClangException CInt
  deriving Show

instance Exception ClangException

parseTranslationUnit :: ClangIndex -> String -> [ String ] -> IO TranslationUnit
parseTranslationUnit idx path args = do
  tun <- child idx $ \idxp -> 
    withCString path $ \cPath -> do
      cArgs <- VS.fromList <$> traverse newCString args
      ( tup, res ) <- C.withPtr $ \tupp -> [CU.exp| int {
        clang_parseTranslationUnit2(
          $(CXIndex idxp),
          $(char* cPath),
          $vec-ptr:(const char * const * cArgs), $vec-len:cArgs,
          NULL, 0,
          0,
          $(CXTranslationUnit *tupp)
          )
        } |]
      traverse_ free $ VS.toList cArgs
      when (res /= 0) $
        throwIO (ClangException res)
      return ( clang_disposeTranslationUnit tup, tup )
  return $ TranslationUnitRef tun

translationUnitCursor :: TranslationUnit -> Cursor
translationUnitCursor tu = unsafePerformIO $ do
  cn <- child tu $ \tup -> do
    cp <- [CU.block| CXCursor* { ALLOC(CXCursor,
      clang_getTranslationUnitCursor($(CXTranslationUnit tup))
      )} |]
    return ( free cp, cp )
  return $ Cursor cn

cursorTranslationUnit :: Cursor -> TranslationUnit
cursorTranslationUnit (Cursor c) = parent c

cursorKind :: Cursor -> CInt
cursorKind c = uderef c $ \cp ->
  [CU.exp| int { clang_getCursorKind(*$(CXCursor *cp)) } |]

cursorChildren :: Fold Cursor Cursor
cursorChildren f c = uderef c $ \cp -> do
  fRef <- newIORef noEffect
  let 
    visitChild chp = do
      ch <- child (cursorTranslationUnit c) $ \_ ->
        return ( free chp, chp )
      modifyIORef' fRef (*> f (Cursor ch))
  [C.exp| void {
    clang_visitChildren(
      *$(CXCursor *cp),
      visit_haskell,
      $fun:(void (*visitChild)(CXCursor*)))
    } |]
  readIORef fRef

withCXString :: (Ptr CXString -> IO ()) -> IO ByteString
withCXString f = allocaBytes cxStringSize $ \cxsp -> do
  f cxsp
  cs <- [CU.exp| const char * { clang_getCString(*$(CXString *cxsp)) } |]
  s <- BS.packCString cs
  [CU.exp| void { clang_disposeString(*$(CXString *cxsp)) } |]
  return s

cursorSpelling :: Cursor -> ByteString
cursorSpelling c = uderef c $ \cp -> withCXString $ \cxsp ->
  [CU.block| void {
    *$(CXString *cxsp) = clang_getCursorSpelling(*$(CXCursor *cp));
    } |]

cursorExtent :: Cursor -> Maybe SourceRange
cursorExtent c = uderef c $ \cp -> do
  srp <- [CU.block| CXSourceRange* {
    CXSourceRange sr = clang_getCursorExtent(*$(CXCursor *cp));
    if (clang_Range_isNull(sr)) {
      return NULL;
    }

    ALLOC(CXSourceRange, sr);
    } |]
  if srp == nullPtr
    then return Nothing
    else do
      srn <- child (cursorTranslationUnit c) $ \_ ->
        return ( free srp, srp )
      return $ Just $ SourceRange srn

rangeStart, rangeEnd :: SourceRange -> SourceLocation
rangeStart sr = uderef sr $ \srp -> do
  slp <- [CU.block| CXSourceLocation* { ALLOC(CXSourceLocation,
    clang_getRangeStart(*$(CXSourceRange *srp))
    )} |]
  sln <- child (parent sr) $ \_ ->
    return ( free slp, slp )
  return $ SourceLocation sln

rangeEnd sr = uderef sr $ \srp -> do
  slp <- [CU.block| CXSourceLocation* { ALLOC(CXSourceLocation,
    clang_getRangeEnd(*$(CXSourceRange *srp))
    )} |]
  sln <- child (parent sr) $ \_ ->
    return ( free slp, slp )
  return $ SourceLocation sln

spellingLocation :: SourceLocation -> Location
spellingLocation sl = uderef sl $ \slp -> do
  ( f, l, c, o ) <- C.withPtrs_ $ \( fp, lp, cp, offp ) ->
    [CU.exp| void {
      clang_getSpellingLocation(
        *$(CXSourceLocation *slp),
        $(CXFile *fp),
        $(unsigned int *lp),
        $(unsigned int *cp),
        $(unsigned int *offp))
      } |]
  fn <- child (parent sl) $ \_ -> return ( return (), f )
  return $ Location
    { file = File fn
    , line = fromIntegral l
    , column = fromIntegral c
    , offset = fromIntegral o
    }

getFile :: TranslationUnit -> FilePath -> Maybe File
getFile tu p = uderef tu $ \tup -> withCString p $ \fn -> do
  fp <- [CU.exp| CXFile {
    clang_getFile($(CXTranslationUnit tup), $(const char *fn))
    } |]
  if fp == nullPtr
    then return Nothing
    else (Just . File) <$> child tu (\_ -> return ( return (), fp ))

fileName :: File -> ByteString
fileName f = uderef f $ \fp -> withCXString $ \cxsp ->
  [CU.block| void {
    *$(CXString *cxsp) = clang_getFileName($(CXFile fp));
    } |]

instance Eq Cursor where
  (==) = defaultEq $ \lp rp ->
    [CU.exp| int { clang_equalCursors(*$(CXCursor *lp), *$(CXCursor *rp)) } |]

instance Eq SourceRange where
  (==) = defaultEq $ \lp rp ->
    [CU.exp| int { clang_equalRanges(*$(CXSourceRange *lp), *$(CXSourceRange *rp)) } |]

instance Eq SourceLocation where
  (==) = defaultEq $ \lp rp ->
    [CU.exp| int { clang_equalLocations(*$(CXSourceLocation *lp), *$(CXSourceLocation *rp)) } |]

defaultEq :: (Ref r, RefType r ~ a) => (Ptr a -> Ptr a -> IO CInt) -> r -> r -> Bool
defaultEq ne l r
  = node l == node r || uderef2 l r ne /= 0

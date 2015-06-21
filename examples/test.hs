{-# LANGUAGE PatternSynonyms, ViewPatterns #-}

import Clang
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Tree

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "examples/test.c" []
  let
    Just srcFile = getFile tu "examples/test.c"
    loc
      = fmap (file . spellingLocation . rangeStart) . cursorExtent
    ast
      = flip unfoldTree (translationUnitCursor tu) $ \n ->
        ( show $ cursorKind n
        , toListOf (droppingWhile (\c -> loc c /= Just srcFile) cursorChildren) n
        )
  putStrLn (drawTree ast)

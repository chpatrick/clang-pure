{-# LANGUAGE PatternSynonyms, ViewPatterns, RankNTypes, LambdaCase #-}

import Clang
import Data.Function
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Tree

-- generalized universeOf
univ :: Fold a a -> Fold a a
univ fld f = fld $ \x -> f x *> univ fld f x

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "examples/test.c" []
  let
    Just srcFile = getFile tu "examples/test.c"
    inFile c = case cursorExtent c of
      Nothing -> False
      Just ex -> (file $ spellingLocation $ rangeStart ex) == srcFile
    root = translationUnitCursor tu
    allNodes :: Fold Cursor Cursor
    allNodes = univ (cursorChildren . filtered inFile)
    gotos = lengthOf (allNodes . filtered (\c -> cursorKind c == GotoStmt)) root
    literalValues = root ^.. allNodes . filtered (\c -> cursorKind c == IntegerLiteral)
                      . folding (firstOf (folding cursorExtent . to tokenize . folding tokenSetTokens))
                      . to tokenSpelling
  print literalValues
  putStrLn $ if gotos == 0
    then "No gotos!"
    else show gotos ++ " goto(s)! You should feel bad!"

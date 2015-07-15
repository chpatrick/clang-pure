{-
Copyright 2014 Google Inc. All Rights Reserved.

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
-}

{-# LANGUAGE PatternSynonyms, ViewPatterns, RankNTypes, LambdaCase #-}

import Clang
import Data.Function
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Tree

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "examples/test.c" []
  let
    inFile c = ((isFromMainFile . rangeStart) <$> cursorExtent c) == Just True
    root = translationUnitCursor tu
    allNodes :: Fold Cursor Cursor
    allNodes = cosmosOf (cursorChildren . filtered inFile)
    gotos = lengthOf (allNodes . filtered (\c -> cursorKind c == GotoStmt)) root
    literalValues = root ^.. allNodes . filtered (\c -> cursorKind c == IntegerLiteral)
                      . folding (firstOf (folding cursorExtent . to tokenize . folding tokenSetTokens))
                      . to tokenSpelling
  print literalValues
  putStrLn $ if gotos == 0
    then "No gotos!"
    else show gotos ++ " goto(s)! You should feel bad!"

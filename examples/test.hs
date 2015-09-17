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

{-# LANGUAGE PatternSynonyms, ViewPatterns, RankNTypes, LambdaCase, TupleSections #-}

import Clang
import Data.Foldable
import Data.Function
import Control.Lens
import qualified Data.ByteString.Char8 as BS
import Data.Monoid
import Data.Tree
import Data.Tree

main = do
  idx <- createIndex
  tu <- parseTranslationUnit idx "examples/test.c" []
  let
    root = translationUnitCursor tu
    funDecs =
      root ^..
        cosmosOf cursorChildren
        . filtered (\c -> cursorKind c == FunctionDecl)
        . folding (\c -> ( cursorSpelling c, ) <$> (typeSpelling <$> cursorType c))
  for_ funDecs $ \(f, t) -> putStrLn $ BS.unpack f ++ " :: " ++ BS.unpack t

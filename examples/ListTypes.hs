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

{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Clang
import qualified Language.C.Clang.Cursor as UT
import           Language.C.Clang.Cursor.Typed
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : clangArgs -> do
      idx <- createIndex
      tu <- parseTranslationUnit idx path clangArgs
      let funDecs =
              cosmosOnOf cursorChildrenF UT.cursorChildrenF         -- fold over cursors recursively
            . folding (matchKind @'FunctionDecl)                    -- find only FunctionDecls...
            . filtered (isFromMainFile . rangeStart . cursorExtent) -- ...that are actually in the given file
            . to (\funDec -> cursorSpelling funDec <> " :: " <> typeSpelling (cursorType funDec))
      BS.putStrLn $ BS.unlines (translationUnitCursor tu ^.. funDecs)

    _ -> putStrLn "usage: list-fun-types path [clang opts]"
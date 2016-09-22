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

{-|
Module      :  Language.C.Clang.Cursor
Copyright   :  (C) 2016 Patrick Chilton

If you know what `CursorKind`s you want to operate on,
consider using "Language.C.Clang.Cursor.Typed" instead.
-}
module Language.C.Clang.Cursor
  ( Cursor()
  , translationUnitCursor
  , cursorTranslationUnit
  , cursorChildrenF
  , cursorChildren
  , cursorSpelling
  , cursorExtent
  , cursorUSR
  , cursorReferenced
  , cursorType
  , cursorKind
  , CursorKind(..)
  )
where

import Language.C.Clang.Internal.FFI
import Language.C.Clang.Internal.Types

import Lens.Micro

cursorChildren :: Cursor -> [ Cursor ]
cursorChildren = toListOf cursorChildrenF

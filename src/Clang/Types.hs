module Clang.Types where

import Foreign

import Clang.Refs

data CXIndexImpl
type CXIndex = Ptr CXIndexImpl
type instance RefType ClangIndex = CXIndexImpl
newtype ClangIndex = ClangIndex (Root CXIndexImpl) deriving Ref

data CXTranslationUnitImpl
type CXTranslationUnit = Ptr CXTranslationUnitImpl
type instance RefType TranslationUnit = CXTranslationUnitImpl
newtype TranslationUnit = TranslationUnitRef (Child ClangIndex CXTranslationUnitImpl) deriving Ref

data CXCursor
type instance RefType Cursor = CXCursor
newtype Cursor = Cursor (Child TranslationUnit CXCursor) deriving Ref

data CXString
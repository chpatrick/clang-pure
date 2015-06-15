module Clang.Types where

import Foreign

import Clang.Refs

data CXIndexImpl
type CXIndex = Ptr CXIndexImpl
type instance RefType ClangIndex = CXIndexImpl
type instance ParentType ClangIndex = ()
newtype ClangIndex = ClangIndex (Root CXIndexImpl) deriving Ref

data CXTranslationUnitImpl
type CXTranslationUnit = Ptr CXTranslationUnitImpl
type instance RefType TranslationUnit = CXTranslationUnitImpl
type instance ParentType TranslationUnit = ClangIndex
newtype TranslationUnit = TranslationUnitRef (Child ClangIndex CXTranslationUnitImpl) deriving Ref

data CXCursor
type instance RefType Cursor = CXCursor
type instance ParentType Cursor = TranslationUnit
newtype Cursor = Cursor (Child TranslationUnit CXCursor) deriving Ref

data CXSourceRange
type instance RefType SourceRange = CXSourceRange
type instance ParentType SourceRange = TranslationUnit
newtype SourceRange
  = SourceRange (Child TranslationUnit CXSourceRange) deriving Ref

data CXSourceLocation
type instance RefType SourceLocation = CXSourceLocation
type instance ParentType SourceLocation = TranslationUnit
newtype SourceLocation
  = SourceLocation (Child TranslationUnit CXSourceLocation) deriving Ref

data CXFileImpl
type CXFile = Ptr CXFileImpl
type instance RefType File = CXFileImpl
type instance ParentType File = TranslationUnit
newtype File
  = File (Child TranslationUnit CXFileImpl) deriving Ref

data CXString

data Location = Location
  { file :: File
  , line :: Word
  , column :: Word
  , offset :: Word
  }

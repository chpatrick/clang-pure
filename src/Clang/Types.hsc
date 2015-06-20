module Clang.Types where

import Foreign

import Clang.Refs

#include "clang-c/Index.h"

-- The default instance of Eq for Clang types does structural equality,
-- i.e. it checks whether they represent the same AST object.
-- This is inconsistent with an Ord instance, so if you need ordering
-- then wrap them up in this type.
newtype Ordered a = Ordered { getOrdered :: a }

instance Ref a => Eq (Ordered a) where
  Ordered x == Ordered y = node x == node y

instance Ref a => Ord (Ordered a) where
  Ordered x `compare` Ordered y = node x `compare` node y

data CXIndexImpl
type CXIndex = Ptr CXIndexImpl
type instance RefType ClangIndex = CXIndexImpl
type instance ParentType ClangIndex = ()
newtype ClangIndex = ClangIndex (Root CXIndexImpl)
  deriving (Eq, Ref)

data CXTranslationUnitImpl
type CXTranslationUnit = Ptr CXTranslationUnitImpl
type instance RefType TranslationUnit = CXTranslationUnitImpl
type instance ParentType TranslationUnit = ClangIndex
newtype TranslationUnit = TranslationUnitRef (Child ClangIndex CXTranslationUnitImpl)
  deriving (Eq, Ref)

data CXCursor
type instance RefType Cursor = CXCursor
type instance ParentType Cursor = TranslationUnit
newtype Cursor = Cursor (Child TranslationUnit CXCursor)
  deriving Ref

data CXSourceRange
type instance RefType SourceRange = CXSourceRange
type instance ParentType SourceRange = TranslationUnit
newtype SourceRange
  = SourceRange (Child TranslationUnit CXSourceRange)
  deriving Ref

data CXSourceLocation
type instance RefType SourceLocation = CXSourceLocation
type instance ParentType SourceLocation = TranslationUnit
newtype SourceLocation
  = SourceLocation (Child TranslationUnit CXSourceLocation)
  deriving Ref

data CXFileImpl
type CXFile = Ptr CXFileImpl
type instance RefType File = CXFileImpl
type instance ParentType File = TranslationUnit
newtype File
  = File (Child TranslationUnit CXFileImpl)
  deriving (Ref, Eq)

data CXString
cxStringSize :: Int
cxStringSize = #size CXString

data Location = Location
  { file :: File
  , line :: Word
  , column :: Word
  , offset :: Word
  }
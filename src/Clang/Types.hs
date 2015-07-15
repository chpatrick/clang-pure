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

module Clang.Types where

import Foreign

import Clang.Refs

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

data Location = Location
  { file :: File
  , line :: Word
  , column :: Word
  , offset :: Word
  }

data CursorKind
  = UnexposedDecl
  | StructDecl
  | UnionDecl
  | ClassDecl
  | EnumDecl
  | FieldDecl
  | EnumConstantDecl
  | FunctionDecl
  | VarDecl
  | ParmDecl
  | ObjCInterfaceDecl
  | ObjCCategoryDecl
  | ObjCProtocolDecl
  | ObjCPropertyDecl
  | ObjCIvarDecl
  | ObjCInstanceMethodDecl
  | ObjCClassMethodDecl
  | ObjCImplementationDecl
  | ObjCCategoryImplDecl
  | TypedefDecl
  | CXXMethod
  | Namespace
  | LinkageSpec
  | Constructor
  | Destructor
  | ConversionFunction
  | TemplateTypeParameter
  | NonTypeTemplateParameter
  | TemplateTemplateParameter
  | FunctionTemplate
  | ClassTemplate
  | ClassTemplatePartialSpecialization
  | NamespaceAlias
  | UsingDirective
  | UsingDeclaration
  | TypeAliasDecl
  | ObjCSynthesizeDecl
  | ObjCDynamicDecl
  | CXXAccessSpecifier
  | FirstDecl
  | LastDecl
  | FirstRef
  | ObjCSuperClassRef
  | ObjCProtocolRef
  | ObjCClassRef
  | TypeRef
  | CXXBaseSpecifier
  | TemplateRef
  | NamespaceRef
  | MemberRef
  | LabelRef
  | OverloadedDeclRef
  | VariableRef
  | LastRef
  | FirstInvalid
  | InvalidFile
  | NoDeclFound
  | NotImplemented
  | InvalidCode
  | LastInvalid
  | FirstExpr
  | UnexposedExpr
  | DeclRefExpr
  | MemberRefExpr
  | CallExpr
  | ObjCMessageExpr
  | BlockExpr
  | IntegerLiteral
  | FloatingLiteral
  | ImaginaryLiteral
  | StringLiteral
  | CharacterLiteral
  | ParenExpr
  | UnaryOperator
  | ArraySubscriptExpr
  | BinaryOperator
  | CompoundAssignOperator
  | ConditionalOperator
  | CStyleCastExpr
  | CompoundLiteralExpr
  | InitListExpr
  | AddrLabelExpr
  | StmtExpr
  | GenericSelectionExpr
  | GNUNullExpr
  | CXXStaticCastExpr
  | CXXDynamicCastExpr
  | CXXReinterpretCastExpr
  | CXXConstCastExpr
  | CXXFunctionalCastExpr
  | CXXTypeidExpr
  | CXXBoolLiteralExpr
  | CXXNullPtrLiteralExpr
  | CXXThisExpr
  | CXXThrowExpr
  | CXXNewExpr
  | CXXDeleteExpr
  | UnaryExpr
  | ObjCStringLiteral
  | ObjCEncodeExpr
  | ObjCSelectorExpr
  | ObjCProtocolExpr
  | ObjCBridgedCastExpr
  | PackExpansionExpr
  | SizeOfPackExpr
  | LambdaExpr
  | ObjCBoolLiteralExpr
  | ObjCSelfExpr
  | LastExpr
  | FirstStmt
  | UnexposedStmt
  | LabelStmt
  | CompoundStmt
  | CaseStmt
  | DefaultStmt
  | IfStmt
  | SwitchStmt
  | WhileStmt
  | DoStmt
  | ForStmt
  | GotoStmt
  | IndirectGotoStmt
  | ContinueStmt
  | BreakStmt
  | ReturnStmt
  | GCCAsmStmt
  | AsmStmt
  | ObjCAtTryStmt
  | ObjCAtCatchStmt
  | ObjCAtFinallyStmt
  | ObjCAtThrowStmt
  | ObjCAtSynchronizedStmt
  | ObjCAutoreleasePoolStmt
  | ObjCForCollectionStmt
  | CXXCatchStmt
  | CXXTryStmt
  | CXXForRangeStmt
  | SEHTryStmt
  | SEHExceptStmt
  | SEHFinallyStmt
  | MSAsmStmt
  | NullStmt
  | DeclStmt
  | OMPParallelDirective
  | OMPSimdDirective
  | OMPForDirective
  | OMPSectionsDirective
  | OMPSectionDirective
  | OMPSingleDirective
  | OMPParallelForDirective
  | OMPParallelSectionsDirective
  | OMPTaskDirective
  | OMPMasterDirective
  | OMPCriticalDirective
  | OMPTaskyieldDirective
  | OMPBarrierDirective
  | OMPTaskwaitDirective
  | OMPFlushDirective
  | SEHLeaveStmt
  | LastStmt
  | TranslationUnit
  | FirstAttr
  | UnexposedAttr
  | IBActionAttr
  | IBOutletAttr
  | IBOutletCollectionAttr
  | CXXFinalAttr
  | CXXOverrideAttr
  | AnnotateAttr
  | AsmLabelAttr
  | PackedAttr
  | PureAttr
  | ConstAttr
  | NoDuplicateAttr
  | CUDAConstantAttr
  | CUDADeviceAttr
  | CUDAGlobalAttr
  | CUDAHostAttr
  | LastAttr
  | PreprocessingDirective
  | MacroDefinition
  | MacroExpansion
  | MacroInstantiation
  | InclusionDirective
  | FirstPreprocessing
  | LastPreprocessing
  | ModuleImportDecl
  | FirstExtraDecl
  | LastExtraDecl
  deriving (Eq, Ord, Show)

data TypeKind
  = Invalid
  | Unexposed
  | Void
  | Bool
  | Char_U
  | UChar
  | Char16
  | Char32
  | UShort
  | UInt
  | ULong
  | ULongLong
  | UInt128
  | Char_S
  | SChar
  | WChar
  | Short
  | Int
  | Long
  | LongLong
  | Int128
  | Float
  | Double
  | LongDouble
  | NullPtr
  | Overload
  | Dependent
  | ObjCId
  | ObjCClass
  | ObjCSel
  | FirstBuiltin
  | LastBuiltin
  | Complex
  | Pointer
  | BlockPointer
  | LValueReference
  | RValueReference
  | Record
  | Enum
  | Typedef
  | ObjCInterface
  | ObjCObjectPointer
  | FunctionNoProto
  | FunctionProto
  | ConstantArray
  | Vector
  | IncompleteArray
  | VariableArray
  | DependentSizedArray
  | MemberPointer
    deriving (Eq, Ord, Show)

data CXType
type instance RefType Type = CXType
type instance ParentType Type = TranslationUnit
newtype Type = Type (Child TranslationUnit CXType)
  deriving Ref

data CXToken
data TokenSet = TokenSet 
  { tokenSetRef :: Child TranslationUnit CXToken
  , tokenSetSize :: Int
  }

type instance RefType Token = CXToken
type instance ParentType Token = TranslationUnit
data Token = Token TokenSet Int
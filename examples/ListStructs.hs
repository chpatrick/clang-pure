{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

import           Language.C.Clang
import           Language.C.Clang.Cursor.Typed
import           Control.Lens
import qualified Data.ByteString.Char8 as BS
import           Data.Monoid
import           Data.Word
import           System.Environment

data CField = CField
  { cFieldName :: BS.ByteString
  , cFieldOffset :: Word64
  , cFieldType :: Type
  } deriving (Show)

data CStruct = CStruct
  { cStructName :: BS.ByteString
  , cStructFields :: [ CField ]
  } deriving (Show)

main :: IO ()
main = do
  args <- getArgs
  case args of
    path : clangArgs -> do
      idx <- createIndex
      tu <- parseTranslationUnit idx path clangArgs

      let toCField fieldDecC = do
            offset <- offsetOfField fieldDecC
            return $ CField
              { cFieldName = cursorSpelling fieldDecC
              , cFieldOffset = offset
              , cFieldType = cursorType fieldDecC
              }

      let toCStruct structDecC = do
            let fieldDecs =
                  structDecC
                    ^.. cursorDescendantsF
                      . folding (matchKind @'FieldDecl)
            cFields <- traverse toCField fieldDecs
            return $ CStruct (cursorSpelling structDecC) cFields

      let cStructs =
            translationUnitCursor tu
              ^.. cursorDescendantsF
                . folding (matchKind @'StructDecl)
                . to toCStruct
                . _Right

      print cStructs

    _ -> putStrLn "usage: list-structs path [clang opts]"
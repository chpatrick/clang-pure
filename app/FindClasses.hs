{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import qualified Data.ByteString as BS
import           Data.List
import qualified Data.HashMap.Strict as HMS
import           Data.Hashable
import           Language.C.Clang
import           Language.C.Clang.Cursor
import           Control.Lens
import           Data.Traversable
import           Data.Maybe
import           GHC.Generics (Generic)
import           System.Environment

deriving instance Generic CursorKind
instance Hashable CursorKind

classes :: [ ( String, Cursor -> Bool ) ]
classes =
  [ ( "HasType", isJust . cursorType )
  , ( "HasChildren", notNullOf cursorChildrenF )
  , ( "HasExtent", isJust . cursorExtent )
  , ( "HasSpelling", not . BS.null . cursorSpelling )
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "usage: find-classes file1 [file2] [fileN...]"

    paths -> do
      idx <- createIndex

      pathClassResults <- for paths $ \path -> do
        tu <- parseTranslationUnit idx path []
        let root = translationUnitCursor tu
        return $ HMS.fromList
          [ ( className, findClass predicate root )
          | ( className, predicate ) <- classes
          ]

      let classResults = foldl1' (HMS.unionWith (HMS.unionWith (&&))) pathClassResults

      let allInstances =
            intercalate "\n"
              [ instances
              | ( className, kindResults ) <- HMS.toList classResults
              , let sortedNames = sort [ show kind | ( kind, matches ) <- HMS.toList kindResults, matches ]
              , let instances = unlines $ map (\kindName -> "instance " ++ className ++ " '" ++ kindName) sortedNames
              ]

      putStrLn allInstances

findClass :: (Cursor -> Bool) -> Cursor -> HMS.HashMap CursorKind Bool
findClass predicate root = HMS.fromListWith (&&) kindResults
  where
    kindResults = root ^.. cosmosOf cursorChildrenF . to (\c -> ( cursorKind c, predicate c ) )

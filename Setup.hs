{-|
Module      : Main
Description : Setup script for clang-pure build
Copyright   : (C) Richard Cook, 2016
Licence     : Apache 2.0
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This setup script adds a configuration hook to the build process to discover
the LLVM library and include paths from the standard system-wide installation
location or from an alternative location if overridden with the
@CLANG_PURE_LLVM_DIR@ environment variable.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Exception
import Control.Monad
#ifndef mingw32_HOST_OS
import Data.Maybe
#endif
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Directory
import System.Environment
import System.FilePath
import System.IO.Error

data SetupException = SetupException String deriving Show

instance Exception SetupException

data LLVMPathInfo = LLVMPathInfo
    { llvmLibraryDir :: FilePath
    , llvmIncludeDir :: FilePath
    }

llvmDirEnvVarName :: String
llvmDirEnvVarName = "CLANG_PURE_LLVM_DIR"

libraryExtension :: String
#ifdef mingw32_HOST_OS
libraryExtension = ".dll"
#elif darwin_HOST_OS
libraryExtension = ".dylib"
#else
libraryExtension = ".so"
#endif

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
    allNames <- getDirectoryContents dir `catch` (\e -> if isPermissionError e then return [] else throw e)
    let names = filter (`notElem` [".", ".."]) allNames
    paths <- forM names $ \name -> do
        let path = dir </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory
           then getRecursiveContents path
           else return [path]
    return (concat paths)

findFileRecursive :: FilePath -> String -> IO FilePath
findFileRecursive dir fileName = do
    paths <- getRecursiveContents dir
    let matches = filter (\p -> takeFileName p == fileName) paths
    case matches of
         [] -> throw $ SetupException ("Could not find " ++ fileName ++ " under " ++ dir)
         firstMatch : _ -> return firstMatch

getLLVMRootDir :: IO FilePath
#ifdef mingw32_HOST_OS
getLLVMRootDir = do
    maybeOverride <- lookupEnv llvmDirEnvVarName
    case maybeOverride of
        Nothing -> do
            programFilesDir <- getEnv "PROGRAMFILES"
            return $ programFilesDir </> "LLVM"
        Just llvmRootDir -> return llvmRootDir
#else
getLLVMRootDir = do
    maybeOverride <- lookupEnv llvmDirEnvVarName
    let llvmRootDir = fromMaybe "/usr" maybeOverride
    return llvmRootDir
#endif

getLLVMPathInfo :: IO LLVMPathInfo
getLLVMPathInfo = do
    llvmRootDir <- getLLVMRootDir
    libclangPath <- findFileRecursive llvmRootDir ("libclang" -<.> libraryExtension)
    let libraryDir = takeDirectory libclangPath
    indexHeaderPath <- findFileRecursive llvmRootDir "Index.h"
    let includeDir = takeDirectory $ takeDirectory indexHeaderPath
    return $ LLVMPathInfo libraryDir includeDir

clangPureConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
clangPureConfHook (d, bi) flags = do
    localBuildInfo <- confHook simpleUserHooks (d, bi) flags
    let pd = localPkgDescr localBuildInfo
        Just lib = library pd
        lbi = libBuildInfo lib

    LLVMPathInfo{..} <- getLLVMPathInfo

    return localBuildInfo {
        localPkgDescr = pd {
            library = Just $ lib {
                libBuildInfo = lbi
                { includeDirs = llvmIncludeDir : includeDirs lbi
                , extraLibDirs = llvmLibraryDir : extraLibDirs lbi
                }
            }
        }
    }

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = clangPureConfHook }

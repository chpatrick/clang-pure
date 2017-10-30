{-|
Module      : Main
Description : Setup script for clang-pure build
Copyright   : (C) Richard Cook, Patrick Chilton 2016
Licence     : Apache 2.0
Maintainer  : rcook@rcook.org
Stability   : experimental
Portability : portable

This setup script adds a configuration hook to the build process to discover
the LLVM library and include paths from the standard system-wide installation
location or from an alternative location if overridden with both the
@CLANG_PURE_LLVM_LIB_DIR@ and @CLANG_PURE_LLVM_INCLUDE_DIR@ environment variables.
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Main (main) where

import Control.Exception
import Control.Monad
import Distribution.PackageDescription
import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Setup
import System.Environment
import System.IO.Error
import System.Process
import Data.Version
import Text.ParserCombinators.ReadP

data SetupException = SetupException String deriving Show

instance Exception SetupException

data LLVMPathInfo = LLVMPathInfo
  { llvmLibraryDir :: FilePath
  , llvmIncludeDir :: FilePath
  } deriving (Eq, Ord, Show)

llvmLibDirEnvVarName :: String
llvmLibDirEnvVarName = "CLANG_PURE_LLVM_LIB_DIR"

llvmIncludeDirEnvVarName :: String
llvmIncludeDirEnvVarName = "CLANG_PURE_LLVM_INCLUDE_DIR"

minVersion :: Version
minVersion = makeVersion [ 3, 8, 0 ]

findLLVMConfigPaths :: IO LLVMPathInfo
findLLVMConfigPaths = do
  let llvmConfigCandidates =
        "llvm-config" :
          [ "llvm-config-" ++ show major ++ "." ++ show minor
          | major <- [9,8..3 :: Int]
          , minor <- [9,8..0 :: Int]
          ]
  let tryCandidates [] = throwIO $ SetupException $ "Could not find llvm-config with minimum version " ++ showVersion minVersion ++ "."
      tryCandidates (llvmConfig : candidates) = do
        llvmConfigResult <- tryJust
          (guard . isDoesNotExistError)
          (readProcess llvmConfig ["--version", "--libdir", "--includedir"] "")
        case llvmConfigResult of
          Left _ -> tryCandidates candidates
          Right llvmConfigOutput -> case lines llvmConfigOutput of
            [ versionString, libraryDir, includeDir ] ->
              case readP_to_S (parseVersion <* eof) versionString of
                [ ( version, _ ) ]
                  | version >= minVersion -> return $ LLVMPathInfo libraryDir includeDir
                  | otherwise -> tryCandidates candidates
                _ -> throwIO $ SetupException "Couldn't parse llvm-config version string."
            _ -> throwIO $ SetupException "Unexpected llvm-config output."
  tryCandidates llvmConfigCandidates

getLLVMPathInfo :: IO LLVMPathInfo
getLLVMPathInfo = do
  m'llvmLibDirEnvVar <- lookupEnv llvmLibDirEnvVarName
  m'llvmIncludeDirEnvVar <- lookupEnv llvmIncludeDirEnvVarName
  case (m'llvmLibDirEnvVar, m'llvmIncludeDirEnvVar) of
    ( Just libraryDir, Just includeDir ) -> return $ LLVMPathInfo libraryDir includeDir
    _ -> findLLVMConfigPaths

clangPureConfHook :: (GenericPackageDescription, HookedBuildInfo) -> ConfigFlags -> IO LocalBuildInfo
clangPureConfHook (d, bi) flags = do
  localBuildInfo <- confHook simpleUserHooks (d, bi) flags
  let pd = localPkgDescr localBuildInfo
  let Just lib = library pd
  let lbi = libBuildInfo lib

  LLVMPathInfo{..} <- getLLVMPathInfo

  return localBuildInfo {
    localPkgDescr = pd {
      library = Just $ lib {
        libBuildInfo = lbi
        { includeDirs = llvmIncludeDir : includeDirs lbi
        , extraLibDirs = llvmLibraryDir : extraLibDirs lbi
        , cSources = -- define the generated c-sources here so that they don't get picked up by sdist
#if defined(mingw32_HOST_OS) && !(MIN_VERSION_GLASGOW_HASKELL(8, 0, 2, 0))
            ["srcLanguageCClangInternalFFI.c"] -- work around a bug in hsc2hs
#else
            ["src/Language/C/Clang/Internal/FFI.c"]
#endif
        }
      }
    }
  }

main :: IO ()
main = defaultMainWithHooks simpleUserHooks { confHook = clangPureConfHook }

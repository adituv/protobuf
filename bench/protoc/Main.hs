module Main(main) where

import Criterion.Main

import qualified Data.List as List
import Data.Monoid((<>))
import System.Directory(createDirectoryIfMissing, removeDirectoryRecursive)
import System.Info(os, arch)
import System.Process
import System.Exit(ExitCode(..))

protocZipInfo :: (String, String)
protocZipInfo =
  let
    os' = case os of
            "darwin" -> "osx"
            "linux"  -> "linux"
            other    -> error ("Unsupported operation system: " <> other)
    arch' = case arch of
              "x86_64" -> "x86_64"
              "x86_32" -> "x86_32"
              "x86"    -> "x86_32"
              other    -> error ("Unsupported architecture: " <> other)
  in
    (os', arch')

protocZipUrl :: FilePath
protocZipUrl =
    "https://github.com/google/protobuf/releases/download/v3.3.0/protoc-3.3.0-"
    <> os' <> "-" <> arch' <> ".zip"
  where
    (os', arch') = protocZipInfo

installProtoc :: IO FilePath
installProtoc = do
    (code, protocPath, _) <- readProcessWithExitCode "which" ["protoc"] ""
    case code of
        ExitSuccess -> do
          putStrLn $ "protoc found: " <> protocPath
          pure $ trimEndLines protocPath
        ExitFailure _ -> do
          putStrLn "protoc not found.  Installing to /tmp/bin/protoc"
          fetchAndInstall
          pure "/tmp/bin/protoc"
  where
    fetchAndInstall = do
      callProcess "curl" ["-o", "/tmp/protoc.zip", "-L", protocZipUrl]
      callProcess "unzip" ["-o", "/tmp/protoc.zip", "-d", "/tmp", "bin/protoc"]

getProtoPluginPath :: IO FilePath
getProtoPluginPath = do
  protoPluginPath <- readProcess "which" ["proto-lens-protoc"] ""
  pure $ trimEndLines protoPluginPath

getHsProtocPath :: IO FilePath
getHsProtocPath = do
  hsProtocPath <- readProcess "which" ["hsprotoc"] ""
  pure $ trimEndLines hsProtocPath

trimEndLines :: String -> String
trimEndLines = List.dropWhileEnd (=='\n')

benchProtoc :: FilePath -> FilePath -> FilePath -> Benchmark
benchProtoc protocPath pluginPath protoPath =
  bench protoPath $ perRunEnvWithCleanup
    (createDirectoryIfMissing True "/tmp/protoc-out")
    (\_ -> removeDirectoryRecursive "/tmp/protoc-out")
    (\_ -> runProtoc
             protocPath
             pluginPath
             ("bench/" <> protoPath <> ".proto")
    )


runProtoc :: FilePath -> FilePath -> FilePath -> IO ()
runProtoc protocPath pluginPath protoPath =
  callProcess protocPath
    [ "--plugin"
    , "protoc-gen-haskell=" <> pluginPath
    , "--haskell_out"
    , "/tmp/protoc-out"
    , protoPath
    ]

benchHsProtoc :: FilePath -> FilePath -> Benchmark
benchHsProtoc hsprotocPath protoPath =
  bench protoPath $ perRunEnvWithCleanup
    (createDirectoryIfMissing True "/tmp/hsprotoc-out")
    (\_ -> removeDirectoryRecursive "/tmp/hsprotoc-out")
    (\_ -> callProcess hsprotocPath
             [ "-o", "/tmp/hsprotoc-out"
             , "bench/" <> protoPath <> ".proto"
             ]
    )

main :: IO ()
main = do
  protocPath <- installProtoc
  protoPluginPath <- getProtoPluginPath
  hsProtocPath <- getHsProtocPath
  defaultMain
    [ bgroup "protoc"
       [ benchProtoc protocPath protoPluginPath "SearchResponse"
       , benchProtoc protocPath protoPluginPath "SearchResponse2"
       , benchProtoc protocPath protoPluginPath "Test3"
       ]
    , bgroup "hsprotoc"
       [ benchHsProtoc hsProtocPath "SearchResponse"
       , benchHsProtoc hsProtocPath "SearchResponse2"
       , benchHsProtoc hsProtocPath "Test3"
       ]
    ]

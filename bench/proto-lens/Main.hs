{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Main(main) where

import Criterion.Main
import Proto.Bench.SearchResponse

import Data.ProtoLens

import Data.ByteString(ByteString)
import Data.List(dropWhileEnd)
import Data.Monoid((<>))
import System.Exit(ExitCode(..))
import System.Info(os,arch)
import System.Process

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

protocZipFilePath :: FilePath
protocZipFilePath = concat ["protoc-3.3.0-", os', "-", arch', ".zip"]
  where
    (os', arch') = protocZipInfo

protocZipUrl :: FilePath
protocZipUrl = "https://github.com/google/protobuf/releases/download/v3.3.0/"
               <> protocZipFilePath

installProtoc :: IO FilePath
installProtoc = do
    (code, protocPath, _) <- readProcessWithExitCode "which" ["protoc"] ""
    case code of
        ExitSuccess -> do
          putStrLn $ "protoc found: " <> protocPath
          pure protocPath
        ExitFailure _ -> do
          putStrLn "protoc not found.  Installing to /tmp/bin/protoc"
          fetchAndInstall
          pure "/tmp/bin/protoc"
  where
    fetchAndInstall = do
      callProcess "curl" ["-OL", protocZipUrl]
      callProcess "unzip" ["-o", protocZipFilePath, "-d", "/tmp", "bin/protoc"]

main :: IO ()
main = do
    protocPath <- installProtoc
    pluginPath <- trimEnd <$> readProcess "stack" [ "exec", "which", "--", "proto-lens-protoc"] ""
    callProcess "stack" [ "exec", protocPath, "--"
                        , "--plugin=protoc-gen-haskell=" <> pluginPath
                        , "--haskell_out=bench/proto-lens"
                        , "bench/SearchResponse.proto" ]
    runBenchmarks
  where
    trimEnd = dropWhileEnd (== '\n')

sampleMessage :: SearchResponse
sampleMessage = SearchResponse "some query" 1 2 ["result 1", "result 2", "result 3"]

sampleMessageBytes :: ByteString
sampleMessageBytes = encodeMessage sampleMessage

runBenchmarks :: IO ()
runBenchmarks = defaultMain
  [ bgroup "Round trip"
     [ bench "Encode-Decode" $ whnf (decodeMessage @SearchResponse . encodeMessage) sampleMessage
     , bench "Decode-Encode" $ nf (encodeMessage . decodeMessageOrDie @SearchResponse) sampleMessageBytes
     ]
  ]

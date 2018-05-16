module Main(main) where

{-
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
main = defaultMain
  [ bgroup "Round trip encoding+decoding"
      bgroup "SearchResponse"
        [ roundTrip sampleMessage sampleMessageBytes ]
  ]
-}

main :: IO ()
main = pure ()

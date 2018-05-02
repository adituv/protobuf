{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Main where

import           Options

import           Data.Protobuf.CodeGen
import           Data.Protobuf.Parser

import           Control.Monad         (forM_)
import           Data.Semigroup        ((<>))
import qualified Data.Text.IO          as TIO
import qualified Data.Text.Lazy.IO     as TLIO
import           Options.Applicative   (execParser)
import           System.Exit           (exitFailure)
import           System.Directory      (createDirectoryIfMissing)
import           System.FilePath       (takeDirectory, (</>))
import           Text.Megaparsec       (parseErrorPretty, runParser)

main :: IO ()
main = do
    opts <- execParser options
    compileProtos opts

compileProtos :: Options -> IO ()
compileProtos opts@Options{sourceFiles} = mapM_ (compileProto opts) sourceFiles

compileProto :: Options -> FilePath -> IO ()
compileProto Options{..} srcPath = do
    src <- TIO.readFile srcPath
    spec <- pure (runParser proto3File srcPath src) >>= \case
        Left perror -> do
            putStrLn $ "Compilation error in \"" <> srcPath <> "\":"
            putStrLn ""
            putStrLn $ "\t" <> parseErrorPretty perror
            exitFailure
        Right s -> pure s
    dstFiles <- case genFiles namespace spec of
        Just files -> pure files
        Nothing    -> putStrLn "Codegen error!!!" >> exitFailure
    forM_ dstFiles $ \(path, contents) -> do
        let dir = outdir </> takeDirectory path
        createDirectoryIfMissing True dir   -- True = create parents
        TLIO.writeFile (outdir </> path) contents

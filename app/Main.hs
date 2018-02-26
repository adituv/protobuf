{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Protobuf.CodeGen
import Data.Protobuf.Encoding
import Data.Protobuf.Parser

import qualified Data.Text.Lazy
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy.IO as TLIO
import           System.Environment(getArgs)
import           System.Exit(exitFailure)
import           Text.Megaparsec(parseErrorPretty, runParser)

type LazyText = Data.Text.Lazy.Text

main :: IO ()
main = do
    (inFileName:_) <- getArgs
    src <- TIO.readFile inFileName
    let scope = "Autogen"
    let maybeSpec = runParser proto3File inFileName src
    spec <- case maybeSpec of
      Right spec -> pure spec
      Left perror -> do
          putStrLn $ parseErrorPretty perror
          exitFailure
    let files = genFiles scope spec
    mapM_ (uncurry TLIO.writeFile) files


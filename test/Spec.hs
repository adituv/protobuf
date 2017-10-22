module Main(main) where

import Test.Hspec
import Data.Protobuf.Parser.Tests

main :: IO ()
main = hspec $ do
  parserTests
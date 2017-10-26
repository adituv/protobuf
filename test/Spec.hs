module Main(main) where

import           Data.Protobuf.Parser.Tests
import           Test.Hspec

main :: IO ()
main = hspec $ do
  parserTests

module Main(main) where

import           Data.Protobuf.Parser.Tests
import           Data.Tree.ScopeTree.Tests
import           Test.Hspec

main :: IO ()
main = hspec $ do
  parserTests
  scopeTreeTests

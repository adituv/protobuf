module Data.Tree.ScopeTree.Tests where

import Data.Tree.ScopeTree

import Test.Hspec

import Data.Semigroup(All(..))
import Data.Tree(Tree(..), Forest)
import GHC.Exts(sortWith)

-- * Utility functions to check invariants

isValidTree :: Ord a => ScopeTree a -> Bool
isValidTree = (&&) <$> isSorted . nodes <*> getAll . foldMap (All . isSorted . subForest) . nodes
  where
    isSorted :: Ord a => Forest a -> Bool
    isSorted nodes = nodes == sortWith rootLabel nodes

-- * Tests

scopeTreeTests :: Spec
scopeTreeTests =
  describe "ScopeTree" $ do
    constructionTests
    lookupTests

sampleTree :: ScopeTree Int
sampleTree = insert [4,2]
           . insert [4,1]
           . insert [2]
           $ empty

sampleNodes :: Forest Int
sampleNodes = [Node 2 [], Node 4 [Node 1 [], Node 2 []]]

constructionTests :: Spec
constructionTests =
  describe "insert" $ do
    it "inserts a top-level name into an empty tree" $
      nodes (insert [1] empty) ==
        [Node 1 []]
    it "inserts a top-level name into a filled tree (start)" $
      nodes (insert [1] sampleTree) ==
        [Node 1 [], Node 2 [], Node 4 [Node 1 [], Node 2 []]]
    it "inserts a top-level name into a filled tree (mid)" $
      nodes (insert [3] sampleTree) ==
        [Node 2 [], Node 3 [], Node 4 [Node 1 [], Node 2 []]]
    it "inserts a top-level name into a filled tree (end)" $
      nodes (insert [5] sampleTree) ==
        [Node 2 [], Node 4 [Node 1 [], Node 2 []], Node 5 []]
    it "does not insert a node when already present" $
      nodes (insert [4] sampleTree) == sampleNodes
    it "correctly inserts an inner scope name" $
      nodes (insert [4, 0] sampleTree) ==
        [Node 2 [], Node 4 [Node 0 [], Node 1 [], Node 2 []]]

-- TODO Property tests

lookupTests :: Spec
lookupTests =
  describe "contains" $ do
    it "returns false for an empty tree" $
      not (empty `contains` [1])
    it "returns false for a missing top-level name" $
      not (sampleTree `contains` [3])
    it "returns true for a present top-level name" $
      sampleTree `contains` [4]
    it "returns false for a missing inner-scope name" $
      not (sampleTree `contains` [4,3])
    it "returns true for a present inner-scope name" $
      sampleTree `contains` [4,2]

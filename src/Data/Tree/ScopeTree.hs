module Data.Tree.ScopeTree(ScopeTree, nodes, empty, insert, merge, contains) where

import Data.Tree(Tree(..), Forest)
import GHC.Exts(groupWith, sortWith)

-- | Represents a tree of names in nested scopes.  Each scope itself is
--   assumed to be a name for the purposes of this, as other than the outer
--   namespace (which shall be handled separately), all scoping is provided
--   by nested message types.
data ScopeTree a = ScopeTree (Forest a)
-- Using a forest internally so we can have an empty tree of an
-- arbitrary type.  This is equivalent to a rose tree where you
-- discard the label of the root node.

-- | Access the nodes in a 'ScopeTree'
nodes :: ScopeTree a -> Forest a
nodes (ScopeTree ns) = ns

-- Invariant: all forests within the ScopeTree are sorted with respect to the
-- label of the nodes within.

-- | A 'ScopeTree' containing no names.
empty :: ScopeTree a
empty = ScopeTree []

-- | Insert a name into a 'ScopeTree'.  The name is represented as a list of
--   scopes from outermost scope to innermost scope.  For example,
--   \"Data.Tree.ScopeTree.ScopeTree\" would be represented as
--   @["Data", "Tree", "ScopeTree", "ScopeTree"]@
insert :: Ord a => [a] -> ScopeTree a -> ScopeTree a
insert xs (ScopeTree nodes) = ScopeTree $ insert' xs nodes

-- TODO: tail call optimisation
insert' :: Ord a => [a] -> Forest a -> Forest a
insert' [] nodes = nodes
insert' (x:xs) [] = [Node x (insert' xs [])]
insert' (x:xs) (y@(Node z children):ys)
  | x == z = Node x (insert' xs children):ys
  | x > z = y:insert' (x:xs) ys
  | otherwise = Node x (insert' xs []):y:ys

-- | Returns whether a name (represented as detailed in 'insert') is contained
--   within a 'ScopeTree'.  This function is intended to be used as an infix
--   operator (i.e. @tree `contains` name@)
contains :: Ord a => ScopeTree a -> [a] -> Bool
contains (ScopeTree nodes) = contains' nodes

contains' :: Ord a => Forest a -> [a] -> Bool
contains' tree [] = True
contains' [] (x:xs) = False
contains' (Node y children:ys) (x:xs)
  | x == y = contains' children xs
  | x > y  = contains' ys (x:xs)
  | otherwise = False

-- | Combine two trees of scoped names into a single 'ScopeTree'
merge :: Ord a => ScopeTree a -> ScopeTree a -> ScopeTree a
merge (ScopeTree n1) (ScopeTree n2) = ScopeTree $ fixup (n1 ++ n2)
  where
    fixup = fmap combineDupes . groupWith rootLabel . sortWith rootLabel
    combineDupes [n] = n
    combineDupes [Node x c1, Node _ c2] = Node x $ fixup (c1 ++ c2)
    combineDupes _ = error "Assumption incorrect in combineDupes: list length == 1 or 2"

-- As for our purposes we do not currently need to remove names from the tree,
-- deletion methods are omitted.  I am pretty sure 'merge' and 'empty' form a
-- monoid over 'ScopeTree' but want to look into the monoid laws further before
-- adding that.

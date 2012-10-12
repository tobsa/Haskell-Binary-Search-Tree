------------------------------------------------------------
-- BinaryTree.hs
------------------------------------------------------------

------------------------------------------------------------
-- Module exports
------------------------------------------------------------
module BinTree (treeMember) where

------------------------------------------------------------
-- Define a binary tree
------------------------------------------------------------
data (Ord a) => BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)

------------------------------------------------------------
-- Define some random trees (for testing purposes)
------------------------------------------------------------
tree0 = Empty
tree1 = Node 0 Empty Empty
tree2 = Node 5 Empty Empty
tree3 = Node 10 Empty Empty
tree4 = Node 15 Empty Empty
tree5 = Node 20 Empty Empty
tree6 = Node 25 Empty Empty
tree7 = Node 13 tree3 tree4

------------------------------------------------------------
-- treeMember
-- Check if an element is in a tree
------------------------------------------------------------
treeMember :: (Ord a,Show a) => BinaryTree a -> a -> Bool
treeMember Empty _ = False
treeMember tree x  = True




------------------------------------------------------------
-- BinaryTree.hs
------------------------------------------------------------

------------------------------------------------------------
-- Module exports
------------------------------------------------------------
module BinTree (treeMember,treeHeight) where

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
tree8 = Node 16 tree7 tree6

------------------------------------------------------------
-- treeInsert
-- Insert an element into a tree
------------------------------------------------------------
treeInsert :: (Ord a,Show a) => BinaryTree a -> a -> BinaryTree a
treeInsert Empty x = Node x Empty Empty
treeInsert (Node x left right) v
    | v == x = Node x left right
    | v < x  = Node x (treeInsert left v) right
    | v > x  = Node x left (treeInsert right v)

------------------------------------------------------------
-- treeMember
-- Check if an element is in a tree
------------------------------------------------------------
treeMember :: (Ord a,Show a) => BinaryTree a -> a -> Bool
treeMember Empty _ = False
treeMember (Node x left right) v 
    | v == x = True
    | v < x  = treeMember left v
    | v > x  = treeMember right v

------------------------------------------------------------
-- treeHeight
-- Return the height of the tree
------------------------------------------------------------
treeHeight :: (Ord a, Show a) => BinaryTree a -> Integer
treeHeight Empty = 0
treeHeight (Node x left right) = 1 + max (treeHeight left) (treeHeight right)




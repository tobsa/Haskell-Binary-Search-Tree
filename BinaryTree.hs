------------------------------------------------------------
-- BinaryTree.hs
------------------------------------------------------------

------------------------------------------------------------
-- Module exports
------------------------------------------------------------
module BinTree (treeInsert,
                treeMember,
                treeHeight,
                treeDelete,
                treeBuild,
                treePreorder,
                treeInorder,
                treePostorder) 
                where
------------------------------------------------------------

------------------------------------------------------------
-- Define a binary tree
------------------------------------------------------------
data (Ord a) => BinaryTree a = Empty | Node a (BinaryTree a) (BinaryTree a) deriving (Show)
------------------------------------------------------------

------------------------------------------------------------
-- Define some random trees (for testing purposes)
------------------------------------------------------------
tree0  = Empty
tree1  = Node 0 Empty Empty
tree2  = Node 5 Empty Empty
tree3  = Node 10 Empty Empty
tree4  = Node 15 Empty Empty
tree5  = Node 20 Empty Empty
tree6  = Node 25 Empty Empty
tree7  = Node 13 tree3 tree4
tree8  = Node 16 tree7 tree6
tree9  = Node 50 tree5 Empty
tree10 = Node 50 Empty tree5
------------------------------------------------------------



------------------------------------------------------------
-- treeInsert
-- Insert an element into a tree
------------------------------------------------------------
treeInsert :: (Ord a,Show a) => BinaryTree a -> a -> BinaryTree a

-- Handle empty tree
treeInsert Empty x = Node x Empty Empty

-- Otherwise insert the value 
treeInsert (Node x left right) v
    | v == x = Node x left right
    | v < x  = Node x (treeInsert left v) right
    | v > x  = Node x left (treeInsert right v)
------------------------------------------------------------

    
------------------------------------------------------------
-- treeDelete
-- Delete an element from a tree
------------------------------------------------------------
treeDelete :: (Ord a,Show a) => BinaryTree a -> a -> BinaryTree a

-- Handle empty tree
treeDelete Empty _ = Empty

-- Delete a node with one leaf
treeDelete (Node x left Empty) v  | v == x = left
treeDelete (Node x Empty right) v | v == x = right

-- Delete a node with two leaves
treeDelete (Node x left right) v
    | v == x = Node key left (treeDelete right key)
    | v < x  = Node x (treeDelete left v) right
    | v > x  = Node x left (treeDelete right v)
    where   key = treeMinimum right
            treeMinimum (Node x _ Empty) = x
            treeMinimum (Node _ left _)  = treeMinimum left
------------------------------------------------------------



------------------------------------------------------------
-- treeBuild
-- Build a binary tree from a given list
------------------------------------------------------------
treeBuild :: (Ord a,Show a) => [a] -> BinaryTree a

-- Get an empty tree if the list is empty
treeBuild [] = Empty

-- Build a tree from a list
treeBuild xs = foldl treeInsert Empty xs
------------------------------------------------------------



------------------------------------------------------------
-- treeMember
-- Check if an element is in a tree
------------------------------------------------------------
treeMember :: (Ord a,Show a) => BinaryTree a -> a -> Bool

-- Handle empty tree
treeMember Empty _ = False

-- Otherwise check if the value exist
treeMember (Node x left right) v 
    | v == x = True
    | v < x  = treeMember left v
    | v > x  = treeMember right v
------------------------------------------------------------
    
    
    
------------------------------------------------------------
-- treeHeight
-- Return the height of the tree
------------------------------------------------------------
treeHeight :: (Ord a, Show a) => BinaryTree a -> Integer

-- Handle empty tree
treeHeight Empty = 0

-- Otherwise calculate the height of the tree
treeHeight (Node x left right) = 1 + max (treeHeight left) (treeHeight right)
------------------------------------------------------------



------------------------------------------------------------
-- treePreorder
-- Get a list of all the elements inorder
------------------------------------------------------------
treePreorder :: (Ord a,Show a) => BinaryTree a -> [a]

-- Handle empty tree
treePreorder Empty = []

-- Produce the preorder list
treePreorder (Node x left right) = [x] ++ treePreorder left ++ treePreorder right
------------------------------------------------------------



------------------------------------------------------------
-- treeInorder
-- Get a list of all the elements inorder
------------------------------------------------------------
treeInorder :: (Ord a,Show a) => BinaryTree a -> [a]

-- Handle empty tree
treeInorder Empty = []

-- Produce the inorder list
treeInorder (Node x left right) = treeInorder left ++ [x] ++ treeInorder right
------------------------------------------------------------



------------------------------------------------------------
-- treePostorder
-- Get a list of all the elements inorder
------------------------------------------------------------
treePostorder :: (Ord a,Show a) => BinaryTree a -> [a]

-- Handle empty tree
treePostorder Empty = []

-- Produce the postorder list
treePostorder (Node x left right) = treePostorder left ++ treePostorder right ++ [x]
------------------------------------------------------------

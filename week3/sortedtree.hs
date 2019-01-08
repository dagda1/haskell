-- sortedtree.hs
-- Jeremy.Singer@glasgow.ac.uk
-- Example code for #FLhaskell course

-- Nodes contain integers, Leaves are empty
data Tree = Leaf | Node Int Tree Tree deriving Show

treeDepth :: Tree -> Int
-- longest path from root to a leaf
treeDepth Leaf = 0
treeDepth (Node _ leftSubtree rightSubtree) = 
  1 + max (treeDepth leftSubtree) (treeDepth rightSubtree)

isSortedTree :: Tree -> Int -> Int -> Bool
-- is the tree sorted in-order?
-- the two Int params indicate min and max
-- for the current subtree
isSortedTree Leaf _ _ = True
isSortedTree (Node x leftSubtree rightSubtree) minVal maxVal =
    let leftSorted  = isSortedTree leftSubtree minVal x
        rightSorted = isSortedTree rightSubtree x maxVal
    in x >= minVal && x< maxVal && leftSorted && rightSorted

addNewMax :: Tree -> Tree
-- add a new max element to tree
-- will go down rightmost path to Leaf
addNewMax Leaf = Node 0 Leaf Leaf  -- input tree with no nodes
addNewMax (Node x t1 Leaf) = Node x t1 (Node (x+1) Leaf Leaf)  -- this is the rightmost Node
addNewMax (Node x t1 t2) = Node x t1 (addNewMax t2) -- intermediate node, go down right subtree

-- Node 1 Leaf (Node 2 (Node 3 Leaf Leaf) (Node 4 Leaf (Node 5 Leaf (Node 6 Leaf Leaf))))
listTree :: Tree -> [Int]
listTree Leaf = []
listTree (Node x t1 t2) = x:(listTree t1) ++ (listTree t2)

insert :: Int -> Tree -> Tree
insert v Leaf = (Node v Leaf Leaf)
insert v (Node x t1 t2)
  | v > x = Node x t1 (insert v t2)
  | v < x = Node x (insert v t1) t2
  | otherwise = Node v t1 t2

multiply :: Int -> Int -> Int
multiply a b = do
  a * b

data Person = Person { name :: String }
greet :: Person -> String -> String
greet person otherName =
  "Hi " ++ otherName ++ ", my name is " ++ name person

module BST
    ( BST
    , bstLeft
    , bstRight
    , bstValue
    , empty
    , fromList
    , insert
    , singleton
    , toList
    ) where

data BST a = Leaf | Node (BST a) a (BST a) deriving (Eq, Show)

bstLeft :: BST a -> Maybe (BST a)
bstLeft Leaf = Nothing
bstLeft (Node l _ _) = Just l

bstRight :: BST a -> Maybe (BST a)
bstRight Leaf = Nothing
bstRight (Node _ _ r) = Just r

bstValue :: BST a -> Maybe a
bstValue Leaf = Nothing
bstValue (Node _ a _) = Just a

empty :: BST a
empty = Leaf

fromList :: Ord a => [a] -> BST a
fromList [] = Leaf
fromList (x:xs) = let left = [y | y<-xs, y <= x]
                      right = [y | y<-xs, y > x] in
                    Node (fromList left) x (fromList right)

insert :: (Ord a) => a -> BST a -> BST a 
insert x Leaf = Node Leaf x Leaf
insert x (Node l v r) | x < v = Node (insert x l) v r
                      | x > v = Node l v (insert x r)
                      | otherwise = Node (insert x l) v r

singleton :: a -> BST a
singleton x = Node Leaf x Leaf

toList :: BST a -> [a]
toList Leaf = []
toList (Node Leaf x Leaf) = [x]
toList (Node l x Leaf) = toList l ++ [x]
toList (Node Leaf x r) = x : toList r
toList (Node l x r) = toList l ++ [x] ++ toList r
 

data Color
    = Red
    | Black
    deriving (Show, Eq)
    
data RBTree a
    = Node Color a (RBTree a) (RBTree a)
    | Nil
    deriving (Show)

fromList :: (Ord a) => [a] -> (RBTree a)
fromList = foldl (flip insert) Nil

toList :: (Ord a) => (RBTree a) -> [a]
toList Nil = []
toList (Node _ v l r) = (toList l) ++ [v] ++ (toList r)

check :: (Ord a) => (RBTree a) -> Bool
check Nil = True
check (Node Red _ l r)
    | isBlack l && isBlack r = check l && check r
    | otherwise = False
check (Node _ _ l r) = check l && check r

find :: (Ord a, Eq a) => a -> (RBTree a) -> (Maybe a)
find x Nil = Nothing
find x (Node _ v l r)
    | x < v  = find x l
    | x > v  = find x r
    | x == v = Just v
    
insert :: (Ord a) => a -> (RBTree a) -> (RBTree a)
insert x t = putBlack $ insert' x t
    where
        insert' x Nil = Node Red x Nil Nil
        insert' x (Node c v l r)
            | x <= v = fixInsertL $ Node c v (insert' x l) r 
            | x >  v = fixInsertR $ Node c v l (insert' x r)
        fixInsertL
            g@(Node Black _
                p@(Node Red _ (Node Red _ _ _) _)
                u@(Node Red _ _ _)) = 
            put (invert g) (invert p) (invert u)
        fixInsertL
            g@(Node _ _
                p@(Node Red _ _ (Node Red _ _ _))
                _) =
            fixInsertR $ g `putLeft` rotateLeft p    
        fixInsertL
            g@(Node Black _
                p@(Node Red _
                    (Node Red _ _ _)
                    _)
                u)| isBlack u =
            rotateRight ((invert g) `putLeft` (invert p))
        fixInsertL x = x
        
        fixInsertR
            g@(Node Black _
                u@(Node Red _ _ _)
                p@(Node Red _ _ (Node Red _ _ _))) = 
            put (invert g) (invert u) (invert p)
        fixInsertR
            g@(Node _ _
                _
                p@(Node Red _ (Node Red _ _ _) _)) =
            fixInsertR $ g `putRight` rotateRight p
        fixInsertR
            g@(Node Black _
                u
                p@(Node Red _
                    _
                    (Node Red _ _ _))) | isBlack u=
            rotateLeft ((invert g) `putRight` (invert p))
        fixInsertR x = x
    
isBlack (Node Black _ _ _) = True
isBlack Nil = True
isBlack _ = False

isRed = not . isBlack

rotateRight :: (RBTree a) -> (RBTree a)
rotateRight n@(Node _ _ l@(Node _ _ _ r) _) =
    l `putRight` (n `putLeft` r)
    
rotateLeft :: (RBTree a) -> (RBTree a)
rotateLeft n@(Node _ _ _ r@(Node _ _ l _)) =
    r `putLeft` (n `putRight` l)
    
invert (Node Red v l r) = Node Black v l r
invert (Node Black v l r) = Node Red v l r

putLeft (Node c v _ r) l = Node c v l r
putRight (Node c v l _) r = Node c v l r
put (Node c v _ _) l r = Node c v l r

putBlack (Node _ v l r) = Node Black v l r
putRed (Node _ v l r) = Node Red v l r
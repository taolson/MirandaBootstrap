|| -*- mode: indented-text -*-
||
|| avl.m -- AVL tree


%export + -height -delta -rotL -rotR -balanceL -balanceR

%include "maybe"


avlTree * ::= AVLLeaf | AVLNode * (avlTree *) (avlTree *) num

height :: avlTree * -> num
height AVLLeaf           = 0
height (AVLNode v l r h) = h

delta :: avlTree * -> num
delta AVLLeaf           = 0
delta (AVLNode v l r h) = height l - height r

rotL :: avlTree * -> avlTree *
rotL (AVLNode v l (AVLNode vr lr rr hr) h)
    = a_balance (AVLNode vr (a_balance (AVLNode v l lr h)) rr hr)

rotL n = n

rotR :: avlTree * -> avlTree *
rotR (AVLNode v (AVLNode vl ll rl hl) r h)
    = a_balance (AVLNode vl ll (a_balance (AVLNode v rl r h)) hl)

rotR n = n

a_moveR :: avlTree * -> avlTree * -> avlTree *
a_moveR (AVLLeaf) rn = rn
a_moveR (AVLNode v l r h) rn
    = a_balance n
      where
        mrn = a_moveR r rn
        n   = AVLNode v l mrn h

a_balance :: avlTree * -> avlTree *
a_balance AVLLeaf = AVLLeaf
a_balance n
    = balanceR n,       if d < -1
    = balanceL n,       if d > 1
    = AVLNode v l r h', otherwise
      where
        AVLNode v l r h = n
        d               = delta n
        h'              = max2 (height l) (height r) + 1

balanceR :: avlTree * -> avlTree *
balanceR n = rotL n', if delta r > 0
           = rotL n,  otherwise
             where
               AVLNode v l r h = n
               rr              = rotR r
               n'              = AVLNode v l rr h

balanceL :: avlTree * -> avlTree *
balanceL n = rotR n', if delta l < 0
           = rotR n,  otherwise
             where
               AVLNode v l r h = n
               rl              = rotL l
               n'              = AVLNode v rl r h

a_empty :: avlTree *
a_empty = AVLLeaf

a_null :: avlTree * -> bool
a_null AVLLeaf = True
a_null n       = False

a_singleton :: * -> avlTree *
a_singleton v = AVLNode v AVLLeaf AVLLeaf 1

a_size :: avlTree * -> num
a_size AVLLeaf = 0
a_size (AVLNode v l r h)
    = sl $seq sr $seq (sl + sr + 1)
      where
        sl = a_size l
        sr = a_size r

a_member :: * -> avlTree * -> bool
a_member v
    = go
      where
        go AVLLeaf = False
        go (AVLNode v' l r h)
            = True, if v == v'
            = go l, if v < v'
            = go r, otherwise

a_first :: avlTree * -> *
a_first AVLLeaf = error "a_first: null tree"
a_first (AVLNode v l r h)
    = v,         if l == AVLLeaf
    = a_first l, otherwise

a_last :: avlTree * -> *
a_last AVLLeaf = error "a_last: null tree"
a_last (AVLNode v l r h)
    = v,        if r == AVLLeaf
    = a_last r, otherwise

a_insert :: * -> avlTree * -> avlTree *
a_insert v
    = go
      where
        go  AVLLeaf = a_singleton v
        go (AVLNode v' l r h)
            = a_balance (AVLNode v' (go l) r h), if v < v'
            = a_balance (AVLNode v' l (go r) h), otherwise

|| delete v from tree if present, or return the original tree
a_delete :: * -> avlTree * -> avlTree *
a_delete v n
    = fromMaybe n (go n)
      where
        go AVLLeaf = Nothing
        go (AVLNode v' l r h)
            = Just (a_moveR l r), if v == v'
            = insL $mb_fmap go l, if v < v'
            = insR $mb_fmap go r, otherwise
              where
                insL l' = a_balance (AVLNode v' l' r h)
                insR r' = a_balance (AVLNode v' l r' h)

a_fromList :: [*] -> avlTree *
a_fromList = foldl (converse a_insert) a_empty

a_toList :: avlTree * -> [*]
a_toList AVLLeaf           = []
a_toList (AVLNode v l r h) = a_toList l ++ [v] ++ a_toList r

a_union :: avlTree * -> avlTree * -> avlTree *
a_union a1 a2 = foldr a_insert a1 (a_toList a2)


|| functor interface
a_fmap :: (* -> **) -> avlTree * -> avlTree **
a_fmap f = a_fromList . map f . a_toList        || mapping can change value order, so rebuild from mapped list


|| foldable interface
a_fold :: (* -> ** -> **) -> ** -> avlTree * -> **
a_fold f s AVLLeaf           = s
a_fold f s (AVLNode v l r h) = a_fold f (a_fold f (f v s) l) r

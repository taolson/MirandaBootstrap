|| -*- mode: indented-text -*-
||
|| set.m -- implementation of a strict set using AVL trees


%export + s_empty s_null s_singleton s_size s_member s_first s_last s_delete s_toList s_fold

%include "avl" s_empty/a_empty s_null/a_null s_singleton/a_singleton s_size/a_size 
               s_member/a_member s_first/a_first s_last/a_last s_delete/a_delete s_toList/a_toList
               s_fold/a_fold


s_set * == avlTree *

s_insert :: * -> s_set * -> s_set *
s_insert v
    = go
      where
        go  AVLLeaf = s_singleton v
        go (AVLNode v' l r h)
            = AVLNode v l r h,                   if v == v'
            = a_balance (AVLNode v' (go l) r h), if v < v'
            = a_balance (AVLNode v' l (go r) h), otherwise

s_deleteFindMin :: s_set * -> (*, s_set *)
s_deleteFindMin s
    = s' $seq (x, s')
      where
        x  = s_first s
        s' = x $seq s_delete x s

s_fromList :: [*] -> s_set *
s_fromList = foldr s_insert s_empty

s_union :: s_set * -> s_set * -> s_set *
s_union s1 s2 = foldl (converse s_insert) s1 (s_toList s2)

s_difference :: s_set * -> s_set * -> s_set *
s_difference s1 s2
    = foldl (converse s_delete) s1 (s_toList s2)

s_intersect :: s_set * -> s_set * -> s_set *
s_intersect s1 s2
    = s_fromList (go (s_toList s1) (s_toList s2))
      where
        go (x : xs) (y : ys)
            =     go xs       (y : ys), if x < y
            =     go (x : xs) ys,       if x > y
            = x : go xs       ys,       otherwise
        go xs ys = []

s_filter :: (* -> bool) -> s_set * -> s_set *
s_filter p = s_fromList . filter p . s_toList


|| functor interface
|| s_fmap changes the key value that is used in the AVL tree, so the easiest
|| way to do it is to serialize to a list, map, then deserialize to the new set
s_fmap :: (* -> **) -> s_set * -> s_set **
s_fmap f = s_fromList . map f . s_toList

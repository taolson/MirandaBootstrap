|| -*- mode: indented-text -*-
||
|| bag.m -- implementation of a strict multiset using AVL trees


%export + b_empty b_null b_size b_member b_first b_last b_deleteKey b_lookup b_findWithDefault b_elems b_keys b_keysSet b_toList


%include "avl"
%include "map" b_empty/m_empty b_null/m_null b_size/m_size b_member/m_member b_first/m_first b_last/m_last b_lookup/m_lookup
               b_findWithDefault/m_findWithDefault b_elems/m_elems b_keys/m_keys b_keysSet/m_keysSet b_toList/m_toList b_deleteKey/m_delete
%include "maybe"


b_bag * == m_map * num

b_singleton :: * -> b_bag *
b_singleton x = m_singleton x 1

b_insert :: * -> b_bag * -> b_bag *
b_insert key = m_insertWith (+) key 1

b_insertTimes :: * -> num -> b_bag * -> b_bag *
b_insertTimes = m_insertWith (+)

b_delete :: * -> b_bag * -> b_bag *
b_delete key = b_deleteTimes key 1

b_deleteTimes :: * -> num -> b_bag * -> b_bag *
b_deleteTimes key count m
    = snd (del m)
      where
        del AVLLeaf = (False, AVLLeaf)
        del n       = (True, nd),         if key == k & count < v
                    = mr $seq (True, mr), if key == k
                    = bn $seq (True, bn), if deletedN
                    = (False, n),         otherwise
                      where
                        AVLNode (k, v) l r h = n
                        deletedN             = deletedL, if key < k
                                             = deletedR, otherwise
                        nd                   = AVLNode (k, v - count) l r h
                        n'                   = AVLNode (k, v) dl r h, if key < k
                                             = AVLNode (k, v) l dr h, otherwise
                        (deletedL, dl)       = del l
                        (deletedR, dr)       = del r
                        mr                   = a_moveR l r
                        bn                   = a_balance n'

b_fromList :: [*] -> b_bag *
b_fromList = foldr b_insert b_empty

b_fromCountList :: [(*, num)] -> b_bag *
b_fromCountList
    = foldl addEntry b_empty
      where
        addEntry b (k, v) = b_insertTimes k v b

b_withKeys :: [*] -> b_bag *
b_withKeys
    = foldl ins b_empty
      where
        ins b k = b_insertTimes k 0 b

b_union :: b_bag * -> b_bag * -> b_bag *
b_union b1 b2
    = foldl addEntry b1 (b_toList b2)
      where
        addEntry b (k, v) = b_insertTimes k v b

b_difference :: b_bag * -> b_bag * -> b_bag *
b_difference b1 b2
    = foldl delEntry b1 (b_toList b2)
      where
        delEntry b (k, v) = b_deleteTimes k v b

|| -*- mode: indented-text -*-
||
|| map.m -- implementation of a strict map from key to value, using AVL trees


%export + m_empty m_null m_size m_first m_last m_toList

%include "maybe"
%include "avl" m_empty/a_empty m_null/a_null m_size/a_size m_first/a_first m_last/a_last m_toList/a_toList


m_map * ** == avlTree (*, **)

m_singleton :: * -> ** -> m_map * **
m_singleton k v = v $seq a_singleton (k, v)

m_member :: * -> m_map * ** -> bool
m_member k
    = go
      where
        go  AVLLeaf = False
        go (AVLNode (k', v) l r h)
            = True, if k == k'
            = go l, if k < k'
            = go r, otherwise

m_insert :: * -> ** -> m_map * ** -> m_map * **
m_insert k v
    = go
      where
        go AVLLeaf = m_singleton k v
        go (AVLNode p l r h)
            = v $seq AVLNode (k, v) l r h,      if k == k'
            = a_balance (AVLNode p (go l) r h), if k < k'
            = a_balance (AVLNode p l (go r) h), otherwise
              where
                (k', v') = p

m_insertWith :: (** -> ** -> **) -> * -> ** -> m_map * ** -> m_map * **
m_insertWith f k v
    = go
      where
        go AVLLeaf = m_singleton k v
        go (AVLNode p l r h)
            = fv $seq AVLNode (k, fv) l r h,    if k == k'
            = a_balance (AVLNode p (go l) r h), if k < k'
            = a_balance (AVLNode p l (go r) h), otherwise
              where
                (k', v') = p
                fv       = f v v'

|| delete the key k and its value from the map, if present,
m_delete :: * -> m_map * ** -> m_map * **
m_delete k n
    = fromMaybe n (go n)
      where
        go AVLLeaf = Nothing
        go (AVLNode p l r h)
            = Just (a_moveR l r), if k == k'
            = insL $mb_fmap go l, if k < k'
            = insR $mb_fmap go r, otherwise
              where
                (k', v') = p
                insL l'  = a_balance (AVLNode p l' r h)
                insR r'  = a_balance (AVLNode p l r' h)

m_adjust :: (** -> **) -> * -> m_map * ** -> m_map * **
m_adjust f k
    = go
      where
        go AVLLeaf = AVLLeaf
        go (AVLNode p l r h)
            = fv $seq AVLNode (k, fv) l r h, if k == k'
            = AVLNode p (go l) r h,          if k < k'
            = AVLNode p l (go r) h,          otherwise
              where
                (k', v') = p
                fv       = f v'

m_lookup :: * -> m_map * ** -> maybe **
m_lookup k
    = go
      where
        go AVLLeaf = Nothing
        go (AVLNode (k', v') l r h)
            = Just v', if k == k'
            = go l,    if k < k'
            = go r,    otherwise

m_findWithDefault :: ** -> * -> m_map * ** -> **
m_findWithDefault d k m
    = d, if isNothing search
    = v, otherwise
      where
        search = m_lookup k m
        v      = fromJust search

m_fromList :: [(*, **)] -> m_map * **
m_fromList
    = foldl ins m_empty
      where
        ins m (a, b) = m_insert a b m

m_elems :: m_map * ** -> [**]
m_elems AVLLeaf                = []
m_elems (AVLNode (k, v) l r h) = m_elems l ++ [v] ++ m_elems r

m_keys :: m_map * ** -> [*]
m_keys AVLLeaf                = []
m_keys (AVLNode (k, v) l r h) = m_keys l ++ [k] ++ m_keys r

|| extract the map's keys as a set
|| note that it extracts to an avlTree (parent of set) to prevent
|| having a dependency on the set module.  Since the map's keys
|| are guaranteed to be unique, they naturally form a set
m_keysSet :: m_map * ** -> avlTree *
m_keysSet AVLLeaf                = AVLLeaf
m_keysSet (AVLNode (k, v) l r h) = AVLNode k (m_keysSet l) (m_keysSet r) h

m_union :: m_map * ** -> m_map * ** -> m_map * **
m_union m1 m2
    = foldl addEntry m1 (m_toList m2)
      where
        addEntry m (k, v) = m_insert k v m

m_filter :: (** -> bool) -> m_map * ** -> m_map * **
m_filter p m
    = m_filterWithKey (ignoreKey p) m
      where
        ignoreKey p (k, x) = p x

m_filterWithKey :: ((*, **) -> bool) -> m_map * ** -> m_map * **
m_filterWithKey p = m_fromList . filter p . m_toList

m_mapAccumL :: (*** -> ** -> (***, ****)) -> *** -> m_map * ** -> (***, m_map * ****)
m_mapAccumL f s AVLLeaf = (s, AVLLeaf)
m_mapAccumL f s (AVLNode (k, v) l r h)
    = (s3, AVLNode (k, v') l' r' h)
      where
        (s1, l') = m_mapAccumL f s l
        (s2, v') = f s1 v
        (s3, r') = m_mapAccumL f s2 r


|| functor interface
m_fmap :: (** -> ***) -> m_map * ** -> m_map * ***
m_fmap f
    = go
      where
        go AVLLeaf                = AVLLeaf
        go (AVLNode (k, v) l r h) = AVLNode (k, f v) (go l) (go r) h

m_fmapWithKey :: (* -> ** -> ***) -> m_map * ** -> m_map * ***
m_fmapWithKey f
    = go
      where
        go AVLLeaf                = AVLLeaf
        go (AVLNode (k, v) l r h) = AVLNode (k, f k v) (go l) (go r) h


|| foldable interface
m_fold :: (** -> *** -> ***) -> *** -> m_map * ** -> ***
m_fold f s AVLLeaf                = s
m_fold f s (AVLNode (k, v) l r h) = m_fold f (m_fold f (f v s) l) r

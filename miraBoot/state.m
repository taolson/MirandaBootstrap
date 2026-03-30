|| -*- mode: indented-text -*-
||
|| state.m -- state functor / applicative / monad


%export +


state * ** == (* -> (**, *))

|| running
st_runState :: state * ** -> * -> (**, *)
st_runState = id

st_evalState :: state * ** -> * -> **
st_evalState m = fst . st_runState m

st_execState :: state * ** -> * -> *
st_execState m = snd . st_runState m


|| state access
st_get :: state * *
st_get = go where go s = (s, s)

st_put :: * -> state * ()
st_put s' = const ((), s')

st_modify :: (* -> *) -> state * ()
st_modify f = go where go s = ((), f s)


|| monad interface
st_bind :: state * ** -> (** -> state * ***) -> state * ***
st_bind m f
    = go
      where
        go s = bind (st_runState m s)

        bind (a, s') = st_runState (f a) s'

st_join :: state * (state * **) -> state * **
st_join ma
    = go
      where
        go s          = join (st_runState ma s)
        join (mb, s') = st_runState mb s'

st_pure :: ** -> state * **
st_pure a = go where go s = (a, s)

st_kbind :: (** -> state * ***) -> (*** -> state * ****) -> ** -> state * ****
st_kbind f g = go where go x = f x $st_bind g

st_mapM :: (** -> state * ***) -> [**] -> state * [***]
st_mapM f = foldr (st_liftA2 (:) . f) (st_pure [])

st_mapM_ :: (** -> state * ***) -> [**] -> state * ()
st_mapM_ f = foldr (st_right . f) (st_pure ())

st_foldM :: (** -> *** -> state * **) -> ** -> [***] -> state * **
st_foldM f z0 xs
    = foldr c st_pure xs z0
      where
        c x k z = f z x $st_bind k


|| functor interface
st_fmap :: (** -> ***) -> state * ** -> state * ***
st_fmap f m = m $st_bind (st_pure . f)


|| applicative interface
st_apply :: state * (** -> ***) -> state * ** -> state * ***
st_apply mf ma = mf $st_bind converse st_fmap ma

st_liftA2 :: (** -> *** -> ****) -> state * ** -> state * *** -> state * ****
st_liftA2 f ma mb = st_fmap f ma $st_apply mb

st_liftA3 :: (** -> *** -> **** -> *****) -> state * ** -> state * *** -> state * **** -> state * *****
st_liftA3 f ma mb mc = st_liftA2 f ma mb $st_apply mc

st_liftA4 :: (** -> *** -> **** -> ***** -> ******) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * ******
st_liftA4 f ma mb mc md = st_liftA3 f ma mb mc $st_apply md

st_liftA5 :: (** -> *** -> **** -> ***** -> ****** -> *******) -> state * ** -> state * *** -> state * **** -> state * ***** -> state * ****** ->
             state * *******
st_liftA5 f ma mb mc md me = st_liftA4 f ma mb mc md $st_apply me

st_liftA6 :: (** -> *** -> **** -> ***** -> ****** -> ******* -> ********) ->
             state * ** -> state * *** -> state * **** -> state * ***** -> state * ****** -> state * ******* -> state * ********
st_liftA6 f ma mb mc md me mf = st_liftA5 f ma mb mc md me $st_apply mf

st_liftA7 :: (** -> *** -> **** -> ***** -> ****** -> ******* -> ******** -> *********) ->
             state * ** -> state * *** -> state * **** -> state * ***** -> state * ****** -> state * ******* -> state * ******** -> state * *********
st_liftA7 f ma mb mc md me mf mg = st_liftA6 f ma mb mc md me mf $st_apply mg

st_left :: state * ** -> state * *** -> state * **
st_left = st_liftA2 const

st_right :: state * ** -> state * *** -> state * ***
st_right = st_liftA2 (converse const)


|| lift multiple independent state actions and bind them to an N-ary function
st_bind2 :: state * ** -> state * *** -> (** -> *** -> state * ****) -> state * ****
st_bind2 ma mb f = st_join (st_liftA2 f ma mb)

st_bind3 :: state * ** -> state * *** -> state * **** -> (** -> *** -> **** -> state * *****) -> state * *****
st_bind3 ma mb mc f = st_join (st_liftA3 f ma mb mc)

st_bind4 :: state * ** -> state * *** -> state * **** -> state * ***** -> (** -> *** -> **** -> ***** -> state * ******) -> state * ******
st_bind4 ma mb mc md f = st_join (st_liftA4 f ma mb mc md)

st_bind5 :: state * ** -> state * *** -> state * **** -> state * ***** -> state * ****** ->
            (** -> *** -> **** -> ***** -> ****** -> state * *******) -> state * *******
st_bind5 ma mb mc md me f = st_join (st_liftA5 f ma mb mc md me)

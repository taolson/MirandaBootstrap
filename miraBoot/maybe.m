|| -*- mode: indented-text -*-
||
|| maybe.m -- sum type of "Nothing" and another type


%export +


maybe * ::= Nothing | Just *

fromMaybe :: * -> maybe * -> *
fromMaybe default Nothing  = default
fromMaybe default (Just x) = x

|| called "maybe" in Haskell, but that conflicts with the datatype name
fromMaybef :: ** -> (* -> **) -> maybe * -> **
fromMaybef default f Nothing  = default
fromMaybef default f (Just x) = f x

fromJust :: maybe * -> *
fromJust (Just x) = x
fromJust ma       = error "fromJust: Nothing"

isNothing :: maybe * -> bool
isNothing Nothing = True
isNothing x       = False

isJust :: maybe * -> bool
isJust (Just x) = True
isJust x        = False

appendMaybe :: maybe * -> maybe * -> maybe *
appendMaybe Nothing b = b
appendMaybe a       x = a

catMaybes :: [maybe *] -> [*]
catMaybes [] = []
catMaybes (Nothing : xs) = catMaybes xs
catMaybes (Just x : xs)  = x : catMaybes xs

mapMaybe :: (* -> maybe **) -> [*] -> [**]
mapMaybe f lst = catMaybes (map f lst)


|| monad interface
mb_bind :: maybe * -> (* -> maybe **) -> maybe **
mb_bind Nothing f  = Nothing
mb_bind (Just x) f = f x

mb_pure :: * -> maybe *
mb_pure = Just

mb_kbind :: (* -> maybe **) -> (** -> maybe ***) -> * -> maybe ***
mb_kbind f g = go where go x = f x $mb_bind g

mb_mapM :: (* -> maybe **) -> [*] -> maybe [**]
mb_mapM f = foldr (mb_liftA2 (:) . f) (mb_pure [])

mb_foldM :: (* -> ** -> maybe *) -> * -> [**] -> maybe *
mb_foldM f z0 xs
    = foldr c mb_pure xs z0
      where
        c x k z = f z x $mb_bind k


|| functor interface
mb_fmap :: (* -> **) -> maybe * -> maybe **
mb_fmap f ma = ma $mb_bind (mb_pure . f)


|| applicative interface
mb_apply :: maybe (* -> **) -> maybe * -> maybe **
mb_apply mf ma = mf $mb_bind converse mb_fmap ma

mb_liftA2 :: (* -> ** -> ***) -> maybe * -> maybe ** -> maybe ***
mb_liftA2 f ma mb = mb_fmap f ma $mb_apply mb


|| alternative interface
mb_alt :: maybe * -> maybe * -> maybe *
mb_alt Nothing  mr = mr
mb_alt (Just x) mr = Just x

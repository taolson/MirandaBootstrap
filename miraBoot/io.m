|| -*- mode: indented-text -*-
||
|| io.m -- an IO type to handle messages, exceptions, and values 


%export +

%include "avl"
%include "either"
%include "exception"
%include "grammar"
%include "lens"
%include "maybe"
%include "mirandaExtensions"
%include "name"


iot * ::= Msg sys_message | Excpt exception | Val *

|| An ios is a stream of iots, used to include any console output or file system commands along with
|| the function result or exception.  Nested functions using ios immediately return any sys_message
|| they receive so that the outermost top-level function can print them
ios * == [iot *]

|| convert an iot to a sys_message, using a function to convert any Vals to strings
toSysMsg :: (* -> string) -> iot * -> sys_message
toSysMsg f (Msg m)   = m
toSysMsg f (Val x)   = Stdout (f x)
toSysMsg f (Excpt e)
    = Stderr s, if isError e
    = Stdout s, otherwise
      where s = showException e ++ "\n"

|| monad interface for ios *
io_bind :: ios * -> (* -> ios **) -> ios **
io_bind ms f
    = go ms
      where
        go (Msg m   : ms') = Msg m   : go ms'   || report a Msg immediately and continue
        go [Excpt e]       = [Excpt e]          || terminate on a final exception
        go (Excpt e : ms') = Excpt e : go ms'   || report an Excpt that isn't final, and continue
        go (Val v   : ms') = f v                || bind a returned Val to the fn
        go []              = f undef            || an empty ios without a Val to bind to f -- bind an undef (which matches type)
                                                || this will be discarded if the fn f ignores it (if it is bound with io_right, e.g.)
io_pure :: * -> ios *
io_pure x = [Val x]

|| turn an (excpt *) into an (io *)
io_excpt :: excpt * -> ios *
io_excpt (Left errs)        = map Excpt errs
io_excpt (Right (r, warns)) = map Excpt warns ++ [Val r]

|| create a non-string message (e.g. Tofile)
io_ctrl :: sys_message -> ios *
io_ctrl m = [Msg m]

|| create a string message
io_msg  :: string -> ios *
io_msg s = [Msg (Stdout (s ++ "\n"))]

io_msgc :: string -> ios *
io_msgc s = [Msg (Stdout s)]

io_kbind :: (* -> ios **) -> (** -> ios ***) -> * -> ios ***
io_kbind f g = go where go x = f x $io_bind g

io_mapM :: (* -> ios **) -> [*] -> ios [**]
io_mapM f = foldr (io_liftA2 (:) . f) (io_pure [])

|| note: io_mapM_ must return the type ios *, because it will eventually be passed to "toSysMsg".
|| to facilitate this, the base result of the foldr is an Excpt (Note, Info ""), which
|| will match the type ios *
io_mapM_ :: (* -> ios **) -> [*] -> ios *
io_mapM_ f = foldr (io_right . f) [Excpt (Note, Info "", emptyLocInfo)]

io_foldM :: (* -> ** -> ios *) -> * -> [**] -> ios *
io_foldM f z0 xs
    = foldr c io_pure xs z0
      where
        c x k z = f z x $io_bind k


|| functor interface
io_fmap :: (* -> **) -> ios * -> ios **
io_fmap f ms = ms $io_bind (io_pure . f)


|| applicative interface
io_apply :: ios (* -> **) -> ios * -> ios **
io_apply mf ma = mf $io_bind converse io_fmap ma

io_liftA2 :: (* -> ** -> ***) -> ios * ->  ios ** -> ios ***
io_liftA2 f ma mb = io_fmap f ma $io_apply mb

io_left  = io_liftA2 const
io_right = io_liftA2 (converse const)

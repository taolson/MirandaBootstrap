|| -*- mode: indented-text -*-
||
|| serialize.m -- conversions between data structures and binary character streams


%export +

%include "avl"
%include "bag"
%include "either"
%include "exception"
%include "grammar"
%include "map"
%include "maybe"
%include "mirandaExtensions"
%include "module"
%include "lens"
%include "name"
%include "set"
%include "state"


|| serialS is a difference list, which takes a string to append to the right to produce the final serialized string.
|| this allows the final stream to be built from function composes, which can be traversed faster than a chain of (++)
|| which are left-associated
serialS     == [char] -> [char]

|| deserializer for type *
deserializer * == state [char] *


|| basic type serialization

|| used to encode positive numbers < 96, e.g. ctor encodings
serialCtor :: num -> serialS
serialCtor n = (decode (n + 32) :) 

deserialCtor :: deserializer num
deserialCtor (c : cs) = (code c - 32, cs)

serialChar :: char -> serialS
serialChar c = (c :)

deserialChar :: deserializer char
deserialChar (c : cs) = (c, cs)

|| serialize a positive integer
serialPosInt :: num -> serialS
serialPosInt n
    = go n 0 ""
      where
        go 0 sz dig = serialCtor sz . (dig ++)
        go n sz dig = go ((div) n 10) (sz + 1) (decode ((mod) n 10 + code '0') : dig)

|| serialize a signed integer
serialInt :: num -> serialS
serialInt n
    = serialCtor 1 . serialPosInt (neg n), if n < 0
    = serialCtor 0 . serialPosInt n,       otherwise

deserialPosInt :: deserializer num
deserialPosInt
    = deserialCtor $st_bind (st_fmap numval . splitAt)

deserialInt :: deserializer num
deserialInt
    = st_liftA2 mkNum deserialCtor deserialPosInt
      where
        mkNum 0 n = n
        mkNum 1 n = neg n

serialString :: string -> serialS
serialString s = serialPosInt (#s) . (s ++)

deserialString :: deserializer string
deserialString
    = deserialPosInt $st_bind splitAt

serialBool :: bool -> serialS
serialBool False = serialCtor 0
serialBool True  = serialCtor 1

deserialBool :: deserializer bool
deserialBool
    = st_fmap encBool deserialCtor
      where
        encBool 0 = False
        encBool 1 = True

serialTup2 :: (* -> serialS) -> (** -> serialS) -> (*, **) -> serialS
serialTup2 sa sb (a, b) = sa a . sb b

deserialTup2 :: deserializer * -> deserializer ** -> deserializer (*, **)
deserialTup2 = st_liftA2 pair

serialTup3 :: (* -> serialS) -> (** -> serialS) -> (*** -> serialS) -> (*, **, ***) -> serialS
serialTup3 sa sb sc (a, b, c) = sa a . sb b . sc c

deserialTup3 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer (*, **, ***)
deserialTup3 = st_liftA3 triple

serialTup4 :: (* -> serialS) -> (** -> serialS) -> (*** -> serialS) -> (**** -> serialS) -> (*, **, ***, ****) -> serialS
serialTup4 sa sb sc sd (a, b, c, d) = sa a . sb b . sc c . sd d

deserialTup4 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer (*, **, ***, ****)
deserialTup4 = st_liftA4 mkTup4 where mkTup4 a b c d = (a, b, c, d)

serialTup5 :: (* -> serialS) -> (** -> serialS) -> (*** -> serialS) -> (**** -> serialS) -> (***** -> serialS) -> (*, **, ***, ****, *****) -> serialS
serialTup5 sa sb sc sd se (a, b, c, d, e) = sa a . sb b . sc c . sd d . se e

deserialTup5 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer ***** -> deserializer (*, **, ***, ****, *****)
deserialTup5 = st_liftA5 mkTup5 where mkTup5 a b c d e = (a, b, c, d, e)

serialTup6 :: (* -> serialS) -> (** -> serialS) -> (*** -> serialS) -> (**** -> serialS) -> (***** -> serialS) -> (****** -> serialS) ->
              (*, **, ***, ****, *****, ******) -> serialS
serialTup6 sa sb sc sd se sf (a, b, c, d, e, f) = sa a . sb b . sc c . sd d . se e . sf f

deserialTup6 :: deserializer * -> deserializer ** -> deserializer *** -> deserializer **** -> deserializer ***** -> deserializer ****** ->
                deserializer (*, **, ***, ****, *****, ******)
deserialTup6 = st_liftA6 mkTup6 where mkTup6 a b c d e f = (a, b, c, d, e, f)

serialList :: (* -> serialS) -> [*]  -> serialS
serialList sx xs = serialPosInt (#xs) . converse (foldr sx) xs

deserialList :: deserializer * -> deserializer [*]
deserialList ds
    = deserialPosInt $st_bind d_count
      where
        d_count n = st_pure [],                         if n <= 0
                  = st_liftA2 (:) ds (d_count (n - 1)), otherwise

serialSet :: (* -> serialS) -> s_set * -> serialS
serialSet ss = serialList ss . s_toList

deserialSet :: deserializer * -> deserializer (s_set *)
deserialSet ds = deserialList ds $st_bind (st_pure . s_fromList)

serialBag :: (* -> serialS) -> b_bag * -> serialS
serialBag sb = serialList (serialTup2 sb serialPosInt) . b_toList

deserialBag :: deserializer * -> deserializer (b_bag *)
deserialBag db = deserialList (deserialTup2 db deserialPosInt) $st_bind (st_pure . b_fromCountList)

serialEither :: (* -> serialS) -> (** -> serialS) -> either * ** -> serialS
serialEither sl sr (Left x)  = serialCtor 0 . sl x
serialEither sl sr (Right x) = serialCtor 1 . sr x

deserialEither :: deserializer * -> deserializer ** -> deserializer (either * **)
deserialEither dl dr
    = deserialCtor $st_bind dse
      where
        dse 0 = st_fmap Left  dl
        dse 1 = st_fmap Right dr


|| AST serialization

serialCaseSel :: caseSel -> serialS
serialCaseSel Cnone     = serialCtor 0
serialCaseSel Csingle   = serialCtor 1
serialCaseSel Cpriority = serialCtor 2
serialCaseSel Cvectored = serialCtor 3

deserialCaseSel :: deserializer caseSel
deserialCaseSel 
    = ctor $st_fmap deserialCtor
      where
        ctor 0 = Cnone
        ctor 1 = Csingle
        ctor 2 = Cpriority
        ctor 3 = Cvectored

serialMultSel :: multSel -> serialS
serialMultSel Mfun = serialCtor 0
serialMultSel Mpat = serialCtor 1

deserialMultSel :: deserializer multSel
deserialMultSel 
    = ctor $st_fmap deserialCtor
      where
        ctor 0 = Mfun
        ctor 1 = Mpat

serialLoc :: location -> serialS
serialLoc = serialTup3 serialString serialPosInt serialPosInt

deserialLoc :: deserializer location
deserialLoc = deserialTup3 deserialString deserialPosInt deserialPosInt

serialAnno :: anno -> serialS
serialAnno = serialTup6 serialLoc serialBool serialPosInt serialPosInt (serialSet serialName) (serialSet serialCtor)

deserialAnno :: deserializer anno
deserialAnno = deserialTup6 deserialLoc deserialBool deserialPosInt deserialPosInt (deserialSet deserialName) (deserialSet deserialCtor)

serialName :: name -> serialS
serialName (Unqualified n)  = serialCtor 0 . serialString n
serialName (Unresolved m n) = serialCtor 1 . serialString m . serialString n
serialName (Qualified m n)  = serialCtor 2 . serialString m . serialString n
serialName (Private m n)    = serialCtor 3 . serialString m . serialString n
serialName (Builtin n)      = serialCtor 4 . serialString n
serialName (Internal n d)   = serialCtor 5 . serialString n  . serialPosInt d

deserialName :: deserializer name
deserialName
    = deserialCtor $st_bind ctor
      where
        ctor 0 = st_fmap   Unqualified deserialString
        ctor 1 = st_liftA2 Unresolved  deserialString deserialString
        ctor 2 = st_liftA2 Qualified   deserialString deserialString
        ctor 3 = st_liftA2 Private     deserialString deserialString
        ctor 4 = st_fmap   Builtin     deserialString
        ctor 5 = st_liftA2 Internal    deserialString deserialPosInt

serialDefnMap :: defnMap -> serialS
serialDefnMap = serialList (serialTup2 serialName serialAst) . m_toList

deserialDefnMap :: deserializer defnMap
deserialDefnMap = st_fmap m_fromList (deserialList (deserialTup2 deserialName deserialAst))

serialNameMap :: nameMap -> serialS
serialNameMap = serialList (serialTup2 serialName serialName) . m_toList

deserialNameMap :: deserializer nameMap
deserialNameMap = st_fmap m_fromList (deserialList (deserialTup2 deserialName deserialName))

serialAst :: ast -> serialS
serialAst (Evar n)            = serialCtor  0 . serialName n
serialAst (Eint n)            = serialCtor  1 . serialInt n
serialAst (Echar c)           = serialCtor  2 . serialChar c
serialAst (Estring s)         = serialCtor  3 . serialString s
serialAst (Eap e1 e2)         = serialCtor  4 . serialAst e1 . serialAst e2
serialAst (Ecall e args)      = serialCtor  5 . serialAst e . serialList serialAst args
serialAst (Econd conds)       = serialCtor  6 . serialList serialAst conds
serialAst (Ecase s e alts)    = serialCtor  7 . serialCaseSel s . serialAst e . serialList serialAst alts
serialAst (Efail e)           = serialCtor  8 . serialAst e
serialAst (Efatbar e1 e2 e3)  = serialCtor  9 . serialAst e1 . serialAst e2 . serialAst e3
serialAst (Elet r ds ps e)    = serialCtor 10 . serialBool r . serialDefnMap ds . serialList serialAst ps . serialAst e
serialAst (ElistComp e qs an) = serialCtor 11 . serialAst e . serialList serialAst qs . serialAnno an
serialAst (Cif e1 e2)         = serialCtor 12 . serialAst e1 . serialAst e2
serialAst (Cdefault e)        = serialCtor 13 . serialAst e
serialAst (CaseAlt p e)       = serialCtor 14 . serialAst p . serialAst e
serialAst (Qgen ps e)         = serialCtor 15 . serialList serialAst ps . serialAst e
serialAst (Qrecur p e1 e2)    = serialCtor 16 . serialAst p . serialAst e1 . serialAst e2
serialAst (Qguard e)          = serialCtor 17 . serialAst e
serialAst (Pvar n)            = serialCtor 18 . serialName n
serialAst (Plit e)            = serialCtor 19 . serialAst e
serialAst (Pctor n ps)        = serialCtor 20 . serialName n . serialList serialAst ps
serialAst Pwildcard           = serialCtor 21
serialAst (Tname n tes)       = serialCtor 22 . serialName n . serialList serialAst tes
serialAst (Tvar n)            = serialCtor 23 . serialPosInt  n
serialAst (Ttuple tes)        = serialCtor 24 . serialList serialAst tes
serialAst (Tlist te)          = serialCtor 25 . serialAst te
serialAst (Tarr t1 t2)        = serialCtor 26 . serialAst t1 . serialAst t2
serialAst (Tstrict te)        = serialCtor 27 . serialAst te
serialAst (DefFn n ps e an)   = serialCtor 28 . serialName n . serialList serialAst ps . serialAst e . serialAnno an
serialAst (DefMult m ds)      = serialCtor 29 . serialMultSel m . serialList serialAst ds
serialAst (DefPat p e an)     = serialCtor 30 . serialAst p . serialAst e . serialAnno an
serialAst (DefSyn tf te an)   = serialCtor 31 . serialAst tf . serialAst te . serialAnno an
serialAst (DefData tf tfs an) = serialCtor 32 . serialAst tf . serialList serialAst tfs . serialAnno an
serialAst (DefSpec tf te an)  = serialCtor 33 . serialAst tf . serialAst te . serialAnno an
serialAst (DefAbs tf ds t an) = serialCtor 34 . serialAst tf . serialList serialAst ds . serialAst t . serialAnno an
serialAst (EprimInt n)        = serialCtor 35 . serialInt n
serialAst (EprimChar c)       = serialCtor 36 . serialChar c
serialAst (EprimString s)     = serialCtor 37 . serialString s
serialAst a                   = error ("unhandled serialization of ast: " ++ showAst a)

deserialAst :: deserializer ast
deserialAst
    = deserialCtor $st_bind ctor
      where
        ctor  0 = st_fmap   Evar         deserialName
        ctor  1 = st_fmap   Eint         deserialInt
        ctor  2 = st_fmap   Echar        deserialChar
        ctor  3 = st_fmap   Estring      deserialString
        ctor  4 = st_liftA2 Eap          deserialAst                deserialAst
        ctor  5 = st_liftA2 Ecall        deserialAst                (deserialList deserialAst)
        ctor  6 = st_fmap   Econd        (deserialList deserialAst)
        ctor  7 = st_liftA3 Ecase        deserialCaseSel            deserialAst                 (deserialList deserialAst)
        ctor  8 = st_fmap   Efail        deserialAst
        ctor  9 = st_liftA3 Efatbar      deserialAst                deserialAst                 deserialAst
        ctor 10 = st_liftA4 Elet         deserialBool               deserialDefnMap             (deserialList deserialAst) deserialAst
        ctor 11 = st_liftA3 ElistComp    deserialAst                (deserialList deserialAst)  deserialAnno
        ctor 12 = st_liftA2 Cif          deserialAst                deserialAst
        ctor 13 = st_fmap   Cdefault     deserialAst
        ctor 14 = st_liftA2 CaseAlt      deserialAst                deserialAst
        ctor 15 = st_liftA2 Qgen         (deserialList deserialAst) deserialAst
        ctor 16 = st_liftA3 Qrecur       deserialAst                deserialAst                 deserialAst
        ctor 17 = st_fmap   Qguard       deserialAst
        ctor 18 = st_fmap   Pvar         deserialName
        ctor 19 = st_fmap   Plit         deserialAst
        ctor 20 = st_liftA2 Pctor        deserialName               (deserialList deserialAst)
        ctor 21 = st_pure   Pwildcard
        ctor 22 = st_liftA2 Tname        deserialName               (deserialList deserialAst)
        ctor 23 = st_fmap   Tvar         deserialPosInt
        ctor 24 = st_fmap   Ttuple       (deserialList deserialAst)
        ctor 25 = st_fmap   Tlist        deserialAst
        ctor 26 = st_liftA2 Tarr         deserialAst                deserialAst
        ctor 27 = st_fmap   Tstrict      deserialAst
        ctor 28 = st_liftA4 DefFn        deserialName               (deserialList deserialAst)  deserialAst               deserialAnno
        ctor 29 = st_liftA2 DefMult      deserialMultSel            (deserialList deserialAst)
        ctor 30 = st_liftA3 DefPat       deserialAst                deserialAst                 deserialAnno
        ctor 31 = st_liftA3 DefSyn       deserialAst                deserialAst                 deserialAnno
        ctor 32 = st_liftA3 DefData      deserialAst                (deserialList deserialAst)  deserialAnno
        ctor 33 = st_liftA3 DefSpec      deserialAst                deserialAst                 deserialAnno
        ctor 34 = st_liftA3 DefAbs       deserialAst                (deserialList deserialAst)  deserialAst     $st_apply deserialAnno
        ctor 35 = st_fmap   EprimInt     deserialInt
        ctor 36 = st_fmap   EprimChar    deserialChar
        ctor 37 = st_fmap   EprimString  deserialString

|| module serialization

serialFileSpec :: fileSpec -> serialS
serialFileSpec (FileSpec ps n) = serialList serialString ps . serialString n

deserialFileSpec :: deserializer fileSpec
deserialFileSpec = st_liftA2 FileSpec (deserialList deserialString) deserialString

serialLibQual :: libQual -> serialS
serialLibQual LibUnqualified = serialCtor 0
serialLibQual LibQualified   = serialCtor 1

deserialLibQual :: deserializer libQual
deserialLibQual
    = deserialCtor $st_bind ctor
      where
        ctor 0 = st_pure LibUnqualified
        ctor 1 = st_pure LibQualified

serialLibAs :: libAs -> serialS
serialLibAs LibNoAs   = serialCtor 0
serialLibAs (LibAs s) = serialCtor 1 . serialString s

deserialLibAs :: deserializer libAs
deserialLibAs
    = deserialCtor $st_bind ctor
      where
        ctor 0 = st_pure LibNoAs
        ctor 1 = LibAs $st_fmap deserialString

serialLibPart :: libPart -> serialS
serialLibPart (LibIdent n l)  = serialCtor 0 . serialName n     . serialLoc l
serialLibPart (LibRemove n l) = serialCtor 1 . serialName n     . serialLoc l
serialLibPart (LibFile f l)   = serialCtor 2 . serialFileSpec f . serialLoc l
serialLibPart (LibAll l)      = serialCtor 3 . serialLoc l              

deserialLibPart :: deserializer libPart
deserialLibPart
    = deserialCtor $st_bind ctor
      where
        ctor 0 = st_liftA2 LibIdent     deserialName     deserialLoc
        ctor 1 = st_liftA2 LibRemove    deserialName     deserialLoc
        ctor 2 = st_liftA2 LibFile      deserialFileSpec deserialLoc
        ctor 3 = st_fmap   LibAll       deserialLoc

serialLibAlias :: libAlias -> serialS
serialLibAlias (Alias n1 n2)   = serialCtor 0 . serialName n1 . serialName n2
serialLibAlias (AliasRemove n) = serialCtor 1 . serialName n

deserialLibAlias :: deserializer libAlias
deserialLibAlias
    = deserialCtor $st_bind dla
      where
        dla 0 = st_liftA2 Alias deserialName deserialName
        dla 1 = st_fmap AliasRemove deserialName

serialLibBinding :: libBinding -> serialS
serialLibBinding (BindVar n e)   = serialCtor 0 . serialName n . serialAst e
serialLibBinding (BindType n te) = serialCtor 1 . serialName n . serialAst te

deserialLibBinding :: deserializer libBinding
deserialLibBinding
    = deserialCtor $st_bind dlb
      where
        dlb 0 = st_liftA2 BindVar  deserialName deserialAst
        dlb 1 = st_liftA2 BindType deserialName deserialAst

serialLibEnv :: libEnv -> serialS
serialLibEnv = serialTup6 serialFileSpec serialLoc serialLibQual serialLibAs (serialList serialLibBinding) (serialList serialLibAlias)

deserialLibEnv :: deserializer libEnv
deserialLibEnv = deserialTup6 deserialFileSpec deserialLoc deserialLibQual deserialLibAs (deserialList deserialLibBinding) (deserialList deserialLibAlias)

serialBuildStatus :: buildStatus -> serialS
serialBuildStatus Unbuilt    = serialCtor 0
serialBuildStatus Built      = serialCtor 1
serialBuildStatus Serialized = serialCtor 2

deserialBuildStatus :: deserializer buildStatus
deserialBuildStatus
    = mkbs $st_fmap deserialCtor
      where
        mkbs 0 = Unbuilt
        mkbs 1 = Built
        mkbs 2 = Serialized

serialModule :: module -> [char]
serialModule (Module fs bs exs ins defs pats specs env)
    = ( serialFileSpec fs
      . serialBuildStatus Serialized
      . serialEither (serialList serialLibPart) serialNameMap exs
      . serialList serialLibEnv ins
      . serialDefnMap defs
      . serialList serialAst pats
      . serialDefnMap specs
      . serialNameMap env
      ) ""

deserialModule :: deserializer module
deserialModule = ((st_liftA6 Module
                     deserialFileSpec
                     deserialBuildStatus
                     (deserialEither (deserialList deserialLibPart) deserialNameMap)
                     (deserialList deserialLibEnv)
                     deserialDefnMap
                     (deserialList deserialAst))  $st_apply
                     deserialDefnMap)             $st_apply
                     deserialNameMap

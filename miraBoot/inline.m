|| -*- mode: indented-text -*-
||
|| inline.m -- inlining saturated calls to known non-recursive functions and simplification transformations
||
|| todo: currently the analysis info (free vars, uses, complexity) is recomputed after each inlining pass for the function group being
||       inlined.  This might be able to be changed to updating those vars during the inline pass, rather than re-performing the
||       analysis each time.  Performace doesn't seem to bad right now, though.
||
|| re-calculation of fuel when eliminating dead code seems wrong -- fuel can actually increase above the original limit
||
|| look into idea of join points -- let-bound tail calls of fully-saturated functions, and tag them as such (in analyzer?)
|| when generating code for a join point, let does no allocation, and the call is just an "adjust stack pointer and jump"
|| Paper: "Compiling Without Continuations"


|| configuration
|| inlining limits and verbose control. Defaults determined through a series of performance runs,
|| comparing various settings.  Most effect is maxInlineFnCpx, which drops in performance when
|| much lower or higher than 150.
verbose        = False  || add extra info in the output on inlining
streamFusion   = False  || enable stream fusion optimizations, should be set the same as streamFusion config in mirac.m
useSel         = False  || must match other configurations of useSel; use builtinSel for selecting a value from a sum type
maxInlineIters = 15     || maximum number of iterations of the inliner per function group
maxInlineCpx   = 1500   || maximum total additional complexity inlined per function group
maxInlineFnCpx = 150    || maximum complexity of a function for inlining
maxCaseDup     = 16     || maximum number of CaseAlt duplications to allow (4 * 4)
noteLimit      = 100    || maximum length of an added note to show when tracing

|| functions which don't return (call error and terminate)
noReturnNames = map (Qualified "stdlib") ["exit", "error", "blackHole", "undef", "caseFail", "matchFail", "cmpFn"]

|| functions we shouldn't inline
isIOfn :: name -> bool
isIOfn n = getModName n == "io"

noInlineNames :: [name]
noInlineNames = map (Qualified "stdlib") ["trace", "cmpTags"]                          ++
                map (Qualified "stream") ["toStream", "fromStream", "readByteStreamS"] ++
                noReturnNames

|| builtin functions that can be inlined by computing at compile-time
inlineBuiltinNames = ["cmp#", "+#", "-#", "*#", "div#", "mod#"]

%export inline streamSubst

%include "analyze"
%include "ast"
%include "avl"
%include "dependency"
%include "either"
%include "exception"
%include "grammar"
%include "lens"
%include "map"
%include "maybe"
%include "mirandaExtensions"
%include "module"
%include "name"
%include "predef"
%include "rename"
%include "set"
%include "state"


|| a localEnv consists of a stack of frames, which are either a function boundary marker
|| or a defnMap.  The function boundary marker is used to indicate whether we have crossed
|| a function definition boundary when performing a lookup, to later determine viability
|| of inlining the looked-up name (don't inline complex exprs if they get moved into a fnDefn that
|| could be executed multiple times)
frame    ::= FnBound | Env defnMap
localEnv == [frame]

|| state passed through inline
|| dynamic state:
|| genv:       global environment
|| subst:      name substitution map (for substituting stream versions of list functions)
|| fuel:       complexity remaining allotted to subsequent inlining
|| simplified: bool indicating that simplification was performed (controls iteration on inlining and simplify)
|| errs:       exceptions (notes)
||
|| lexically-scoped state:
|| lenv: local lexical environment
|| loc:  locInfo
inDynSt  == (globalEnv, nameMap, num, bool, [exception])
inLexSt  == (locInfo, localEnv)
inSt     == (inDynSt, inLexSt)

|| lenses for inSt fields
inDyn        = lensFst
inLex        = lensSnd
inGenv       = composeLens inDyn lensTup5_0
inSubst      = composeLens inDyn lensTup5_1
inFuel       = composeLens inDyn lensTup5_2
inSimplified = composeLens inDyn lensTup5_3
inErrs       = composeLens inDyn lensTup5_4
inLoc        = composeLens inLex lensFst
inLenv       = composeLens inLex lensSnd
inLocation   = composeLens inLoc locInfoLocation
inDefnPath   = composeLens inLoc locInfoDefnPath

initInSt :: globalEnv -> nameMap -> inSt
initInSt genv subst = ((genv, subst, maxInlineCpx, False, []), (emptyLocInfo, []))

|| add a note to the inline state, limiting the length of the note string
addNote :: string -> inSt -> inSt
addNote s st
    = over inErrs ((Note, Info s', emptyLocInfo) :) st, if verbose
    = st,                                               otherwise
      where
        s' = limitInfo noteLimit s

|| lookup the defn bound to a name in the local or global environments, along with
|| an indication of whether the lookup crossed a function definition boundary
lookupEnv :: name -> inSt -> (maybe defn, bool)
lookupEnv n st
    = lookFrame (view inLenv st) False, if m_null (view inSubst st)
    = lookStream,                       otherwise
      where

        lookFrame []             cf = (lookupDefn (view inGenv st) n, cf)       || name not found in Lenv, check globalEnv
        lookFrame (FnBound : fs) cf = lookFrame fs True                         || crossed a DefFn boundary
        lookFrame (Env e : fs)   cf
            = (md, cf),        if isJust md
            = lookFrame fs cf, otherwise
              where
                md = m_lookup n e

        || only allow functions from the stream module to be inlined
        lookStream
            = (md,      False), if getModName n == strmModName
            = (Nothing, False), otherwise
              where
                md = lookupDefn (view inGenv st) n

|| rewrite an expr, directly replacing var references with the corresponding value of the binding
replace :: expr -> [(name, expr)] -> expr
replace e bdgs
    = e,  if null bdgs
    = e', otherwise
      where
        env                 = m_fromList bdgs
        e'                  = st_evalState (astRewriteTD replaceAst e) ()
        replaceAst (Evar n) = st_pure (m_findWithDefault (Evar n) n env)
        replaceAst a        = st_pure a

|| rewrite only case scrutinees in an expr, replacing var references with the corresponding value of the binding
replaceScrutinee :: expr -> [(name, expr)] -> expr
replaceScrutinee e bdgs
    = e,  if null bdgs
    = e', otherwise
      where
        e' = st_evalState (astRewriteTD replaceAst e) ()
        replaceAst (Ecase b e alts) = st_pure (Ecase b (replace e bdgs) alts)
        replaceAst a                = st_pure a

|| wrap an expr in a sequence of individual let bindings created from a list of names and exprs
wrapInLet :: location -> expr -> [(name, expr)] -> expr
wrapInLet loc e bdgs
    = foldr wrap e bdgs
      where
        an = set anUses 1 (locAnno loc)

        wrap (n, e') e
            = Elet False (m_singleton n d) [] e
              where
                d = DefPat (Pvar n) e' an

|| create a binding of args to vars, directly rewriting simpleExpr args,
|| and wrapping the rest in a let binding
createBinding :: location -> expr -> [pat] -> [expr] -> expr
createBinding loc e vars args
    = wrapInLet loc (replace e smp) cpx
      where
        (smp, cpx) = (partition (isSimpleExpr . snd) . map (mapFst unPvar) . filter (isPvar . fst) . zip2 vars) args

|| perform beta-reduction on a defn with arguments, renaming all the Pvars in the defn first
|| to prevent name capture
betaReduce :: defn -> [expr] -> state inSt expr
betaReduce d args st
    = (e2, st')
      where
        loc            = fromMaybe emptyLocation (view inLocation st)
        (d', genv')    = renameExpr "`" d (view inGenv st)      || rename inlined defn, prefixing included defns to hide them in reports
        st'            = set inGenv genv' st
        vars           = fnVars d'
        e              = fnExpr d'
        (args1, args2) = splitAt (#vars) args
        e1             = createBinding loc e vars args1
        e2             = e1,               if null args2
                       = (Ecall e1 args2), otherwise

canInline :: name -> inSt -> bool
canInline n st = ~(isBuiltin n \/ isConstructor n \/ isIOfn n \/ member noInlineNames n)

|| expr isSimpleExpr or a simple constructor
isSimple :: expr -> bool
isSimple (Ecall (Evar c) args) = isConstructor c & all isSimpleExpr args
isSimple e                     = isSimpleExpr e

|| check if expr can return a value, or if it terminates
|| (used when rewriting nested case exprs, to prevent expansion of a non-returning expr)
noReturn :: expr -> inSt -> bool
noReturn (Evar n) st
    = member noReturnNames n \/ fromMaybef False checkThunk (fst (lookupEnv n st))
      where
        checkThunk (DefPat p e an) = noReturn e st
        checkThunk m               = False

noReturn (Ecall e args) st = noReturn e st
noReturn e              st = False


inlineAst :: astRewriteTraverser inSt

|| early out of the rest of inlining, if out of fuel
inlineAst k e st = (e, st), if view inFuel st <= 0

|| do inlining on the defs of an Elet, then add that as a new lexical environment for inlining the expr of the Elet
inlineAst k (Elet rec defs x e) st
    = (Elet rec defs' x e', st2)
      where
        (defs', st1) = astRewriteMap (astRewriteLex inlineAst) defs st                  || do inlining on all the defns, separately
        (e', st2)    = astRewriteLex inlineAst e (over inLenv (Env defs' :) st1)        || then do inlining on the let expr, with the new defns

|| inline builtinSel on constant ctor call
inlineAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [Ecall (Evar c) args]) st
    = k (args ! idx) st', if useSel & isConstructor c
      where
        note = "inlined builtinSel on constant ctor"
        st' = (addNote note . set inSimplified True) st

|| inline builtinSel on var which is bound to a ctor call
|| note: this could be a sel for a data item which should be returned, or it could be a sel on the first
|| argument of a call, with the selector selecting a fn that gets passed the remaining arguments of the call
|| prime example is Lens (getf/overf) functions
inlineAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) (Evar n : args)) st
    = k selArg              st', if useSel & inlinable & null args
    = k (Ecall selArg args) st', if useSel & inlinable
      where
        inlinable     = isJust md & isDefPat d & isJust margs'
        note          = ("inlined builtinSel on " ++ showName n)
        st'           = (addNote note . set inSimplified True) st
        md            = fst (lookupEnv n st)
        d             = fromJust md
        DefPat p e an = d
        margs'        = ctorCallArgs e
        args'         = fromJust margs'
        selArg        = args' ! idx

        ctorCallArgs (Ecall (Evar c) args) = Just args, if isConstructor c
        ctorCallArgs e                     = Nothing

|| compute some builtin functions on literal unboxed int values -- useful for, e.g. determining
|| rangeByFromTo loop direction at compile time, or removing dead code during specialization of
|| case expressions
inlineAst k (Ecall (Evar (Builtin fn)) [(EprimInt a), (EprimInt b)]) st
    = (EprimInt r, st'), if member inlineBuiltinNames fn
      where
        st' = (addNote ("inlined builtin " ++ fn) . set inSimplified True) st
        r   = doBuiltin fn a b

        doBuiltin "+#"   a b = a + b
        doBuiltin "-#"   a b = a - b
        doBuiltin "*#"   a b = a * b
        doBuiltin "div#" a b = a div b
        doBuiltin "mod#" a b = a mod b
        doBuiltin "cmp#" a b = 0, if a == b
                             = 1, if a < b
                             = 2, otherwise

|| inline builtin cmp# on literal unboxed char values
inlineAst k (Ecall (Evar (Builtin "cmp#")) [(EprimChar a), (EprimChar b)]) st
    = (EprimInt r, st')
      where
        st' = (addNote "inlined builtin cmp" . set inSimplified True) st
        r   = 0, if a == b
            = 1, if a < b
            = 2, otherwise

|| collapse toStream/fromStream pairs
|| note: the fromStream may be the final expr in a chain of Elets, so we need to walk
|| the chain to check the final expr
inlineAst k (Ecall (Evar ts) [e]) st
    = k e' st', if ts == toStreamName & isJust me'
      where
        me'            = walkLets e
        e'             = fromJust me'
        toStreamName   = Qualified "stream" "toStream"
        fromStreamName = Qualified "stream" "fromStream"
        note           = "collapsed toStream/fromStream"
        st'            = (addNote note . set inSimplified True) st

        walkLets (Elet rec defs x e)   = Elet rec defs x $mb_fmap walkLets e
        walkLets (Ecall (Evar fs) [e]) = Just e, if fs == fromStreamName
        walkLets e                     = Nothing

|| substitute stream versions of list functions
inlineAst k (Ecall (Evar n) args) st
    = k (Ecall (Evar n') args) st', if isJust mn'
      where
        mn'  = m_lookup n (view inSubst st)
        n'   = fromJust mn'
        note = "substituted stream version of " ++ showName n
        st'  = (addNote note . set inSimplified True) st

|| inline the function body of a call
|| if inlineable, return without continuing through the new inlined body, to allow more inlining
|| of other functions at the same "level", first.  If enough fuel remains, another pass will continue
|| inlining through the new inlined body
inlineAst k (Ecall (Evar n) args) st
    = (e, st2), if inlinable
      where
        inlinable = canInline n st & isJust md & fnArity d <= #args & ~view defnAnRec d & cpx <= maxInlineFnCpx
        md        = fst (lookupEnv n st)
        d         = fromJust md
        cpx       = view defnAnCpx d
        (e, st1)  = betaReduce d args st
        note      = "inlined function " ++ showName n
        st2       = (addNote note . over inFuel (subtract cpx) . set inSimplified True) st1

|| do inlining of an expr in a case scrutinee
|| if scrutinee is an unbound var, bind its name to each alt pattern type in the alts (to specialize its value in each alt)
|| Note that in some cases, the binding of a var to an alt pattern will cause extra work of building a new ctor, instead of using the
|| exisiting var.  However, it overall is a performance improvement, due to the subsequent optimizations the specialization provides.
|| Note: since we are explicitly evaluating the expr, we can inline complex exprs that have a single use, since we know
|| it won't be passed to some other function which might replicate its use
inlineAst k (Ecase b (Evar n) alts) st
    = k (Ecase b e'       alts)  st2, if inlinable
    = k (Ecase b (Evar n) alts') st3, otherwise
      where
        inlinable    = canInline n st & isJust md & ~view defnAnRec d & isDefPat d & (isSimple e \/ singleCpx) & cpx <= maxInlineFnCpx
        (md, cf)     = lookupEnv n st
        d            = fromJust md
        uses         = view defnAnUses d
        cpx          = view defnAnCpx d
        singleCpx    = uses == 1 & ~cf         || expr is only used once (here), and doesn't cross a DefFn boundary
        e            = fnExpr d
        (e', st1)    = betaReduce d [] st
        note         = "inlined case scrutinee " ++ showName n
        st2          = (addNote note . over inFuel (subtract cpx) . set inSimplified True) st1
        (alts', st3) = st_mapM bindInAlt alts st

        bindInAlt alt st
            = (alt, st),                                              if isNothing me
            = (repFor e alt [(n, e)], over inFuel (subtract cpx) st), otherwise
              where
                me       = mkExpr alt
                (e, cpx) = fromJust me

                repFor (Ecall v args)             = replaceScrutinee, if #args > 1      || limit replacement to scrutinees, if complex
                repFor e                          = replace                             || lits & simple ctors get replaced everywhere

                mkExpr (CaseAlt (Plit l) e)       = Just (l, 1)
                mkExpr (CaseAlt (Pctor c []) e)   = Nothing     || don't rebind nullary ctors, as they just rebind to themselves
                mkExpr (CaseAlt (Pctor c vars) e) = Just (Ecall (Evar c) (map (Evar . unPvar) vars), 2 + #vars), if all isPvar vars
                mkExpr alt                        = Nothing

|| do inlining of an expr as a caseAlt value
inlineAst k (CaseAlt p (Evar n)) st
    = k (CaseAlt p e') st2, if inlinable
      where
        inlinable     = canInline n st & isJust md & ~view defnAnRec d & isDefPat d & (isSimple e \/ singleCpx) & cpx <= maxInlineFnCpx
        (md, cf)      = lookupEnv n st
        d             = fromJust md
        uses          = view defnAnUses d
        cpx           = view defnAnCpx d
        singleCpx     = uses == 1 & ~cf
        e             = fnExpr d
        (e', st1)     = betaReduce d [] st
        note          = "inlined caseAlt expr " ++ showName n
        st2           = (addNote note . over inFuel (subtract cpx) . set inSimplified True) st1

|| do inlining of a simple expr
|| Note: this used to also include cases of single-use complex exprs, but it turns out that the use count from analyze isn't
|| enough to determine dynamic uses at runtime: a case happened where it statically looked like a single-use case, but in reality
|| was passed to a function which replicated it many times, resulting in an O(n^2) performance problem for an expensive thunk.
|| now we only inline simpleExprs
inlineAst k (Evar n) st
    = (e', st2),    if inlinable
    = (Evar n, st), otherwise
      where
        inlinable     = canInline n st & isJust md & isDefPat d & ~view defnAnRec d & isSimpleExpr e
        (md, cf)      = lookupEnv n st
        d             = fromJust md
        cpx           = view defnAnCpx d
        DefPat p e an = d
        (e', st1)     = betaReduce d [] st
        note          = "inlined expr " ++ showName n
        st2           = (addNote note . over inFuel (subtract cpx) . set inSimplified True) st1

|| add locInfo for defn, and a FnBound frame to the local env when entering a DefFn
inlineAst k a st
    = k a st2, if isDefn a
    = k a st,  otherwise
      where
        loc = defnLocInfo a
        st1 = over inLoc (mergeLocInfo loc) st
        st2 = over inLenv (FnBound :) st1, if isDefFn a
            = st1,                         otherwise


|| simplifyAst -- perform a series of simplification transformations on Ecase and Elet exprs that can
|| expose more opportunities for inlining (from the paper "Let-floating: moving bindings to give faster programs"
|| by Simon Peyton Jones
simplifyAst :: astRewriteTraverser inSt

|| don't do any simplification during stream substitution, or if out of fuel
simplifyAst k a st
    = (a, st), if ~m_null (view inSubst st) \/ view inFuel st <= 0

|| float a let into caseAlts, if unused in the case scrutinee
|| this will cause the lets to be specialized in each alt, allowing unused lets to be removed
|| (fewer runtime bindings created)
simplifyAst k (Elet rec defs x (Ecase b e alts)) st
    = k (Ecase b e alts') st', if ~used
      where
        cpx   = (sum . map (view defnAnCpx) . m_elems) defs
        used  = astAccumTD tallyUses e False
        alts' = map insLet alts
        note  = "float let into case alts"
        st'   = (addNote note . over inFuel (subtract (cpx * (#alts - 1))) . set inSimplified True) st

        || handle the degenerate case where the CaseAlt is an Evar ref for a defn in the let, by replacing it directly
        || this gets rid of extraneous thunk creation / updates
        insLet (CaseAlt p (Evar n))
            = CaseAlt p e', if isJust md & ~view defnAnRec d & isDefPat d
              where
                md = m_lookup n defs
                d  = fromJust md
                DefPat p' e' an' = d

        insLet (CaseAlt p e) = CaseAlt p (Elet rec defs x e)

        tallyUses (Evar n) used = used \/ m_member n defs
        tallyUses a        used = used

|| dead code elimination for unused definitions (that have been inlined), and rewrite of the Elet to its
|| expr if all defns are removed.  Note: this pattern comes after the previous simplification on an Elet, because
|| this Elet pattern is more general, and would make the previous unreachable
simplifyAst k (Elet b defs x e) st
    = (k (Elet b defs x e) $st_bind f) (over inLenv (Env defs :) st)
      where
        f (Elet rec defs pats e) st
            = (e, st'),                     if null ds
            = (Elet rec defs' pats e, st'), otherwise
              where
                (ds, dead) = partition (isUsed . snd) (m_toList defs)
                fuel'      = sum (map (view defnAnCpx . snd) dead)
                defs'      = m_fromList ds
                notes      = ("increased fuel by " ++ shownum fuel') : map (("eliminated " ++) .  showName . defnName . snd) dead
                st'        = st,                                               if null dead
                           = (over inFuel (fuel' +) . foldr addNote st) notes, otherwise
                isUsed d   = view defnAnUses d - selfRefUse d > 0

                || does a recursive defn still use itself, or has that been eliminated?
                || note: this is conservative: a recursive function could reference itself multiple times, but we will
                || return only 0 or 1 here, since we only know it is referenced via the defnAnFree set, rather than its count.
                selfRefUse d
                    = 1, if view defnAnRec d & s_member (defnName d) (view defnAnFree d)
                    = 0, otherwise


|| this is needed when a partially-applied Ecall gets inlined
simplifyAst k (Ecall (Ecall f args2) args1) st
    = k (Ecall f (args2 ++ args1)) st'
      where
        st' = (addNote "merged partially-applied Ecall" . set inSimplified True) st

|| simplify case exprs that don't return
simplifyAst k (Ecase b e alts) st
    = k e st', if noReturn e st
      where
        st' = (addNote "simplified no-return scrutinee" . set inSimplified True) st

|| swap order of an Ecase where the scrutinee is also an Ecase
|| note: this duplicates alts1 #alts2 times. Since we are duplicating alts1, and there
|| is a chance that we might float a let defined in one outside of it, we could run into
|| name collision problems.  To solve this, we rename each duplicate.
simplifyAst k (Ecase b1 (Ecase b2 tst alts2) alts1) st
    = k (Ecase b2 tst alts2') st2, if #alts1 * #alts2 <= maxCaseDup
      where
        cpx = analyzeExprCpx (Ecase b1 builtinFail alts1) (view inGenv st)    || template for added cost of pushing outer alts
        (alts2', st1) = st_mapM pushAlt alts2 st
        note          = "swapped order of case (case " ++ showAst tst ++ ")"
        st2           = (addNote note . over inFuel (subtract (cpx * (#alts2 - 1))) . set inSimplified True) st1

        || push a duplicate of alts1 into an alt2
        pushAlt (CaseAlt p e) st
            = (CaseAlt p e, st),                   if noReturn e st     || e doesn't return, so don't replace it
            = (CaseAlt p e', set inGenv genv' st), otherwise
              where
                (e', genv') = renameExpr "" (Ecase b1 e alts1) (view inGenv st)

|| swap order of a builtinSel where the scrutinee is an Ecase
simplifyAst k (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [Ecase b tst alts]) st
    = k (Ecase b tst (map pushSel alts)) st', if useSel
      where
        cpx  = analyzeExprCpx (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [builtinFail]) (view inGenv st) || template for added cost of pushing sel
        note = "swapped order of sel (case " ++ showAst tst ++ ")"
        st'  = (addNote note . over inFuel (subtract (cpx * (#alts - 1))) . set inSimplified True) st
        pushSel (CaseAlt p e) = (CaseAlt p (Ecall (Tname (Builtin "sel") [x, (EprimInt idx)]) [e]))

|| simplify an Ecase with a constant constructor scrutinee by replacing it with its corresponding caseAlt
|| this is the main simplification optimization which the others are trying to expose
simplifyAst k (Ecase b e alts) st
    = k e' st', if isJust ma
      where
        genv = view inGenv st
        loc  = fromMaybe emptyLocation (view inLocation st)
        ma   = (extrCtorArgs e $mb_bind matchCtor) $mb_alt (extrLit e $mb_bind matchLit)
        e'   = fromJust ma
        cpx1 = analyzeExprCpx (Ecase b e alts) genv
        cpx2 = analyzeExprCpx e' genv
        note = "simplified constant ctor scrutinee " ++ showAst e
        st'  = (addNote note . over inFuel (+ (cpx1 - cpx2)) . set inSimplified True) st

        extrCtorArgs (Evar c)              = Just (c, []),   if isConstructor c
        extrCtorArgs (Ecall (Evar c) args) = Just (c, args), if isConstructor c
        extrCtorArgs e                     = Nothing

        matchCtor (c, args)
            = foldr matchAlt Nothing alts
              where
                matchAlt (CaseAlt (Pctor c' vars) e) k
                    = Just (createBinding loc e vars args), if c' == c

                matchAlt (CaseAlt Pwildcard e) k = Just e

                matchAlt (CaseAlt (Pvar n) e) k
                    = Just (createBinding loc e [Pvar n] [ce])
                      where
                        ce = Evar c,                if null args
                           = (Ecall (Evar c) args), otherwise

                matchAlt a k = k

        extrLit (EprimInt n) = Just n
        extrLit e            = Nothing

        matchLit n
            = foldr matchAlt Nothing alts
              where
                matchAlt (CaseAlt (Plit (EprimInt n')) e) k = Just e, if n' == n
                matchAlt (CaseAlt Pwildcard e)            k = Just e
                matchAlt (CaseAlt (Pvar v) e)             k = Just (createBinding loc e [Pvar v] [EprimInt n])
                matchAlt a                                k = k

|| float a let outside of a case scrutinee
|| since the case evaluation is strict, the Elet bindings will always be evaluated, and this
|| exposes opportunities to match case evaluation of a constant or simple expr
simplifyAst k (Ecase b1 (Elet b2 defs x e) alts) st
    = k (Elet b2 defs x (Ecase b1 e alts)) st'
      where
        note = "float let out of case scrutinee: " ++ showAst (Elet b2 defs x e)
        st'  = (addNote note . set inSimplified True) st

|| float a case outside of an Ecall fn
|| exposes opportunities to inline the called fn, as it becomes specialized in each alt
simplifyAst k (Ecall (Ecase b e alts) args) st
    = k (Ecase b e alts') st'
      where
        alts'                 = map insCall alts
        insCall (CaseAlt p e) = CaseAlt p (Ecall e args)
        note                  = "float case out of Ecall fn"
        st'                   = (addNote note . set inSimplified True) st

|| float let outside of an Ecall fn
|| exposes opportunities to inline the called fn
simplifyAst k (Ecall (Elet b defs x f) args) st
    = k (Elet b defs x (Ecall f args)) st'
      where
        note = "float let out of Ecall fn: " ++ showAst (Elet b defs x f)
        st'  = (addNote note . set inSimplified True) st

|| remove FATBARs that no longer have the possibility of failing
simplifyAst k (Efatbar (Pvar fn) e1 e2) st
    = k e1 st', if fc == 0
      where
        fc  = astAccumTD countFails e1 0
        st' = (addNote "removed FATBAR" . set inSimplified True) st

        countFails (Efail (Evar fn')) n = n + 1, if fn' == fn
        countFails e n                  = n

simplifyAst k a st = k a st


dontInlineNames = map (Qualified "test") ["genScore", "bestGuess"]
dontInline n = False || member dontInlineNames n

|| do inlining on a group of possibly mutually-recursive definitions
|| first do a series of inlining passes, preventing the inlining of stream primitives, to allow subsequent recognition of them
|| then do a second series of inlining passes, allowing the inlining of stream primitives (and toStream/fromStream collapsing)
|| if the inlining finishes by running out of iterations or fuel, it likely would be too large to be beneficial, so it is discarded
|| and the code reverts to the un-inlined version
inlineGroup :: defnMap -> [name] -> state inSt defnMap
inlineGroup defs ns st
    = (m_union defs ndefs1, st), if any dontInline ns
    = (m_union defs ndefs2, st3), if ni < maxInlineIters & view inFuel st3 >= 0 || add inlined version if inlining was succesful
    = (m_union defs ndefs1, st3), otherwise                                     || revert to original if could only inline partial
      where
        st1               = addNote ("\n --- " ++ intercalate " " (map showName ns) ++ " ---") st
        ndefs1            = foldl addDef m_empty ns
        st2               = (set inLenv [Env defs] . set inFuel (maxInlineCpx * #ns) . set inSimplified True) st1
        (ndefs2, st3, ni) = (hd . dropWhile canInlineMore . iterate inlineDefs) (ndefs1, st2, 0)

        addDef m n
            = m_insert n (fromJust md) m, if isJust md
            = m,                          otherwise
              where
                md = fst (lookupEnv n st1) $mb_alt lookupDefn (view inGenv st) n

        canInlineMore (d, st, i)
            = i < maxInlineIters & view inSimplified st & view inFuel st > 0

        || note: instead of trying to track and adjust defnAnno values (cpx, free, uses) during
        || inlining and simplification, we just re-run analyze after inline and simplify, at least for now
        inlineDefs (ndefs, st, i)
            = (ndefs4, stLim, i),   if limited
            = (ndefs4, st3, i + 1), otherwise
              where
                (ndefs1, st1) = astRewriteMap (astRewriteLex inlineAst) ndefs (set inSimplified False st)
                ndefs2        = analyzeDefns ndefs1 (view inGenv st1)
                (ndefs3, st2) = astRewriteMap (astRewriteLex simplifyAst) ndefs2 st1
                ndefs4        = analyzeDefns ndefs3 (view inGenv st2)
                st3           = addNote ("iteration: " ++ shownum i ++ " fuel: " ++ shownum (view inFuel st2) ++ "\n") st2
                limited       = i >= maxInlineIters \/ view inFuel st2 <= 0
                stLim         = (set inSimplified False . addNote "*** maxInlineIters limit reached ***") st2, if i >= maxInlineIters
                              = (set inSimplified False . addNote "*** maxInlineCpx limit reached ***")   st2, otherwise

inline :: globalEnv -> excpt globalEnv
inline genv
    = ex_return genv' (reverse warns), if null errs
    = ex_errs errs,                    otherwise
      where
        m              = view genvMod genv
        sccs           = (findSCCs . makeDependencyGraph . makeDefnDependencyList . view modDefns) m
        (defs', st)    = st_foldM inlineGroup m_empty sccs (initInSt genv m_empty)
        genv'          = over genvMod (set modDefns defs') (view inGenv st)
        (errs, warns)  = partition isError (view inErrs st)


|| make the stream function substitution map, by finding all of the stream functions that end with a single quote ('),
|| and finding the corresponding names for the same functions without the quote
mkStreamSubstMap :: globalEnv -> nameMap
mkStreamSubstMap genv
    = m_empty, if isNothing msubns
    = subMap,  otherwise
      where
        msubns = (filter (isSubChar. last . getName) . m_keys . view modDefns) $mb_fmap lookupModule genv "stream"      || get the compatible substitution fns
        subns  = fromJust msubns
        mns    = map (lookupName genv . Unqualified . init . getName) subns                                             || get the names they will replace
        subMap = (m_fromList . map (mapFst fromJust) . filter (isJust . fst) . zip2 mns) subns                          || make the substitution name map

        isSubChar c = c == '_' \/ c == '@'

|| stream substitution and fusion
|| replace selected list functions with compatible stream versions, then use the inlining
|| mechanism (restricted to only stream functions) to further inline those operations and
|| fuse paired toStream/fromStream functions
streamSubst :: globalEnv -> excpt globalEnv
streamSubst genv
    = ex_return genv [],               if ~streamFusion
    = ex_return genv' (reverse warns), if null errs
    = ex_errs errs,                    otherwise
      where
        m             = view genvMod genv
        subst         = mkStreamSubstMap genv
        sccs          = (findSCCs . makeDependencyGraph . makeDefnDependencyList . view modDefns) m
        (defs', st)   = st_foldM inlineGroup m_empty sccs (initInSt genv subst)
        genv'         = over genvMod (set modDefns defs') (view inGenv st)
        (errs, warns) = partition isError (view inErrs st)

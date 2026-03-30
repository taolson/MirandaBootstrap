|| -*- mode: indented-text -*-
||
|| reify.m -- module environment reification of includes, exports, and expansion of type synonyms


%export reifyIncludes reifyDefns reifyDefAbs reifyExports reifySyns reifyTypes

%include "avl"
%include "ast"
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

|| insert a defn name into a nameMap, inserting all of the associated constructors if it is a data defn
insNameMapDefn :: nameMap -> name -> defn -> excpt nameMap
insNameMapDefn e n d
    = ex_foldM insDataName e ns, if isDefData d     || insert data type name and constructor names (implicitly Qualified)
    = insName e n (defnName d),  otherwise
      where
        loci = defnLocInfo d
        ns   = n : map tformName (view dataCtors d)

        insDataName e n = insName e n n

        insName e n n'
            = insNameMap loci (unresolveName n) n' e $ex_bind
              insNameMap loci (unqualifyName n) n'    || add both unresolved (qualified) and unqualified names

|| add included environments to the top-level environment, processing aliases and removes
reifyIncludes :: globalEnv -> excpt globalEnv
reifyIncludes genv
    = updateGenv $ex_fmap ex_foldM reifyInclude m (view modIncludes m)
      where
        m   = view genvMod genv
        mm  = view genvModMap genv

        updateGenv m = set genvMod m genv

        reifyInclude m (fs, loc, qual, as, binds, aliases)
            = updateModEnv $ex_fmap (doAliases $ex_bind doMerge), if null binds
            = ex_error (Unsupported "bindings in %include") loci, otherwise
              where
                loci = locOnlyInfo loc
                env  = view modEnv m
                imn  = fileSpecName fs
                ienv = (view modExportMap . fromJust . m_lookup imn) mm

                updateModEnv e = set modEnv e m 
                doAliases      = ex_foldM aliasNameMap ienv aliases
                doMerge        = mergeEnvironments loci qual as env

                aliasNameMap e (AliasRemove n)
                    = delNameMap loci (unresolveName n) e $ex_bind
                      delNameMap loci (unqualifyName n)

                aliasNameMap e (Alias n' n)
                    = replaceNameMapKey loci (unresolveName n) (unresolveName n') e $ex_bind
                      replaceNameMapKey loci (unqualifyName n) (unqualifyName n')

|| reify module definitions that match a qualifying function
reifyDefns :: (defn -> bool) -> globalEnv -> excpt globalEnv
reifyDefns defType genv
    = updateGenv $ex_fmap ex_foldM ins env defLst
      where
        m      = view genvMod genv
        env    = view modEnv m
        defLst = (filter (defType . snd) . m_toList . view modDefns) m

        updateGenv e = set genvMod (set modEnv e m) genv
        ins e (n, d) = insNameMapDefn e n d

|| convert unreified list of export directive parts to a reified export environment
reifyExports :: globalEnv -> excpt globalEnv
reifyExports genv
    = updateGenv $ex_fmap doExports m exps
      where
        m    = view genvMod genv
        mn   = view modName m
        exps = view modExportParts m
        env  = view modEnv m

        updateGenv m     = set genvMod m genv 

        doExports m []   = doExports m [LibAll (initLocation mn)]     || default is to export everything
        doExports m exps = converse setModExportMap m $ex_fmap ex_foldM doPart m_empty exps

        doPart e (LibAll loc)      = mergeFromModule loc mn e
        doPart e (LibFile f loc)   = mergeFromModule loc (fileSpecName f) e
        doPart e (LibRemove n loc) = doRemove loc n e
        doPart e (LibIdent n loc)
            = ex_error (Undefined n) loci,       if isNothing md
            = insNameMapDefn e qn (fromJust md), otherwise
              where
                loci = locOnlyInfo loc
                qn   = qualifyName mn n
                md   = m_lookup n env $mb_bind lookupDefn genv

        mergeFromModule loc mn e
            = ex_error (Info ("module " ++ mn ++ " not found")) loci,                   if isNothing m
            = mergeEnvironments loci LibUnqualified LibNoAs e (definedInModule mn env), otherwise
              where
               loci = locOnlyInfo loc
               m    = lookupModule genv mn

        || remove both qualified and unqualified entries in nameMap
        doRemove loc n e
            = delNameMap (locOnlyInfo loc) (unresolveName n') e $ex_bind
              delNameMap (locOnlyInfo loc) (unqualifyName n')
              where
                n' = qualifyName mn n, if isUnqualified n
                   = n,                otherwise


|| reification (expansion) of type synonyms

|| state passed through expansion
tsSt == ((globalEnv, [exception]), locInfo)

|| lenses for tsSt fields
tsGenv     = composeLens lensFst lensFst
tsErrs     = composeLens lensFst lensSnd
tsLoc      = lensSnd

genvModDefns = genvMod $composeLens modDefns
genvModSpecs = genvMod $composeLens modSpecs
tsModDefns   = tsGenv  $composeLens genvModDefns
tsModSpecs   = tsGenv  $composeLens genvModSpecs

initTsSt :: globalEnv -> tsSt
initTsSt genv = ((genv, []), emptyLocInfo)

addTsErr :: exceptionInfo -> locInfo -> tsSt -> tsSt
addTsErr ei loc = over tsErrs ((Error, ei, loc) :)


|| expand references to type synonyms in a texpr
expand :: astRewriter tsSt
expand (Tname n vars) st
    = (Tname n vars, st),                    if isBuiltin n \/ isNothing mdef \/ ~isDefType def
    = (Tname n vars, addTsErr Arity loc st), if #vars ~= #vars'       || add an error to st if arities don't match
    = (renamedTe, st),                       if isSyn def
    = (Tname n vars, st),                    otherwise
      where
        loc                          = view tsLoc st
        mdef                         = lookupDefn (view tsGenv st) n
        def                          = fromJust mdef
        Tname n' vars'               = view defnTform def
        te'                          = view defnType def
        rnMap                        = m_fromList (zip2 vars' vars)
        (renamedTe, x)               = astRewriteTDCont renameTvars te' rnMap   || note: TDCont used to prevent infinite loop in renaming Tvars
        renameTvars k (Tvar n) rnMap = (m_findWithDefault (Tvar n) (Tvar n) rnMap, rnMap)
        renameTvars k a        s     = k a s
expand a s                           = (a, s)

|| expand a type synonym to a base type
expandSyn :: tsSt -> name -> tsSt
expandSyn st n
    = over tsModDefns (m_insert n d') st'
      where
        genv       = view tsGenv st
        d          = fromJust (lookupDefn genv n)        || guaranteed to exist
        te         = view defnType d
        loc        = defnLocInfo d
        (te', st') = astRewriteTD expand te (set tsLoc loc st)
        d'         = set defnType te' d

|| expand type synonyms in dependency order
expandSyns :: tsSt -> tsSt
expandSyns st
    = addTsErr (DepCycle (showCycles cycs)) emptyLocInfo st, if isLeft morder
    = foldl expandSyn st order,                              otherwise
      where
        synDefs     = m_filter isSyn (view tsModDefns st)
        morder      = (dependencyOrder . makeDependencyGraph . makeDefnDependencyList) synDefs
        Left cycs   = morder
        Right order = morder

        showCycles cs
            = (shows "type definitions: " . interShows ", " (map showsCycle cs)) ""
              where
              showsCycle ns = showsInParens (interShows ", " (map showsName ns))

|| expand type synonyms used in defSpecs
expandSpecs :: [defn] -> tsSt -> tsSt
expandSpecs specs st
    = foldl expandSpec st specs
      where
        expandSpec st d
            = over tsModSpecs (insDefnMapUnchecked d') st'
              where
                loc        = defnLocInfo d
                (te', st') = astRewriteTD expand (view defnType d) (set tsLoc loc st)
                d'         = set defnType te' d

|| expand type synonyms used in data defns
expandDataDefs :: [defn] -> tsSt -> tsSt
expandDataDefs ddefs st
    = foldl expandData st ddefs
      where
        expandData st d
            = over tsModDefns (insDefnMapUnchecked d') st'
              where
                loc           = defnLocInfo d
                (ctors', st') = astRewriteList (astRewriteTD expand) (view dataCtors d) (set tsLoc loc st)
                d'            = set dataCtors ctors' d

|| reify type synonyms, checking for dependency loops
reifySyns :: globalEnv -> excpt globalEnv
reifySyns genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        st            = expandSyns (initTsSt genv)
        (errs, warns) = partition isError (view tsErrs st)

|| reify specs and data defs
reifyTypes :: globalEnv -> excpt globalEnv
reifyTypes genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        specs         = (filter isDefSpec . m_elems . view genvModSpecs) genv
        ddefs         = (filter isDefData . m_elems . view genvModDefns) genv
        st            = (expandDataDefs ddefs . expandSpecs specs . initTsSt) genv
        (errs, warns) = partition isError (view tsErrs st)

|| reify module abstype definitions by converting them to opaque DefData with no construtors,
|| then re-expanding their signature specs
reifyDefAbs :: globalEnv -> excpt globalEnv
reifyDefAbs genv
    = ex_return (view tsGenv st) warns, if null errs
    = ex_errs   errs,                   otherwise
      where
        absTys        = (filter isDefAbs . m_elems . view genvModDefns) genv
        st            = foldl update (initTsSt genv) absTys
        (errs, warns) = partition isError (view tsErrs st)

        update st (DefAbs tf specs te an)
            = (expandSpecs specs . over tsModDefns (insDefnMapUnchecked d)) st
              where
                d = DefData tf [] an

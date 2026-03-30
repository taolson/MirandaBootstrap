|| -*- mode: indented-text -*-
||
|| module.m -- Miranda top-level library modules


%export +

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
%include "name"
%include "predef"
%include "set"

|| fileSpec used in imports and exports

fileSpec ::= FileSpec [string] string

fileSpecPath, fileSpecName, fullFileName :: fileSpec -> string
fileSpecPath (FileSpec ps n) = intercalate filePathSeparator ps
fileSpecName (FileSpec ps n) = n
fullFileName (FileSpec ps n) = intercalate filePathSeparator (ps ++ [n])

fileSpecFromString :: string -> fileSpec
fileSpecFromString s
    = (FileSpec ps . withoutSuffix "m") n
      where
        Just (ps, n) = viewR (split (hd filePathSeparator) s)

|| %export directive
libPart ::=
    LibIdent   name     location |      || export this identifier
    LibRemove  name     location |      || remove this identifier from the export
    LibFile    fileSpec location |      || export all identifers exported from this module
    LibAll              location        || export all identifiers in this module (default, if no export directive)

isLibIdent :: libPart -> bool
isLibIdent (LibIdent n loc) = True
isLibIdent lp               = False

isLibRemove :: libPart -> bool
isLibRemove (LibRemove n loc) = True
isLibRemove lp                = False

|| qualification spec for imported identifiers
libQual ::= LibUnqualified | LibQualified
libAs   ::= LibNoAs | LibAs string

|| alias of import module names to local names
libAlias ::=
    Alias       name name |
    AliasRemove name

|| binding of %free variables or types 
libBinding ::=
    BindVar  name expr  |
    BindType name texpr

|| %include/%import directive
libEnv == (fileSpec, location, libQual, libAs, [libBinding], [libAlias])

|| lenses on libEnv
libEnvFile  = lensTup6_0
libEnvLoc   = lensTup6_1
libEnvQual  = lensTup6_2
libEnvAs    = lensTup6_3
libEnvBind  = lensTup6_4
libEnvAlias = lensTup6_5

|| build status, used to differentiate modules that have only been parsed
|| with ones that are fully built (e.g. read in from a serialized version)
buildStatus ::= Unbuilt | Built | Serialized

|| Miranda top-level Library (Module) definition
module ::=
    Module
        fileSpec                   || module file
        buildStatus                
        (either [libPart] nameMap) || exports, initially a list of libParts, eventually reified into a nameMap
        [libEnv]                   || includes
        defnMap                    || top-level definitions (functions, simple patterns, data types, and type synonyms, all in the same namespace)
        [defn]                     || top-level patterns before they are desugared to a simple pattern
        defnMap                    || top-level type specifiers
        nameMap                    || top-level environment for this module, mapping Unqualified names to Qualified names

showModule :: module -> string
showModule m
    = (lay . map showDefn . m_keys) defns
      where
        defns           = view modDefns m
        specs           = view modSpecs m
        showDefn n      = (mshows (m_lookup n specs) . shows "\n" . mshows (m_lookup n defns)) "\n"
        mshows Nothing  = shows ""
        mshows (Just a) = showsAst a

|| create an empty module with a name and initial environment
emptyModule :: fileSpec -> nameMap -> module
emptyModule fs env = Module fs Unbuilt (Left []) [] m_empty [] m_empty env

|| lenses for module components
modFile :: lens module fileSpec
modFile
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = fs
        overf fn (Module fs bs exs ins defs pats specs env) = Module (fn fs) bs exs ins defs pats specs env

|| the name of a module is implicitly its fileSpecName
modName :: lens module string
modName
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = fileSpecName fs
        overf fn (Module fs bs exs ins defs pats specs env) = error "cannot modify modName"

modBuildStatus :: lens module buildStatus
modBuildStatus
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = bs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs (fn bs) exs ins defs pats specs env

|| note: this lens works exclusively on the Left ([libPart]) form of exports
modExportParts :: lens module [libPart]
modExportParts
    = Lens getf overf
      where
        getf (Module fs bs (Left exs) ins defs pats specs env)     = exs
        overf fn (Module fs bs (Left exs) ins defs pats specs env) = Module fs bs (Left (fn exs)) ins defs pats specs env

|| note: this lens works exclusively on the Right (nameMap) form of exports
|| also: the "set" Lens operation cannot be used with this lens to change a (Left [libPart]) form to a (Right nameMap) form; for
|| that, use setModExportMap
modExportMap :: lens module nameMap
modExportMap
    = Lens getf overf
      where
        getf (Module fs bs (Right exs) ins defs pats specs env)     = exs
        overf fn (Module fs bs (Right exs) ins defs pats specs env) = Module fs bs (Right (fn exs)) ins defs pats specs env

|| used to replace the (Left [libPart]) form with a (Right nameMap) form, since a lens cannot effect the either change with an overf fn
setModExportMap :: nameMap -> module -> module
setModExportMap m (Module fs bs exs ins defs pats specs env) = Module fs bs (Right m) ins defs pats specs env
modIncludes :: lens module [libEnv]
modIncludes
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = ins
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs (fn ins) defs pats specs env

modIncludeFiles :: module -> [fileSpec]
modIncludeFiles = map (view libEnvFile) . view modIncludes

modDefns :: lens module defnMap
modDefns
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = defs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins (fn defs) pats specs env

modPats :: lens module [defn]
modPats
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = pats
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs (fn pats) specs env

modSpecs :: lens module defnMap
modSpecs
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = specs
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs pats (fn specs) env

modEnv :: lens module nameMap
modEnv
    = Lens getf overf
      where
        getf (Module fs bs exs ins defs pats specs env)     = env
        overf fn (Module fs bs exs ins defs pats specs env) = Module fs bs exs ins defs pats specs (fn env)


|| defnMap operations

|| insert a name -> defn into a defnMap without checking for name clash or merging with a DefMult
insDefnMapUnchecked :: defn -> defnMap -> defnMap
insDefnMapUnchecked d m = m_insert (defnName d) d m

|| insert a Qualified name -> defn into a defnMap, or report a name clash
insDefnMap :: defn -> defnMap -> excpt defnMap
insDefnMap d m
    = ex_pure (m_insert n d m),                if isNothing md'
    = ex_error (NameClash n') (defnLocInfo d), otherwise
      where
        n   = defnName d
        md' = m_lookup n m
        d'  = fromJust md'
        n'  = defnName d'

|| insert a DefFn into a DefnMap by merging it into an existing DefMult, or create one
insDefMult :: defn -> defnMap -> excpt defnMap
insDefMult d m
    = ex_pure (m_insertWith addMult n dm m),   if isNothing md' \/ isDefMult d'
    = ex_error (NameClash n') (defnLocInfo d), otherwise
      where
        n              = defnName d
        md'            = m_lookup n m
        d'             = fromJust md'
        n'             = defnName d'
        dm             = DefMult Mfun [d]

        addMult (DefMult x1 defs') (DefMult x2 defs)
            = DefMult Mfun (defs ++ defs') || add new def after any existing defs to maintain pattern order

|| insert a Qualified name -> defn into a defnMap, merging DefFns with a DefMult,
|| and merging abstract and implementation types
insDefnMapMerge :: defn -> defnMap -> excpt defnMap
insDefnMapMerge d m
    = insDefMult d  m,          if isDefFn d
    = insDefAbs  d  d',         if isDefAbs d & isJust md'      || abstract type defined before implementation synonym
    = insDefAbs  d' d,          if isDefSyn d & isJust md'      || implementation synonym defined before abstract type
    = ex_pure (m_insert n d m), if isNothing md'
    = ncError,                  otherwise
      where
        n       = defnName d
        md'     = m_lookup n m
        d'      = fromJust md'
        ncError = ex_error (NameClash (defnName d')) (defnLocInfo d)

        || merge an implementation type synonym into a DefAbs
        insDefAbs (DefAbs tf ds t an) (DefSyn tf' t' an')
            = ex_error Arity (defnLocInfo d),              if #tformVars tf ~= #tformVars tf'
            = ex_pure (m_insert n (DefAbs tf ds t' an) m), if t == builtinTypeType        || check for previous merging of implementation type

        insDefAbs d d' = ncError        || all other cases are a name clash

|| makeDefnDependencyList -- make a list of dependencies of each defn on the others in a defnMap, based upon the annotation's freeVar info
makeDefnDependencyList :: defnMap -> [(name, [name])]
makeDefnDependencyList defs
    = map (mapSnd (filter (converse m_member defs) . s_toList . view defnAnFree)) (m_toList defs)


|| functions to insert a top-level declaration into the correct part of a module, or report an error
modInserter == module -> excpt module

|| insert an %export declaration
modInsExp :: [libPart] -> modInserter
modInsExp pts m
    = ex_pure (over modExportParts (pts ++) m)

|| insert an %include/%import declaration, qualifying any alias directives with the module name
modInsInc :: libEnv -> modInserter
modInsInc lenv
    = ex_pure . over modIncludes (lenv' :)
      where
        mn    = fileSpecName (view libEnvFile lenv)
        lenv' = over libEnvAlias (map doQual) lenv

        doQual (Alias n1 n2)   = Alias (qualifyName mn n1) (qualifyName mn n2)
        doQual (AliasRemove n) = AliasRemove (qualifyName mn n)


|| insert a %free declaration
modInsFre :: [defn] -> modInserter
modInsFre specs m = ex_error (Unsupported "%free") emptyLocInfo

|| insert a top-level definition
modInsDef :: defn -> modInserter
modInsDef d m
    = (ex_fmap setDefs . insDefnMapMerge qd . view modDefns) m
      where
        qd           = qualifyDefn (view modName m) d
        setDefs defs = set modDefns defs m

|| insert a top-level pattern
modInsPat :: defn -> modInserter
modInsPat d m
    = ex_pure (over modPats (qd :) m)
      where
        qd = qualifyDefn (view modName m) d

|| make a constructor that has strict fields by inserting an Ecase evaluation of those
|| fields before performing builtinPack
makeStrictCtor :: name -> num -> [texpr] -> anno -> defn
makeStrictCtor n tag cts an
    = DefFn n pvs (Elet False (m_singleton sc d) [] (foldr mkStrict (Ecall (Evar sc) evs) (zip2 evs cts))) an
      where
        sc        = (Internal "Sc" 0)           || name for internal constructor to call after fields made strict
        (x1, vs)  = genNames "var" 0 cts        || generate names for the constructor vars
        pvs       = map Pvar vs
        evs       = map Evar vs
        d         = DefFn sc [] (builtinPack n tag (#cts)) an  || note: nullary DefFn instead of DefPat

        mkStrict (e, t) k
            = (Ecase Csingle e [CaseAlt Pwildcard k]), if isStrict t
            = k,                                       otherwise

|| insert a data definition:
|| insert a type specification for each constructor
|| insert a function for each constructor which is an alias to builtinPack
modInsData :: defn -> modInserter
modInsData d m
    = ex_foldM insFn m (zip2 ctors [0 ..]) $ex_bind modInsDef d
      where
        DefData dt ctors an = d

        insFn m ((Tname n cts), tag)
            = modInsSpec spec m $ex_bind modInsDef fn
              where
                t      = foldr Tarr dt (map unStrict cts)
                spec   = DefSpec (Tname n []) t an
                fn     = makeStrictCtor n tag cts an,              if any isStrict cts
                       = DefFn n [] (builtinPack n tag (#cts)) an, otherwise    || note: nullary DefFn instead of DefPat

|| insert a type specification
modInsSpec :: defn -> modInserter
modInsSpec d m
    = (ex_fmap setSpecs . insDefnMap qd . view modSpecs) m
      where
        qd             = qualifyDefn (view modName m) d
        setSpecs specs = set modSpecs specs m

|| insert an abstract type definition and its signature specs
modInsAbs :: defn -> modInserter
modInsAbs d m
    = ex_foldM (converse modInsSpec) m ds $ex_bind modInsDef d
      where
        DefAbs tf ds t an = d


|| make a module from an initial module and a sequence of modInserter functions
makeModule :: module -> [modInserter] -> excpt module
makeModule m insrs
    = ex_foldM rapply m insrs'
      where
        mn     = view modName m
        stdlib = FileSpec stdlibPath stdlibModName
        stdInc = modInsInc (stdlib, initLocation mn, LibUnqualified, LibNoAs, [], [])

        insrs' = insrs,          if mn == stdlibModName
               = stdInc : insrs, otherwise


|| global environment operations

moduleMap == m_map string module        || map from module name to module
globalEnv == (module, moduleMap, num)   || global environment is current module, the moduleMap, and a generator number

|| lenses for globalEnv
genvMod :: lens globalEnv module
genvMod = lensTup3_0

genvModMap :: lens globalEnv moduleMap
genvModMap = lensTup3_1

genvGen :: lens globalEnv num
genvGen = lensTup3_2

|| update moduleMap with current module
updateEnv :: globalEnv -> globalEnv
updateEnv (m, mm, gen) = (m, m_insert (view modName m) m mm, gen)

lookupModule :: globalEnv -> string -> maybe module
lookupModule genv n
    = Just m,                            if n == view modName m     || m is "cached version" of the module in mm; it may be more up-to-date
    = m_lookup n (view genvModMap genv), otherwise
      where
        m  = view genvMod genv

lookupName :: globalEnv -> name -> maybe name
lookupName genv n
    = (m_lookup n . view modEnv . view genvMod) genv, if isUnqualified n \/ isUnresolved n
    = Just n,                                         otherwise

|| lookup the definition of a Qualified name, using a lens to specify the defnMap to use
lookupDefnWithLens :: globalEnv -> lens module defnMap -> name -> maybe defn
lookupDefnWithLens genv l n
    = Nothing,                                  if isUnqualified n \/ isBuiltin n
    = lookupModule genv (getModName n) $mb_bind
      (m_lookup n . view l),                    otherwise

lookupDefn :: globalEnv -> name -> maybe defn
lookupDefn genv n
    = lookupName genv n $mb_bind lookupDefnWithLens genv modDefns

|| lookup the type specification for a name in the global environment
|| tuple types are dynamically generated
lookupSpec :: globalEnv -> name -> maybe defn
lookupSpec genv n
    = lookupName genv n $mb_bind getSpec
      where
        getSpec qn = Just (makeTupleSpec qn),             if isBuiltinTuple qn
                   = m_lookup qn builtinSpecs,            if isBuiltin qn
                   = lookupDefnWithLens genv modSpecs qn, otherwise

        makeTupleSpec qn = DefSpec (Tname qn []) (makeTupleCtorType qn) (genvAnno genv)


|| lookup all the constructor names associated with a constructor name
|| generate name list for builtin constructors directly, since they aren't stored anywhere else
lookupCtorNames :: globalEnv -> name -> maybe [name]
lookupCtorNames genv n
    = Just [n],                                   if isBuiltinTuple n
    = Just [builtinNilName, builtinConsName],     if s == getName builtinNilName \/ s == getName builtinConsName
    = Just [builtinUnitName],                     if s == getName builtinUnitName
    = lookupSpec genv n            $mb_bind     || lookup spec for ctor, which should be a $Tarr b ... $Tarr (data type Tform)
      (lookupDefn genv . baseName) $mb_kbind    || lookup defn of data type
      (mb_pure . map tformName . view dataCtors), otherwise
      where
        s                            = getName n
        baseName (DefSpec tf te an ) = tformName (baseType te)

|| lookup the arity for a function reference in the global environment
|| since constuctor definitions use the poly-arity "pack" builtin, they don't have arguments,
|| so the arity for them is determined from their specs
lookupArity :: globalEnv -> name -> maybe num
lookupArity genv n
    = lookupSpec genv n $mb_bind sa,  if isConstructor n \/ isBuiltin n
    = lookupDefn genv n $mb_bind fca, otherwise
      where
        fca (DefFn n vars rhs an) = mb_pure (#vars)
        fca (DefPat p e an)       = mb_pure 0
        fca d                     = Nothing
        sa (DefSpec t te an)      = mb_pure (countArrs 0 te)
        sa d                      = Nothing
        countArrs n (Tarr t1 t2)  = countArrs (n + 1) t2
        countArrs n t             = n

|| create an anno for top-level defns from genv
genvAnno :: globalEnv -> anno
genvAnno = locAnno . initLocation . view modName . view genvMod

|| extract all of the top-level definitions from a specified module
definedInModule :: string -> nameMap -> nameMap
definedInModule s e
    = m_filter (fromModule s) e
      where
        fromModule s (Qualified mn v) = s == mn
        fromModule s x                = False


|| nameMap operations

|| insert a mapping into a nameMap, or report a name clash
insNameMap :: locInfo -> name -> name -> nameMap -> excpt nameMap
insNameMap loc k v m
    = ex_pure (m_insert k v m),      if isNothing mprev
    = ex_error (NameClash prev) loc, otherwise
      where
        mprev = m_lookup k m
        prev  = fromJust mprev

|| delete a name mapping from a nameMap, or report an undefined error
delNameMap :: locInfo -> name -> nameMap -> excpt nameMap
delNameMap loc k m
    = ex_pure (m_delete k m),     if m_member k m \/ isUnqualified k    || delete or silently ignore, if Unqualified
    = ex_error (Undefined k) loc, otherwise                             || report an error, otherwise

|| replace a key name in the nameMap
|| used for aliasing names during import
replaceNameMapKey :: locInfo -> name -> name -> nameMap -> excpt nameMap
replaceNameMapKey loc k k' m
    = ex_pure m,                  if isNothing mprev & isUnqualified k || silently ignore replacement for unqualified names
    = delNameMap loc k m $ex_bind
      insNameMap loc k' prev,     otherwise
      where
        mprev = m_lookup k m
        prev  = fromJust mprev

mergeEnvironments :: locInfo -> libQual -> libAs -> nameMap -> nameMap -> excpt nameMap
mergeEnvironments loc qual as e1 e2
    = (ex_foldM (addName qual) e1 . map (renameAs as) . filter (isQualified . snd) . m_toList) e2
      where
        renameAs LibNoAs   e      = e
        renameAs (LibAs s) (k, v)
            = (k, v),                        if isUnqualified k
            = (Unresolved s (getName k), v), otherwise

        addName LibUnqualified m (k, v) = insNameMap loc k v m
        addName LibQualified m (k, v)
            = ex_pure m,            if isUnqualified k
            = insNameMap loc k v m, otherwise

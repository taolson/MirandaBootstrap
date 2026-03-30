#! /usr/local/bin/mira -exec

|| -*- mode: indented-text -*-
||
|| mirac -- top level compiler


|| configuration
streamFusion = False    || should be set the same as streamFusion config in inline.m


%export +

%include "analyze"
%include "ast"
%include "avl"
%include "codegen"
%include "demand"
%include "dependency"
%include "derive"
%include "desugar"
%include "either"
%include "exception"
%include "grammar"
%include "inline"
%include "interp"
%include "io"
%include "lens"
%include "map"
%include "maybe"
%include "mirandaExtensions"
%include "mirandaParser"
%include "module"
%include "name"
%include "normalize"
%include "predef"
%include "reify"
%include "rename"
%include "rewrite"
%include "serialize"
%include "set"
%include "stg"
%include "tokenizer"
%include "typecheck"


|| serialize a module to an ".x2" file
serialize :: globalEnv -> ios globalEnv
serialize genv
    = (io_ctrl . Tofile fx . serialModule) m $io_right
      io_ctrl (Closefile fx)                 $io_right
      io_pure genv'
      where
        m     = view genvMod genv
        fs    = view modFile  m
        fx    = withSuffix "x2" (fullFileName fs)
        genv' = over genvMod (set modBuildStatus Serialized) genv

parseOrDeserial :: fileSpec -> ios module
parseOrDeserial f
    = io_excpt (ex_error (Info (fm ++ " not found")) emptyLocInfo), if isNothing mtsm
    = io_pure deserialRslt,                                         if isJust mtsm & isJust mtsx & tsx >= tsm
    = io_msg ("parsing  " ++ fn) $io_right io_excpt parseRslt,      otherwise
      where
        fn           = fullFileName f
        fm           = withSuffix "m" fn
        fx           = withSuffix "x2" fn
        mtsm         = mtimeFile fm
        mtsx         = mtimeFile fx
        tsm          = fromJust mtsm
        tsx          = fromJust mtsx
        initModule   = emptyModule f builtinEnv
        deserialRslt = (fst . deserialModule . read) fx
        parseRslt    = parse initModule (read fm)

parseFiles :: [fileSpec] -> ios moduleMap
parseFiles files
    = go m_empty files
      where
        go modules [] = io_pure modules
        go modules (f : fs)
            = go modules fs,                      if m_member mn modules    || already parsed
            = parseOrDeserial f $io_bind addIncs, otherwise
              where
                mn = fileSpecName f
                addIncs mdl
                    = go modules' (fs ++ includes)
                      where
                        modules'  = m_insert mn mdl modules
                        includes  = modIncludeFiles mdl

|| build a module and insert into the module map, by making a globalEnvironment
|| and passing that through a sequence of compilation passes
buildModule :: moduleMap -> string -> ios moduleMap
buildModule mm n
    = updateModMap $io_fmap io_foldM doPass genv passes $io_bind serialize
      where
        Just m        = m_lookup n mm
        updateModMap  = view genvModMap . updateEnv . over genvMod (set modBuildStatus Built)
        notData d     = ~isDefData d & ~isConstructor (defnName d)
        doPass genv p = io_excpt (p genv)
        genv          = (m, mm, 20)       || the gen value must be larger than the largest number of free variables in the type signature for any function,
                                          || otherwise the requirement that new free vars cannot conflict with any existing ones during type checking
                                          || can be violated
        passes =
            [ reifyIncludes
            , reifyDefns isDefData        || reify data type and constructor names early for desugaring constructor patterns
            , deriveInstances             || derive any missing instances for show, ord from the DefData and DefSyn defns
            , desugar                     || desugar complex pats, list comprehensions, conditionals, etc.
            , reifyDefns notData          || reify all the desugared fn and pat definitions
            , rename                      || qualify or uniquely rename as Internal all Unqualified names
            , analyze                     || free var analysis for rewrite
            , rewrite                     || organize Elets for typecheck, cleanup and general optimizations   
            , analyzeWarn                 || re-do free var analysis after rewrite and check for unused variables and non-exhaustive patterns
            , reifySyns                   || expand type synonyms to base types, checking for cycles
            , reifyTypes                  || expand specs and dataDefs with expanded type synonyms
            , typecheck                   || type check the module
            , streamSubst                 || do stream-based substitution of standard list-based functions
            , inline                      || do inlining of small, non-recursive functions, ast simplification
            , normalize                   || rewrite to A-Normal Form and saturate calls to known functions
            , demand                      || demand analysis to discover strictness and rewrite strict-use thunks to Ecase exprs
            , analyze                     || final free var analysis for STG (normalization may have created new thunks)
            , reifyDefAbs                 || hide abstype implementations
            , reifyExports                || process export libParts and create export nameMap
            ]

|| build the modules in dependency order
build :: [string] -> moduleMap -> ios moduleMap
build stdMods mm
    = io_excpt (ex_error (DepCycle (showCycles cyc)) emptyLocInfo), if isLeft morder
    = io_foldM bm mm order',                                        otherwise
      where
        morder      = (dependencyOrder . makeDependencyGraph . m_toList . m_fmap (map fileSpecName . modIncludeFiles)) mm
        Left cyc    = morder
        Right order = morder
        order'      = stdMods ++ order       || force stdMods to be built first, as there are implicit dependencies on them

        showCycles cs
            = (shows "module includes: " . interShows ", " (map showsCycle cs)) ""
              where
                showsCycle ns = showsInParens (interShows ", " (map shows ns))

        bm mm mn
            = io_msg ("building " ++ mn) $io_right buildModule mm mn, if bs == Unbuilt
            = io_pure mm,                                             otherwise
              where
                bs = (view modBuildStatus . fromJust . m_lookup mn) mm

compile :: [string] -> ios moduleMap
compile files
    = parseFiles (map mkStd stdMods ++ map fileSpecFromString files) $io_bind build stdMods
      where
        stdMods  = [stdlibModName, strmModName], if streamFusion
                 = [stdlibModName],              otherwise
        mkStd s  = FileSpec stdlibPath s

gen :: string -> string -> [sys_message]
gen s entry
    = map (toSysMsg (const "")) (go fn)
      where
        fs     = fileSpecFromString s
        fn     = fullFileName fs
        mn     = fileSpecName fs
        qentry = Qualified mn entry
        qstart = Qualified mn "!start"
        asm    = withSuffix "s" fn

        go fn
            = (compile [fn] $io_bind
               genStg       $io_kbind
               reportInsns  $io_kbind
               genCode)     $io_right
               io_pure ()

        genStg mm
            = io_msgc "generating STG code " $io_right
              io_pure (buildStg qentry genv)
              where
                m      = fromJust (m_lookup mn mm)
                genv   = (m, mm, 0)

        || tally total insns generated to force evaluation of build before gen
        reportInsns stg
            = io_msg (shownum n ++ " STG insns") $io_right
              io_pure stg
              where
                (e, c, h) = stg
                n         = (sum . map (length . snd) . m_elems . fst) c

        genCode stg
            = io_msg "generating asm code"                $io_right
              (io_ctrl . Tofile asm . codegen qstart) stg $io_right
              io_ctrl (Closefile asm)

main :: [sys_message]
main
    = go cmdline
      where
        cmdline = $*

        go [x, fn]        = gen fn "main"
        go [x, fn, start] = gen fn start
        go x              = error "usage: mirac <file name> {entry name}", otherwise


|| testing

test :: string -> [sys_message]
test mn = (map (toSysMsg showModule) . io_fmap (fromJust . m_lookup mn) . compile) [mn]

testfull :: string -> string -> [sys_message]
testfull mn start
    = map (toSysMsg (const "")) (compile [mn] $io_bind f1)
      where
        f1 mm
            = io_excpt (ex_error (Undefined startName) emptyLocInfo), if isNothing (lookupDefn genv startName)
            = io_mapM_ (msgDefn genv) seen,                           otherwise
              where
                m         = fromJust (m_lookup mn mm)        || guaranteed to exist
                genv      = (m, mm, 0)
                startName = Qualified mn start
                seen      = walk (s_singleton startName) s_empty

                walk ns seen
                    = s_toList seen,  if s_null ns
                    = walk ns1 seen,  if s_member n seen
                    = walk ns1 seen', if isBuiltin n
                    = walk ns2 seen', otherwise
                      where
                        (n, ns1)    = s_deleteFindMin ns
                        d           = fromJust (lookupDefn genv n)      || guaranteed to exist
                        uses        = (s_filter qualOrPvt . view defnAnFree) d
                        ns2         = s_union ns1 uses
                        seen'       = s_insert n seen
                        qualOrPvt n = isQualified n \/ isPrivate n
        msgDefn genv n
            = (io_msg . mshow . lookupSpec genv) n $io_right
              (io_msg . mshow . lookupDefn genv) n $io_right
              io_msg ""
              where
                mshow Nothing  = ""
                mshow (Just a) = showAst a

raw :: string -> [sys_message]
raw mn = (map (toSysMsg showRaw) . io_fmap (fromJust . m_lookup mn) . compile) [mn]

showRaw :: module -> string
showRaw m
    = (lay . map showDefn . m_keys) defns
      where
        defns          = view modDefns m
        specs          = view modSpecs m
        showDefn n     = mshow (m_lookup n specs) ++ mshow (m_lookup n defns)
        mshow Nothing  = ""
        mshow (Just a) = showAstRaw a ++ "\n"

stg :: string -> string -> [sys_message]
stg mn start
    = map (toSysMsg (const "")) (compile [mn] $io_bind doBuild)
      where
        doBuild mm
            = io_mapM_ msgCode (m_toList (fst scode))
              where
                Just m               = m_lookup mn mm
                genv                 = (m, mm, 0)
                (lenv, scode, sheap) = buildStg (Qualified mn start) genv
                msgCode (cp, (loc, insts))
                    = io_msg ("\n" ++ shownum cp ++ " -> " ++ showsLocInfo loc "") $io_right
                      foldr (io_right . io_msg . showStgInsn) (io_pure "") insts

run :: string -> string -> [sys_message]
run mn start
    = map (toSysMsg id) (compile [mn] $io_bind doInterp)
      where
        doInterp mm
            = interp qinit (buildStg qstart genv)
              where
                qstart = Qualified mn start
                qinit  = Qualified mn "!start"
                m      = fromJust (m_lookup mn mm)
                genv   = (m, mm, 0)

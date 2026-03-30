|| -*- mode: indented-text -*-
||
|| parser.m -- basic parser combinators


%export + p_get p_put p_modify p_bind p_pure p_fail p_fmap p_apply p_liftA2 p_liftA3 p_liftA4 p_left p_right p_bind2 p_mapM p_foldM

%include "lens"
%include "maybe"
%include "name"
%include "mirandaExtensions"
%include "maybeState" p_get/mst_get p_put/mst_put p_modify/mst_modify p_bind/mst_bind p_pure/mst_pure p_fail/mst_fail p_fmap/mst_fmap
                      p_apply/mst_apply p_liftA2/mst_liftA2 p_liftA3/mst_liftA3 p_liftA4/mst_liftA4 p_left/mst_left p_right/mst_right
                      p_bind2/mst_bind2 p_mapM/mst_mapM p_foldM/mst_foldM
%include "tokenizer"


|| Basic Parsing
||
|| state passed through the various parsers
|| deepest parse error, stack of indentation levels (for offside handling), and unparsed token stream

psError == (string, num, num, num)              || err string, severity, line#, col#
psSt    == (string, psError, [num], [tokloc])   || module name, deepest parse error, indent-level stack, list of tokens and locations

emptyError  =  ("", 0, 0, 0)

|| lenses for pSt fields
psModName = lensTup4_0
psErr     = lensTup4_1
psErrStr  = composeLens lensTup4_1 lensTup4_0
psErrSev  = composeLens lensTup4_1 lensTup4_1
psErrLn   = composeLens lensTup4_1 lensTup4_2
psErrCn   = composeLens lensTup4_1 lensTup4_3
psIndLev  = lensTup4_2
psToks    = lensTup4_3

parser * == maybeState psSt *


|| parser error handling

|| merge the deepest error reported into the first parser state
mergeErr :: psSt -> psSt -> psSt
mergeErr ps1 ps2
    = set psErr pe' ps1
      where
        pe1                  = view psErr ps1
        pe2                  = view psErr ps2
        (en1, es1, el1, ec1) = pe1
        (en2, es2, el2, ec2) = pe2
        pe'                  = pe1, if el1 > el2        || choose error with largest line number (deepest)
                             = pe2, if el1 < el2
                             = pe1, if ec1 > ec2        || line numbers equal, choose error with largest column number 
                             = pe2, if ec1 < ec2
                             = pe1, if es1 >= es2       || error location equal, compare severity
                             = pe2, otherwise

|| keep deepest error reported in parse as part of the parser state
p_error :: num -> string -> parser *
p_error sev err ps
    = (Nothing, mergeErr ps ps')
      where
        (t, il, ic)      = hd (view psToks ps)
        ps'              = set psErr (err, sev, il, ic) ps


|| report an error that we expected something that wasn't there
p_expected :: string -> parser *
p_expected s = p_error 2 ("expected " ++ s)

|| report an error that we got an unexpected token
p_unexpected :: parser *
p_unexpected ps
    = p_error 1 ("unexpected " ++ err) ps
      where
        ls                = view psIndLev ps
        (t, il, ic) : ts' = view psToks ps
        offside           = ~null ls & (t == Tend \/ ic < hd ls)
        err               = "end of input",          if t == Tend
                          = "OFFSIDE",               if t == Toffside \/ offside
                          = "token " ++ showToken t, otherwise


|| alternative interface
|| note: this cannot simply be an alias to mst_alt, because we need to merge any parse errors from pa
p_alt :: parser * -> parser * -> parser *
p_alt pa pb
    = go
      where
        go ps
            = alt (mst_runState pa ps)
              where
                alt (Nothing, ps') = mst_runState pb (mergeErr ps ps')
                alt ra             = ra

|| p_peek and p_not check for the presence or absence of a token, but don't consume it
p_peek, p_not :: parser * -> parser ()
p_peek pa ps
    = (Just (), ps), if isJust ra
    = (Nothing, ps), otherwise
      where
        (ra, psa) = pa ps

p_not pa ps
    = (Just (), ps), if isNothing ra
    = (Nothing, ps), otherwise
      where
        (ra, psa) = pa ps

|| try a parser, but don't consume any tokens if it fails
p_try :: parser * -> parser *
p_try pa ps
    = parsea,    if isJust ra
    = (ra, ps'), otherwise
      where
        parsea    = pa ps
        (ra, psa) = parsea
        ps'       = mergeErr ps psa

p_optional :: parser * -> parser (maybe *)
p_optional pa ps
    = (Just (Just x), psa), if isJust ra
    = (Just Nothing, ps'),  otherwise
      where
        (ra, psa)  = pa ps
        Just x     = ra
        ps'        = mergeErr ps psa

p_guard :: parser * -> (* -> bool) -> parser *
p_guard p tst
    = (p $p_bind fguard) $p_alt p_unexpected    || note: use p_alt here to rewind parser state to show correct unexpected symbol
      where
        fguard x = p_pure x, if tst x
                 = p_fail,   otherwise

|| 1 or more p
p_some :: parser* -> parser [*]
p_some p = p_liftA2 (:) p (p_many p)

|| 0 or more p
p_many :: parser * -> parser [*]
p_many p = p_some p $p_alt p_pure []

|| 1 or more p seperated by ps
p_someSepBy :: parser* -> parser ** -> parser [**]
p_someSepBy ps p = p_liftA2 (:) p (p_many (ps $p_right p))

|| 0 or more p seperated by ps
p_manySepBy :: parser * -> parser ** -> parser [**]
p_manySepBy ps p = p_someSepBy ps p $p_alt p_pure []


|| token parsers

|| parser which accepts any token
|| also handle offside detection by inserting a Toffside token into the token stream
p_any :: parser token
p_any ps
    = p_unexpected ps,    if t == Tend & null ls        || end of token stream
    = p_error 2 err ps,   if isTerror t                 || error from lexer
    = (Just t', ps'),     otherwise
      where
        ls                = view psIndLev ps
        (t, il, ic) : ts' = view psToks ps
        Terror err        = t
        offside           = ~null ls & (t == Tend \/ ic < hd ls)
        t'                = Toffside,          if offside
                          = t,                 otherwise
        ps'               = ps,                if offside
                          = set psToks ts' ps, otherwise

|| parser which passes only at end of input
p_end :: parser ()
p_end = p_not p_any

|| parser for a token which satifies a condition
p_satisfy :: (token -> bool) -> parser token
p_satisfy tst = p_any $p_guard tst

|| parser which matches a specific token or posts an error
p_token :: token -> parser token
p_token t = p_satisfy (t ==)

|| parser which matches a specific character
p_char :: char -> parser token
p_char c = p_token (Tchar c)

|| parser which matches a specific identifier string
p_ident :: string -> parser token
p_ident s = p_token (Tident (Unqualified s))

|| parser which matches a specific symbol
p_sym :: string -> parser token
p_sym s = p_token (Tsymbol (Unqualified s))

p_semi, p_comma, p_equal, p_eqeq, p_minus, p_percent, p_colons, p_lArrow, p_rArrow, p_vBar, p_terminator :: parser token
p_semi       = p_char ';'
p_comma      = p_char ','
p_equal      = p_sym "="
p_eqeq       = p_sym "=="
p_minus      = p_sym "-"
p_percent    = p_sym "%"
p_colons     = p_sym "::"
p_lArrow     = p_sym "<-"
p_rArrow     = p_sym "->"
p_vBar       = p_sym "|"
p_terminator = p_token Toffside $p_alt p_semi


|| structure parsers

|| ( parser )
p_inParens :: parser * -> parser *
p_inParens p = p_char '(' $p_right p $p_left p_char ')'

|| [ parser ]
p_inBrackets :: parser * -> parser *
p_inBrackets p = p_char '[' $p_right p $p_left p_char ']'

|| { parser }
p_inBraces :: parser * -> parser *
p_inBraces p = p_char '{' $p_right p $p_left p_char '}'

|| < parser >
p_inAngles :: parser * -> parser *
p_inAngles p = p_sym "<" $p_right p $p_left p_sym ">"

|| block layout parser
p_inLayout :: parser * -> parser *
p_inLayout p = p_indent $p_right p $p_left p_outdent

|| push the current token column on the indent level stack in the parser state
p_indent :: parser ()
p_indent ps
    = (Just (), over psIndLev (ic :) ps)
      where
        (t, il, ic) = hd (view psToks ps)       || get current token indent column ic

p_indent' :: parser ()
p_indent' ps
    = (Nothing, ps),  if outdentedFrom lev
    = (Just (), ps'), otherwise
      where
        (t, il, ic) = hd (view psToks ps)       || get current token indent column ic
        lev         = view psIndLev ps
        ps'         = set psIndLev (ic : lev) ps

        outdentedFrom []       = False
        outdentedFrom (l : ls) = ic < l

|| check for separator and pop the indent level stack in the parser state
p_outdent :: parser ()
p_outdent
    = (p_terminator $p_right p_get) $p_bind (p_put . over psIndLev popLev)
      where
        popLev []       = []
        popLev (x : xs) = xs


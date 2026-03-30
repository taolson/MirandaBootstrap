|| -*- mode: indented-text -*-
||
|| name.m -- qualified names


%export +

%include "maybe"
%include "mirandaExtensions"


|| a name is an identifier, constructor, or symbol
name ::= Unqualified string        |         || unresolved, unqualified name
         Unresolved  string string |         || unresolved top-level name qualified by a module name
         Qualified   string string |         || resolved top-level name
         Private     string string |         || resolved top-level name, cannot be exported/imported (shared lit references)
         Builtin     string        |         || resolved top-level name for built-in (primitive)
         Internal    string num              || resolved non-top-level name, specialized with a int

getName :: name -> string
getName (Unqualified n)  = n
getName (Unresolved m n) = n
getName (Qualified m n)  = n
getName (Private m n)    = n
getName (Builtin n)      = n
getName (Internal n d)   = n

getModName :: name -> string
getModName (Unresolved m n) = m
getModName (Qualified m n)  = m
getModName (Private m n)    = m
getModName (Builtin n)      = "builtin"
getModName n                = ""

showName :: name -> string
showName (Unqualified n)   = n
showName (Unresolved m n ) = m ++ "." ++ n
showName (Qualified m n)   = m ++ "." ++ n
showName (Private m n)     = m ++ "." ++ n
showName (Builtin n)       = "builtin." ++ n
showName (Internal n d)    = n ++ "_" ++ shownum d

qualifyName :: string -> name -> name
qualifyName "builtin" (Unqualified n) = Builtin n
qualifyName m         (Unqualified n) = Qualified m n
qualifyName m         n               = n

unqualifyName :: name -> name
unqualifyName n = Unqualified (getName n)

unresolveName :: name -> name
unresolveName (Qualified m n) = (Unresolved m n)
unresolveName n               = n


|| testing 

isUnqualified, isUnresolved, isQualified, isPrivate, isBuiltin, isInternal, isBuiltinTuple :: name -> bool
isUnqualified (Unqualified n) = True
isUnqualified n               = False

isUnresolved (Unresolved m n) = True
isUnresolved n                = False

isQualified (Qualified m n)   = True
isQualified n                 = False

isPrivate (Private m n)       = True
isPrivate n                   = False

isBuiltin (Builtin n)         = True
isBuiltin n                   = False

isInternal (Internal n d)     = True
isInternal n                  = False

isBuiltinTuple (Builtin n)    = "Tuple" $isPrefixOf n
isBuiltinTuple n              = False

|| -*- mode: indented-text -*-
||
|| mirandaExtensions.m -- extensions to Miranda's standard library


%export +

%include "maybe"


|| debugging helpers

trace :: string -> * -> *
trace msg x = seq (force msg) (seq (system ("echo '" ++ msg ++ "' >> TRACE")) x)

|| safe version of hd -- tagged with number to isolate where the error is
safe_hd :: num -> [*] -> *
safe_hd n xs
    = error ("empty hd " ++ shownum n), if null xs
    = hd xs, otherwise

|| safe version of fromJust -- tagged with number to isolate where the error is
safe_fromJust :: num -> maybe * -> *
safe_fromJust n x
    = error ("fromJust Nothing " ++ shownum n), if isNothing x
    = fromJust x, otherwise

|| return the modification timestamp of a file, or Nothing if it doesn't exist
mtimeFile :: string -> maybe num
mtimeFile fn
    = Nothing, if es ~= 0 \/ so == ""
    = Just ts, otherwise
      where
        (so, se, es) = system ("stat -f %m '" ++ fn ++ "'")
        ts           = (numval . takeWhile digit . tl) so

|| basic types

string == [char]


|| application

apply :: (* -> **) -> * -> **
apply f x = f x

rapply :: * -> (* -> **) -> **
rapply x f = f x

strictApply :: (* -> **) -> * -> **
strictApply f x = x $seq f x

|| strict iteration
iterate' :: (* -> *) -> * -> [*]
iterate' f
    = go
      where
        go x = x $seq (x : go (f x))


|| currying

curry :: ((*, **) -> ***) -> * -> ** -> ***
curry f a b = f (a, b)

uncurry :: (* -> ** -> ***) -> (*, **) -> ***
uncurry f (a, b) = f a b

curry3 :: ((*, **, ***) -> ****) -> * -> ** -> *** -> ****
curry3 f a b c = f (a, b, c)

uncurry3 :: (* -> ** -> *** -> ****) -> (*, **, ***) -> ****
uncurry3 f (a, b, c) = f a b c


|| bool functions

not :: bool -> bool
not = (~)

if' :: bool -> * -> * -> *
if' True  a b = a
if' False a b = b


|| char functions

isLower :: char -> bool
isLower c = 'a' <= c <= 'z'

isUpper :: char -> bool
isUpper c = 'A' <= c <= 'Z'

toLower :: char -> char
toLower c
    = decode (code 'a' - code 'A' + code c), if 'A' <= c <= 'Z'
    = c,                                     otherwise

toUpper :: char -> char
toUpper c
    = decode (code c - code 'a' + code 'A'), if 'a' <= c <= 'z'
    = c,                                     otherwise

isSpace :: char -> bool
isSpace c
    = c == ' ' \/ c == '\t' \/ c == '\n'


|| numeric functions

signum :: num -> num
signum x
    = 1,    if x > 0
    = (-1), if x < 0
    = 0,    otherwise

even, odd :: num -> bool
even x = x mod 2 == 0
odd  x = x mod 2 == 1

gcd, lcm :: num -> num -> num
gcd x y
    = x,               if y == 0
    = gcd y (x mod y), otherwise

lcm x y = (x * y) div (gcd x y)


|| ordering functions

ordering ::= EQ | LT | GT

compare :: * -> * -> ordering
compare a b
    = EQ, if a == b
    = LT, if a < b
    = GT, otherwise

comparing :: (* -> **) -> * -> * -> ordering
comparing f a b
    = EQ, if f a == f b
    = LT, if f a <  f b
    = GT, otherwise

descending :: (* -> **) -> * -> * -> ordering
descending f a b
    = EQ, if f a == f b
    = GT, if f a <  f b
    = LT, otherwise


|| Tuple functions

pair :: * -> ** -> (*, **)
pair a b = (a, b)

triple :: * -> ** -> *** -> (*, **, ***)
triple a b c = (a, b, c)

setFst :: (*, **) -> *** -> (***, **)
setFst (a, b) a' = (a', b)

setSnd :: (*, **) -> *** -> (*, ***)
setSnd (a, b) b' = (a, b')

mapFst :: (* -> ***) -> (*, **) -> (***, **)
mapFst f (a, b) = (f a, b)

mapSnd :: (** -> ***) -> (*, **) -> (*, ***)
mapSnd f (a, b) = (a, f b)

mapBoth :: (* -> **) -> (*, *) -> (**, **)
mapBoth f (a, b) = (f a, f b)

swapPair :: (*, **) -> (**, *)
swapPair (a, b) = (b, a)

dup :: * -> (*, *)
dup x = (x, x)


|| list functions

null :: [*] -> bool
null [] = True
null x  = False

any :: (* -> bool) -> [*] -> bool
any p = or . map p

all :: (* -> bool) -> [*] -> bool
all p = and . map p

elem :: * -> [*] -> bool
elem = any . (==)

find :: (* -> bool) -> [*] -> maybe *
find p
    = foldr match Nothing
      where
        match x k
            = Just x, if p x
            = k,      otherwise

elemIndex :: * -> [*] -> maybe num
elemIndex a
    = mb_fmap fst . find ((a ==) . snd) . enumerate

enumerate :: [*] -> [(num, *)]
enumerate = zip2 [0 ..]

isPrefixOf :: [*] -> [*] -> bool
isPrefixOf []       ys       = True
isPrefixOf xs       []       = False
isPrefixOf (x : xs) (y : ys) = x == y & xs $isPrefixOf ys

stripPrefix :: [*] -> [*] -> maybe [*]
stripPrefix []       ys       = Just ys
stripPrefix xs       []       = Nothing
stripPrefix (x : xs) (y : ys) = stripPrefix xs ys, if x == y

unfoldr :: (** -> maybe (*, **)) -> ** -> [*]
unfoldr f
    = go
      where
        go b
            = fromMaybef [] cont (f b)
              where
                cont (a, b') = a : go b'

cycle :: [*] -> [*]
cycle xs = xs' where xs' = xs ++ xs'

singleton :: * -> [*]
singleton a = [a]

replicate :: num -> * -> [*]
replicate n x = take n (repeat x)

append :: [*] -> [*] -> [*]
append = (++)

delete :: * -> [*] -> [*]
delete x [] = []
delete x (y : ys)
    = ys,              if x == y
    = y : delete x ys, otherwise

deleteAt :: num -> [*] -> [*]
deleteAt i xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = xs,                if n == 0
                      = x : go (n - 1) xs, otherwise

setAt :: num -> * -> [*] -> [*]
setAt i a xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = a : xs,            if n == 0
                      = x : go (n - 1) xs, otherwise

modifyAt :: num -> (* -> *) -> [*] -> [*]
modifyAt i f xs
    = xs,      if i < 0
    = go i xs, otherwise
      where
        go n []       = []
        go n (x : xs) = f x : xs,          if n == 0
                      = x : go (n - 1) xs, otherwise

|| group adjacent elements according to a binary relation
|| note: this is different from Haskell's Prelude groupBy, which only performs the relation with the first element of the group
groupBy :: (* -> * -> bool) -> [*] -> [[*]]
groupBy p [] = []
groupBy p (x : xs)
    = (x : ys) : zs
      where
        (ys, zs) = go x xs
        go x (y : ys)
            = (y : as, bs),        if p x y
            = ([], (y : as) : bs), otherwise
              where (as, bs) = go y ys
        go x [] = ([], [])

group :: [*] -> [[*]]
group = groupBy (==)

concatMap :: (* -> [**]) -> [*] -> [**]
concatMap f = foldr ((++) . f) []

scanl :: (* -> ** -> *) -> * -> [**] -> [*]
scanl f z
    = go z
      where
        go z xs
            = z : go' xs
              where
                go' [] = []
                go' (x : xs) = go (f z x) xs

scanr :: (** -> * -> *) -> * -> [**] -> [*]
scanr f z []       = [z]
scanr f z (x : xs) = f x (hd qs) : qs
                     where
                       qs = scanr f z xs

inits, tails :: [*] -> [[*]]
inits = map reverse . scanl (converse (:)) []

tails [] = []
tails xs = xs : tails (tl xs)

mapAccumL :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
mapAccumL f s [] = (s, [])
mapAccumL f s (x : xs)
    = (s2, x' : xs')
      where
        (s1, x')  = f s x
        (s2, xs') = mapAccumL f s1 xs

mapAccumR :: (* -> ** -> (*, ***)) -> * -> [**] -> (*, [***])
mapAccumR f s [] = (s, [])
mapAccumR f s (x : xs)
    = (s2, x' : xs')
      where
        (s1, xs') = mapAccumR f s xs
        (s2, x')  = f s1 x

takeWhile :: (* -> bool) -> [*] -> [*]
takeWhile f [] = []
takeWhile f (x : xs) = x : takeWhile f xs, if f x
                     = [], otherwise

dropWhile :: (* -> bool) -> [*] -> [*]
dropWhile f [] = []
dropWhile f xs = dropWhile f xs', if f x
               = xs,              otherwise
                 where
                   x : xs' = xs

partition :: (* -> bool) -> [*] -> ([*], [*])
partition p
    = foldr part ([], [])
      where
        part x k                        || note: k is evaluated lazily instead of pattern matched, which would
            = (x : as, bs), if p x      || force its evaluation
            = (as, x : bs), otherwise
              where
                (as, bs) = k

span, break :: (* -> bool) -> [*] -> ([*], [*])
span p [] = ([], [])
span p xs
    = (x : ys, zs), if p x
    = ([], xs),     otherwise
      where
        x : xs'  = xs
        (ys, zs) = span p xs'

break p = span (not . p)

viewL :: [*] -> maybe (*, [*])
viewL []       = Nothing
viewL (x : xs) = Just (x, xs)

viewR :: [*] -> maybe ([*], *)
viewR
    = foldr go Nothing
      where
        go x Nothing         = Just ([], x)
        go x (Just (xs, x')) = Just (x : xs, x')

splitAt :: num -> [*] -> ([*], [*])
splitAt n xs
    = ([], xs),     if n <= 0 \/ null xs
    = (x : s1, s2), otherwise
      where
        x : xs'  = xs
        (s1, s2) = splitAt (n - 1) xs'

|| split a list into 2 (nearly) equal halves
split2 :: [*] -> ([*], [*])
split2 xs
    = go xs xs
      where
        go (x : xs) (y : y' : ys)
            = (x : xs', ys')
              where
                (xs', ys') = go xs ys
        go xs ys = ([], xs)

splitWhen :: (* -> bool) -> [*] -> [[*]]
splitWhen p
    = go
      where
        go xs
            = merge y (rest ys)
              where
                (y, ys) = break p xs

                merge [] ys   = ys
                merge y  ys   = y : ys

                rest []       = []
                rest (y : ys) = go ys

splitOneOf :: [*] -> [*] -> [[*]]
splitOneOf xs = splitWhen (member xs)

interleave :: [*] -> [*] -> [*]
interleave (a : as) (b : bs) = a : b : interleave as bs
interleave as       bs       = []

uninterleave :: [*] -> ([*], [*])
uninterleave
    = foldr ui ([], [])
      where
        ui x k
            = (x : ys, xs)
              where
                (xs, ys) = k

chunk :: num -> [*] -> [[*]]
chunk n xs
    = error ("bad chunk size: " ++ show n), if n < 1
    = [xs'],                                if null xs''
    = xs' : chunk n xs'',                   otherwise
      where
        (xs', xs'') = splitAt n xs

nub :: [*] -> [*]
nub [] = []
nub (x : xs) = x : nub (filter (~= x) xs)

length :: [*] -> num
length = (#)

intersperse :: * -> [*] -> [*]
intersperse x [] = []
intersperse x (y : ys)
    = y : intersperse' ys
      where
        intersperse' [] = []
        intersperse' (y : ys) = x : y : intersperse' ys

intercalate :: [*] -> [[*]] -> [*]
intercalate xs xss = concat (intersperse xs xss)

minBy :: (* -> **) -> [*] -> *
minBy f xs
    = (snd . foldl1 cmp . zip2 (map f xs)) xs
      where
        cmp a b = a, if fst a <= fst b
                = b, otherwise

maxBy :: (* -> **) -> [*] -> *
maxBy f xs
    = (snd . foldl1 cmp . zip2 (map f xs)) xs
      where
        cmp a b = a, if fst a >= fst b
                = b, otherwise

sortBy :: (* -> * -> ordering) -> [*] -> [*]
sortBy cmp
    = mergeAll . sequences
      where
        sequences (a : b : xs)
            = descending b [a]   xs, if cmp a b == GT
            = ascending  b (a :) xs, otherwise
        sequences xs = [xs]

        descending a as (b : bs) = descending b (a : as) bs, if cmp a b == GT
        descending a as bs       = (a : as) : sequences bs

        ascending a fa (b : bs)
            = ascending b fa' bs, if cmp a b ~= GT
              where fa' ys = fa (a : ys)

        ascending a fa bs = seq x (x : sequences bs) where x = fa [a]

        mergeAll [x] = x
        mergeAll xs  = mergeAll (mergePairs xs)

        mergePairs (a : b : xs) = seq x (x : mergePairs xs) where x = merge a b
        mergePairs xs           = xs

        merge [] bs = bs
        merge as [] = as
        merge as bs
            = b : merge as bs', if cmp a b == GT
            = a : merge as' bs, otherwise
              where
                (a : as') = as
                (b : bs') = bs

sortOn :: (* -> **) -> [*] -> [*]
sortOn f
    = map snd . sortBy (comparing fst) . map addOrder
      where
        addOrder x = y $seq (y, x) where y = f x

zipWith :: (* -> ** -> ***) -> [*] -> [**] -> [***]
zipWith fn (x : xs) (y : ys) = fn x y : zipWith fn xs ys
zipWith fn xs       ys       = []

unzip2 :: [(*, **)] -> ([*], [**])
unzip2 [] = ([], [])
unzip2 ((a, b) : ts) = (a : as, b : bs) where (as, bs) = unzip2 ts

unzip3 :: [(*, **, ***)] -> ([*], [**], [***])
unzip3 [] = ([], [], [])
unzip3 ((a, b, c) : ts) = (a : as, b : bs, c : cs) where (as, bs, cs) = unzip3 ts


|| string functions

split :: char -> string -> [string]
split c
    = foldr go []
      where
        go x r        = [] : r, if x == c
        go x []       = [[x]]
        go x (xs : t) = (x : xs) : t

words :: string -> [string]
words = splitWhen isSpace

|| add a suffix to a string if it isn't already present
withSuffix :: string -> string -> string
withSuffix suf str
    = str,                              if str == dotSuf
    = str ++ suf,                       if str == "."
    = str ++ dotSuf,                    if #str <= 2
    = hd str : withSuffix suf (tl str), otherwise
      where
        dotSuf = '.' : suf

|| remove a suffix from a string, if it is present
withoutSuffix :: string -> string -> string
withoutSuffix suf str
    = [],                                 if null str \/ str == dotSuf
    = hd str : withoutSuffix suf (tl str), otherwise
      where
        dotSuf = '.' : suf

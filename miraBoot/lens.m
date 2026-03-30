|| -*- mode: indented-text -*-
||
|| lens.m -- optics for accessing nested structures


%export +

%include "maybe"
%include "mirandaExtensions"


lens * **
    ::= Lens
        (* -> **)               || view function
        ((** -> **) -> * -> *)  || over function

view :: lens * ** -> * -> **
view (Lens viewf overf) = viewf

over :: lens * ** -> (** -> **) -> * -> *
over (Lens viewf overf) = overf

|| set written to directly use over to maximize inlining possibility
|| (since inlining is written to explicitly handle the "view" and "over" cases)
set :: lens * ** -> ** -> * -> *
set lns = over lns . const

|| compose outer lens with inner lens
composeLens :: lens * ** -> lens ** *** -> lens * ***
composeLens (Lens view_a over_a) (Lens view_b over_b)
    = Lens getf overf
      where
        getf x   = view_b (view_a x)
        overf fn = over_a (over_b fn)

|| lenses for 2-tuples
lensFst = Lens getf overf where getf (a, b) = a; overf fn (a, b) = (fn a, b)
lensSnd = Lens getf overf where getf (a, b) = b; overf fn (a, b) = (a, fn b)

|| lenses for 3-tuples
lensTup3_0 = Lens getf overf where getf (a, b, c) = a; overf fn (a, b, c) = (fn a, b, c)
lensTup3_1 = Lens getf overf where getf (a, b, c) = b; overf fn (a, b, c) = (a, fn b, c)
lensTup3_2 = Lens getf overf where getf (a, b, c) = c; overf fn (a, b, c) = (a, b, fn c)

|| lenses for 4-tuples
lensTup4_0 = Lens getf overf where getf (a, b, c, d) = a; overf fn (a, b, c, d) = (fn a, b, c, d)
lensTup4_1 = Lens getf overf where getf (a, b, c, d) = b; overf fn (a, b, c, d) = (a, fn b, c, d)
lensTup4_2 = Lens getf overf where getf (a, b, c, d) = c; overf fn (a, b, c, d) = (a, b, fn c, d)
lensTup4_3 = Lens getf overf where getf (a, b, c, d) = d; overf fn (a, b, c, d) = (a, b, c, fn d)

|| lenses for 5-tuples
lensTup5_0 = Lens getf overf where getf (a, b, c, d, e) = a; overf fn (a, b, c, d, e) = (fn a, b, c, d, e)
lensTup5_1 = Lens getf overf where getf (a, b, c, d, e) = b; overf fn (a, b, c, d, e) = (a, fn b, c, d, e)
lensTup5_2 = Lens getf overf where getf (a, b, c, d, e) = c; overf fn (a, b, c, d, e) = (a, b, fn c, d, e)
lensTup5_3 = Lens getf overf where getf (a, b, c, d, e) = d; overf fn (a, b, c, d, e) = (a, b, c, fn d, e)
lensTup5_4 = Lens getf overf where getf (a, b, c, d, e) = e; overf fn (a, b, c, d, e) = (a, b, c, d, fn e)

|| lenses for 6-tuples
lensTup6_0 = Lens getf overf where getf (a, b, c, d, e, f) = a; overf fn (a, b, c, d, e, f) = (fn a, b, c, d, e, f)
lensTup6_1 = Lens getf overf where getf (a, b, c, d, e, f) = b; overf fn (a, b, c, d, e, f) = (a, fn b, c, d, e, f)
lensTup6_2 = Lens getf overf where getf (a, b, c, d, e, f) = c; overf fn (a, b, c, d, e, f) = (a, b, fn c, d, e, f)
lensTup6_3 = Lens getf overf where getf (a, b, c, d, e, f) = d; overf fn (a, b, c, d, e, f) = (a, b, c, fn d, e, f)
lensTup6_4 = Lens getf overf where getf (a, b, c, d, e, f) = e; overf fn (a, b, c, d, e, f) = (a, b, c, d, fn e, f)
lensTup6_5 = Lens getf overf where getf (a, b, c, d, e, f) = f; overf fn (a, b, c, d, e, f) = (a, b, c, d, e, fn f)

|| lenses for 7-tuples
lensTup7_0 = Lens getf overf where getf (a, b, c, d, e, f, g) = a; overf fn (a, b, c, d, e, f, g) = (fn a, b, c, d, e, f, g)
lensTup7_1 = Lens getf overf where getf (a, b, c, d, e, f, g) = b; overf fn (a, b, c, d, e, f, g) = (a, fn b, c, d, e, f, g)
lensTup7_2 = Lens getf overf where getf (a, b, c, d, e, f, g) = c; overf fn (a, b, c, d, e, f, g) = (a, b, fn c, d, e, f, g)
lensTup7_3 = Lens getf overf where getf (a, b, c, d, e, f, g) = d; overf fn (a, b, c, d, e, f, g) = (a, b, c, fn d, e, f, g)
lensTup7_4 = Lens getf overf where getf (a, b, c, d, e, f, g) = e; overf fn (a, b, c, d, e, f, g) = (a, b, c, d, fn e, f, g)
lensTup7_5 = Lens getf overf where getf (a, b, c, d, e, f, g) = f; overf fn (a, b, c, d, e, f, g) = (a, b, c, d, e, fn f, g)
lensTup7_6 = Lens getf overf where getf (a, b, c, d, e, f, g) = g; overf fn (a, b, c, d, e, f, g) = (a, b, c, d, e, f, fn g)

|| lenses for 8-tuples
lensTup8_0 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = a; overf fn (a, b, c, d, e, f, g, h) = (fn a, b, c, d, e, f, g, h)
lensTup8_1 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = b; overf fn (a, b, c, d, e, f, g, h) = (a, fn b, c, d, e, f, g, h)
lensTup8_2 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = c; overf fn (a, b, c, d, e, f, g, h) = (a, b, fn c, d, e, f, g, h)
lensTup8_3 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = d; overf fn (a, b, c, d, e, f, g, h) = (a, b, c, fn d, e, f, g, h)
lensTup8_4 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = e; overf fn (a, b, c, d, e, f, g, h) = (a, b, c, d, fn e, f, g, h)
lensTup8_5 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = f; overf fn (a, b, c, d, e, f, g, h) = (a, b, c, d, e, fn f, g, h)
lensTup8_6 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = g; overf fn (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, fn g, h)
lensTup8_7 = Lens getf overf where getf (a, b, c, d, e, f, g, h) = h; overf fn (a, b, c, d, e, f, g, h) = (a, b, c, d, e, f, g, fn h)

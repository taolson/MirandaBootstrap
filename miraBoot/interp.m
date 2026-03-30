|| -*- mode: indented-text -*-
||
|| interp.m -- interpreter for STG code execution using Eval/Apply model
||
|| Note: interp is non-functional now that Miranda2 IO has switched from Miranda's sys_message to monadic IO
||       it will be hard to emulate monadic IO in terms of sys_message.
|| 
|| TODO: currently we are using a set for the table of roots that point from older space to newer space,
||       added to during execution of an Update (write barrier) however, some code causes many old->new roots
||       to be made, creating a large root set.  Instead, in the real runtime (and possibly
||       in an update to this interpreter), a "card table" should be used.


|| configuration
debugTrace      = False
statusTrace     = False
profile         = False
maxHeap         = 150000
statusFrequency = 100000

%export interp

%include "avl"
%include "bag"
%include "either"
%include "exception"
%include "grammar"
%include "io"
%include "lens"
%include "map"
%include "maybe"
%include "mirandaExtensions"
%include "module"
%include "name"
%include "set"
%include "stateStrict"
%include "stg"


|| the STG Machine state consists of:
|| the current codeObj being executed,
|| the argument values,
|| the environment values,
|| the (mutable) register values,
|| the continuation stack,
|| the heap,
|| the code,
|| the garbage-collection info,
|| and the execution stats
||
|| Note: data definitions with strict fields are used here instead of generic tuples to prevent space leaks due to lazy updating of fields

|| garbage collection info
gcInfo ::=
    GCInfo
        heapPtr!                || gcAllocNew -- allocation pointer in new space
        heapPtr!                || gcAllocOld -- allocation pointer in old space
        (s_set heapPtr)!        || gcRootsNew -- set of roots in old space pointing into new space
        (s_set heapPtr)!        || gcRootsOld -- set of roots in global space pointing into old/new space
        [value]!                || gcMark     -- list of marked values to track
        num!                    || gcOffset   -- heapPtr offset value used when moving heapPtrs during gcMajor
        num!                    || gcMinorCnt -- number of minor collections performed
        num!                    || gcMajorCnt -- number of major collections performed

|| lenses for gcInfo
gcAllocNew = Lens getf overf where getf (GCInfo a b c d e f g h) = a; overf fn (GCInfo a b c d e f g h) = GCInfo (fn a) b c d e f g h
gcAllocOld = Lens getf overf where getf (GCInfo a b c d e f g h) = b; overf fn (GCInfo a b c d e f g h) = GCInfo a (fn b) c d e f g h
gcRootsNew = Lens getf overf where getf (GCInfo a b c d e f g h) = c; overf fn (GCInfo a b c d e f g h) = GCInfo a b (fn c) d e f g h
gcRootsOld = Lens getf overf where getf (GCInfo a b c d e f g h) = d; overf fn (GCInfo a b c d e f g h) = GCInfo a b c (fn d) e f g h
gcMark     = Lens getf overf where getf (GCInfo a b c d e f g h) = e; overf fn (GCInfo a b c d e f g h) = GCInfo a b c d (fn e) f g h
gcOffset   = Lens getf overf where getf (GCInfo a b c d e f g h) = f; overf fn (GCInfo a b c d e f g h) = GCInfo a b c d e (fn f) g h
gcMinorCnt = Lens getf overf where getf (GCInfo a b c d e f g h) = g; overf fn (GCInfo a b c d e f g h) = GCInfo a b c d e f (fn g) h
gcMajorCnt = Lens getf overf where getf (GCInfo a b c d e f g h) = h; overf fn (GCInfo a b c d e f g h) = GCInfo a b c d e f g (fn h)


|| STG registers

frmReg ::= FrmReg heapPtr! num! || frame base and size

emptyFrm :: frmReg
emptyFrm = FrmReg 0 0

regVec ::=
    RegVec
        [value]!                || rvArg -- args vector
        [value]!                || rvEnv -- environment vector
        [value]!                || rvReg -- regs vector
        frmReg!                 || rvFrm -- frame base and size

|| lenses for regVec
rvArg = Lens getf overf where getf (RegVec a b c d) = a; overf fn (RegVec a b c d) = RegVec (fn a) b c d
rvEnv = Lens getf overf where getf (RegVec a b c d) = b; overf fn (RegVec a b c d) = RegVec a (fn b) c d
rvReg = Lens getf overf where getf (RegVec a b c d) = c; overf fn (RegVec a b c d) = RegVec a b (fn c) d
rvFrm = Lens getf overf where getf (RegVec a b c d) = d; overf fn (RegVec a b c d) = RegVec a b c (fn d)


|| execution histogram
execStats ::=
    ExecStats
        codePtr!                || statCp   -- current codeBlock being executed
        (b_bag codePtr)!        || statHist -- execution histogram

|| lenses for execStats
statCp   = Lens getf overf where getf (ExecStats a b) = a; overf fn (ExecStats a b) = ExecStats (fn a) b
statHist = Lens getf overf where getf (ExecStats a b) = b; overf fn (ExecStats a b) = ExecStats a (fn b)


|| STG Machine state
stgSt ::=
    StgSt
        [stgInsn]!              || stgBlock -- basic block being executed
        regVec!                 || stgRv    -- registers
        stgStack!               || stgStk   -- stack
        stgHeap!                || stgHp    -- heap
        stgCode!                || stgCd    -- code
        gcInfo!                 || stgGc    -- gcInfo
        execStats!              || stgStats -- execution statistics

|| lenses for stgSt
stgBlock = Lens getf overf where getf (StgSt a b c d e f g) = a; overf fn (StgSt a b c d e f g) = StgSt (fn a) b c d e f g
stgRv    = Lens getf overf where getf (StgSt a b c d e f g) = b; overf fn (StgSt a b c d e f g) = StgSt a (fn b) c d e f g
stgStk   = Lens getf overf where getf (StgSt a b c d e f g) = c; overf fn (StgSt a b c d e f g) = StgSt a b (fn c) d e f g
stgHp    = Lens getf overf where getf (StgSt a b c d e f g) = d; overf fn (StgSt a b c d e f g) = StgSt a b c (fn d) e f g
stgCd    = Lens getf overf where getf (StgSt a b c d e f g) = e; overf fn (StgSt a b c d e f g) = StgSt a b c d (fn e) f g
stgGc    = Lens getf overf where getf (StgSt a b c d e f g) = f; overf fn (StgSt a b c d e f g) = StgSt a b c d e (fn f) g
stgStat  = Lens getf overf where getf (StgSt a b c d e f g) = g; overf fn (StgSt a b c d e f g) = StgSt a b c d e f (fn g)

stgArg        = composeLens stgRv  rvArg
stgEnv        = composeLens stgRv  rvEnv
stgReg        = composeLens stgRv  rvReg
stgFrm        = composeLens stgRv  rvFrm
stgGcAllocNew = composeLens stgGc  gcAllocNew
stgGcAllocOld = composeLens stgGc  gcAllocOld
stgGcRootsNew = composeLens stgGc  gcRootsNew
stgGcRootsOld = composeLens stgGc  gcRootsOld
stgMark       = composeLens stgGc  gcMark
stgGcOffset   = composeLens stgGc  gcOffset
stgGcMinorCnt = composeLens stgGc  gcMinorCnt
stgGcMajorCnt = composeLens stgGc  gcMajorCnt
stgHeapMap    = composeLens stgHp  lensFst
stgHeapGlbl   = composeLens stgHp  lensSnd      || top of heap area for global top-level values (implicit roots for major collection)
stgCodeMap    = composeLens stgCd  lensFst
stgCp         = composeLens stgStat statCp
stgHist       = composeLens stgStat statHist

|| lift lens operations into stgSt
stg_view :: lens stgSt * -> state stgSt *
stg_view lns st = (view lns st, st)

stg_set :: lens stgSt * -> * -> state stgSt ()
stg_set lns v st = pair () $strictApply set lns v st

stg_over :: lens stgSt * -> (* -> *) -> state stgSt ()
stg_over lns f st = pair () $strictApply over lns f st

|| a combination of st_over and st_bind -- perform an st_bind operation on a state value focused by lens
stg_obind :: lens stgSt * -> (* -> state stgSt *) -> state stgSt ()
stg_obind lns f = stg_view lns $st_bind f $st_kbind stg_set lns


|| garbage collection for the heap, based upon "Simple Generational Garbage Collection and Fast Allocation" paper by Andrew Appel
||
|| the heap is divided into 3 sections:
||    global: at the bottom of the heap; these are Fclosures and Tclosures generated during compilation, and have
||            references to them in immediate values in the code section.  Therefore they are at fixed addresses
||            and cannot move.  Any global thunks which are updated are placed in the gcRootsNew or gcRootsOld lists.
||
||    old:    bottom half of collectable space, between heapGlbl and gcAllocOld, holds heapObjs that have survived a
||            gcMinor collection
||
||    new:      top half of collectable space, between gcAllocOld and gcAllocNew; holds newly-created heapObjs

|| remove entries >= hfree from the heap (performed after garbage collection has compacted live entries)
|| this is only done here to keep the heap as compact as possible during interp; in a real runtime the
|| space wouldn't need to be cleared
gcRemoveFree :: heapPtr -> state stgSt ()
gcRemoveFree hfree
    = stg_over stgHeapMap (m_fromList . filter ((< hfree) . fst) . m_toList)

|| move a heapPtr to the top of the old area, if it isn't already moved, and return the new heapPtr
gcMoveAddr :: heapPtr -> state stgSt heapPtr
gcMoveAddr hp
    = st_bind4 (stg_view stgHeapGlbl) (stg_view stgGcOffset) (stg_view stgGcAllocOld) (stgLookupHeap hp) doMove
      where
        doMove gb off a obj
            = st_pure hp, if inMajor & hp < gb \/ ~inMajor & hp < a  || not in area being relocated
               where
                inMajor = off > 0       || in a major GC if there is an offset, minor GC otherwise

        doMove gb off a (Hfwd af, env) = st_pure af               || object has already been moved; return its forwarding address

        doMove gb off a obj
            = stg_over stgGcAllocOld (+ sz)  $st_right   || allocate the space for the obj
              stgCopyHeap a hp               $st_right   || move obj to new addr
              stgUpdateHeap hp (Hfwd a', []) $st_right   || insert an Hfwd to a' at hp
              st_pure a'
              where
                sz      = heapObjWords obj
                a'      = a - off       || adjust the pointer value by the offset (used during major GC to account for the final block copy)

|| move a heapPtr if it is in the heapMap (can be triggered by attempting to move contiguous heap addr ranges where only part exist)
gcMaybeMoveAddr :: heapPtr -> state stgSt heapPtr
gcMaybeMoveAddr hp st
    = (hp, st),         if isNothing mobj
    = gcMoveAddr hp st, otherwise
      where
        mobj = m_lookup hp (view stgHeapMap st)

|| move heapPtr values, leaving all other values alone
gcMoveValue :: value -> state stgSt value
gcMoveValue (Haddr a) = Haddr $st_fmap gcMoveAddr a
gcMoveValue v         = st_pure v

|| trace a heapObject, moving any heapPtr values
gcTraceAddr :: heapPtr -> state stgSt ()
gcTraceAddr hp
    = stgLookupHeap hp $st_bind traceObj $st_kbind stgUpdateHeap hp
      where
        traceObj (Hpap hp, env)
            = mkPap $st_fmap st_mapM gcMoveValue (Haddr hp : env)
              where mkPap (Haddr hp' : env') = (Hpap hp', env')

        || move the values of the array contiguously to hp (the space was already allocated during the
        || initial move of the array base, and there are no direct references to the individual array
        || elements, so none of them will have been moved to break their contiguous alignment)
        traceObj (Harray sz, env)
            = pair (Harray sz)                            $st_fmap
              (st_mapM_ (gcTraceAddr) [hp + 1 .. hp + sz] $st_right
              st_mapM gcMoveValue env)

        traceObj (Hfwd a, env) = error ("gcTraceAddr: tracing " ++ shownum hp ++ " encountered an Hfwd to " ++ shownum a)
        traceObj (info, env)   = (pair info) $st_fmap st_mapM gcMoveValue env

|| trace from all the roots, moving directly-reachable objects in the new area to the old area
gcTraceRoots :: lens stgSt (s_set heapPtr) -> state stgSt ()
gcTraceRoots gcRoots
    = stg_obind stgFrm  gcMoveFrame           $st_right   || move frame first to ensure contiguous allocation
      stg_obind gcRoots gcTraceRootSet        $st_right   || trace from each entry in the gcRoots set
      stg_obind stgMark (st_mapM gcMoveValue) $st_right
      stg_obind stgArg  (st_mapM gcMoveValue) $st_right
      stg_obind stgEnv  (st_mapM gcMoveValue) $st_right
      stg_obind stgReg  (st_mapM gcMoveValue) $st_right
      stg_obind stgStk  (st_mapM gcMoveCont)  $st_right
      st_pure ()
      where

        || move a stack continuation by moving its env
        gcMoveCont (cp, env) = pair cp $st_fmap st_mapM gcMoveValue env

        || move sz heapPtrs starting at hp, then return the first moved heapPtr
        || Note: uses gcMaybeMoveAddr, since not all of the addresses in the range may actually be present
        || due to changing the heap allocation to bump the gc new ptr by more than 1
        gcMoveFrame (FrmReg hp sz)
            = st_pure (FrmReg hp sz),                                     if sz <= 0
            = mkFrm $st_fmap st_mapM gcMaybeMoveAddr [hp .. hp + sz - 1], otherwise
              where
                mkFrm (hp' : hps) = FrmReg hp' sz

        gcTraceRootSet s
            = st_mapM_ gcTraceAddr (s_toList s) $st_right st_pure s

|| trace all heapObjs between hbot and the gcAllocOld ptr, moving any new area heapPtr values to old and updating their addresses
gcTraceFrom :: heapPtr -> state stgSt heapPtr
gcTraceFrom hbot
    = stg_view stgGcAllocOld $st_bind go
      where
        go htop
            = st_pure hbot,                                                if hbot >= htop
            = gcTraceAddr hbot $st_right stgLookupHeap hbot $st_bind next, otherwise

        next obj
            = gcTraceFrom hbot'
              where
                hbot' = hbot + heapObjWords obj || Note: this should really use a tag value in the Hinfo, but it is fixed enough
                                                || that we can rely upon the heapObjWords calculation

|| do a minor garbage collect cycle, tracing roots into the new area and moving reachable heapObjs from new to old
gcMinor :: state stgSt ()
gcMinor
    = st_bind2 (stg_view stgHeapGlbl) (stg_view stgGcAllocOld) go
      where
        go glblTop oldTop
            = (gcTraceRoots stgGcRootsNew $st_right
               updateRoots glblTop        $st_right
               gcTraceFrom oldTop)        $st_bind
               endTrace glblTop

        || add any roots from stgGcRootsNew that are from the global space into stgGcRootsOld, and clear stgGcRootsNew
        updateRoots glblTop
            = st_bind2 (stg_view stgGcRootsOld) (stg_view stgGcRootsNew) addRoots
              where
                addRoots old new
                    = stg_set stgGcRootsOld old' $st_right stg_set stgGcRootsNew s_empty
                      where
                        old' = foldl (converse s_insert) old (filter (< glblTop) (s_toList new))

        endTrace hglbl htop
            = gcMajor htop,                    if htop >= maxHeap div 2
            = gcRemoveFree hfree           $st_right
              stg_set  stgGcAllocNew hfree $st_right
              stg_over stgGcMinorCnt (+ 1) $st_right
              st_pure (),                      otherwise
                where
                  hfree = htop + (maxHeap - htop) div 2

|| do a major garbage collect cycle, tracing all reachable heapObjs and moving reachable heapObjs from old to new, then swap new and old
gcMajor :: heapPtr -> state stgSt ()
gcMajor hfree
    = stg_view stgHeapGlbl $st_bind go
      where
        go hglbl
          = (stg_set stgGcOffset off     $st_right
             stg_set stgGcAllocOld hfree $st_right
             gcTraceRoots stgGcRootsOld  $st_right
             gcTraceFrom hfree)          $st_bind
             endTrace hglbl
             where
               off = hfree - hglbl                      || offset used to adjust heapPtrs to their final location during gcMove/gcTrace

        || at this point, all of the traceable heap (not including the global space at the bottom) is
        || in hfree .. htop-1, with globals from 0 .. hglbl-1; need to block move from hfree down to hglbl to
        || make the old space contiguous, again.  heapPtrs in the traceable heap range have been adjusted to
        || account for the block move.
        endTrace hglbl htop
            = error "gcMajor: out of memory",      if htop >= maxHeap
            = blockMove hfree hglbl size       $st_right
              gcRemoveFree hfree'              $st_right
              stg_set  stgGcOffset 0           $st_right
              stg_set  stgGcAllocOld htop'     $st_right
              stg_set  stgGcAllocNew hfree'    $st_right
              stg_over stgGcMajorCnt (+ 1),        otherwise
              where
                size   = htop - hfree                           || size of non hglobl old space
                htop'  = hglbl + size                           || new top of old space (gcAllocOld)
                hfree' = htop' + (maxHeap - htop') div 2        || new free space (gcAllocNew)

                blockMove src dst n
                    = stg_view stgHeapMap $st_bind go src dst n
                      where
                        go src dst n hm
                            = st_pure () $st_left stg_set stgHeapMap hm,              if n <= 0
                            = sz $seq hm' $seq go (src + sz) (dst + sz) (n - sz) hm', otherwise   || make blockMove strict in the heapMap
                              where
                                obj = fromJust (m_lookup src hm)
                                sz  = heapObjWords obj
                                hm' = m_insert dst obj hm

|| perform a writeBarrier on a heapObj and a list of values, adding the heapObj
|| to the approprate root list if any of the values is a heapPtr which is in a
|| newer space than the heapObj
gcWriteBarrier :: heapPtr -> [value] -> state stgSt ()
gcWriteBarrier hp vs
    = st_bind2 (stg_view stgHeapGlbl) (stg_view stgGcAllocOld) addRoot
      where
      addRoot glblTop oldTop
          = stg_over stgGcRootsNew (s_insert hp), if hp < oldTop  & any (valueAbove oldTop)  vs        || writing to glbl or old space, value in new space
          = stg_over stgGcRootsOld (s_insert hp), if hp < glblTop & any (valueAbove glblTop) vs        || writing to glbl space, value in old space
          = st_pure (),                           otherwise
            where
              valueAbove lim (Haddr hp') = hp' >= lim
              valueAbove lim v           = False


|| common operations on refs and values

|| get a value from an opr, making access strict to prevent space leaks of captured values
getValue :: opr -> state stgSt value
getValue (Imm v) = st_pure v
getValue (Ref r)
    = go r
      where
        go (Arg, n) = choose n  $st_fmap stg_view stgArg
        go (Env, n) = choose n  $st_fmap stg_view stgEnv
        go (Reg, n) = choose n  $st_fmap stg_view stgReg
        go (Frm, n) = idxHeap n $st_fmap stg_view stgFrm

        || make lookup of the value strict in the reg vector to prevent capture of old reg vectors in the heap
        choose  i xs            = xs ! i
        idxHeap i (FrmReg a sz) = Haddr $strictApply (a + i)


|| set a register ref to a value
setReg :: ref -> value -> state stgSt ()
setReg (Reg, n) v
    = stg_over stgReg (setAtFill n v)
      where
        || NOTE: custom setAt which fills earlier non-existant values with Word 0
        setAtFill n v [] = rep n (Word 0) ++ [v]
        setAtFill n v (x : xs)
            = v : xs,           if n == 0
            = xs' $seq x : xs', otherwise
              where
                xs' = setAtFill (n - 1) v xs


|| code, and heap

stgLookupCode :: codePtr -> state stgSt codeObj
stgLookupCode cp st = ((fromJust . m_lookup cp . view stgCodeMap) st, st)

stgLookupHeap :: heapPtr -> state stgSt heapObj
stgLookupHeap hp st
    = error ("stgLookupHeap failed for " ++ shownum hp), if isNothing mobj
    = (fromJust mobj, st),                               otherwise
      where
        mobj = m_lookup hp (view stgHeapMap st)

showEnv :: [value] -> string
showEnv = show

|| update the destination heapPtr with the heapObj
stgUpdateHeap :: heapPtr -> heapObj -> state stgSt ()
stgUpdateHeap hp obj = stg_over stgHeapMap (m_insert hp obj)

|| copy from src heapPtr to dest heapPtr (copying the entire array, if src is an Harray)
|| note: this assumes that there is correctly allocated contiguous free space in the destination
stgCopyHeap :: heapPtr -> heapPtr -> state stgSt ()
stgCopyHeap dhp shp
    = stgLookupHeap shp $st_bind doCopy
      where
        || copy array tag word + array contents
        doCopy (Harray sz, env)
            = go dhp shp sz
              where
                go da sa n = st_pure (),            if n < 0
                   = (stgLookupHeap sa $st_bind
                     stgUpdateHeap da) $st_right
                     go (da + 1) (sa + 1) (n - 1),  otherwise

        || copy obj
        doCopy obj = stgUpdateHeap dhp obj

|| allocate n contiguous entries in the new area of the heap and return the base address,
|| performing a garbage collection if necessary
stgAllocHeap :: num -> state stgSt heapPtr
stgAllocHeap n
    = st_bind2 (stg_view stgGcAllocNew) (stg_view stgCp) go
      where
        go ap cp
            = stg_over stgGcAllocNew (+ n) $st_right st_pure ap, if ap + n < maxHeap
            = gcMinor $st_right stgAllocHeap n,                  otherwise

|| allocate heap, marking a list of values to track if a GC occurs, and returning their new values
stgAllocHeapMarked :: num -> [value] -> state stgSt (heapPtr, [value])
stgAllocHeapMarked n vs
    = stg_set stgMark vs                                 $st_right
      st_liftA2 pair (stgAllocHeap n) (stg_view stgMark) $st_left
      stg_set stgMark []

|| make a closure in the heap, allocating a new heapPtr if the reference is a Reg, or using an
|| already allocated frame slot reference if the reference is a Frm
|| takes a post-processing function to update the heapPtr with the correct info
|| heap allocation occurs before getting the values of the oprs, because it could trigger a GC and move addrs around
makeClosure :: ref -> [opr] -> (heapPtr -> [value] -> state stgSt ()) -> state stgSt ()
makeClosure rf oprs update
    = st_bind2 getHeap (st_mapM getValue oprs) update   || note: getHeap may trigger a gc which moves addresses, so get values of oprs after getHeap
      where
        getHeap
            = stgAllocHeap sz $st_bind updateReg, if isReg rf
            = getPtr $st_fmap getValue (Ref rf), otherwise
              where
                sz                = #oprs + 2   || size of closure is currently always #oprs + 2 (tag word + ptr word)
                updateReg hp      = setReg rf (Haddr hp) $st_right st_pure hp
                getPtr (Haddr hp) = hp

|| default update of a closure with a heapInfo
updateClosure :: heapInfo -> heapPtr -> [value] -> state stgSt ()
updateClosure hinfo hp env = stgUpdateHeap hp (hinfo, env)

|| IO continuation to force a return to the top-level for IO processing, by preventing a Ret insn
|| from popping the continuation stack to refill the stgBlock
ioCont :: continuation
ioCont = (-1, [])


|| common control-flow operations

|| perform a jump to codePtr, optionally setting Args and Env
execJump :: codePtr -> maybe [opr] -> maybe [value] -> state stgSt ()
execJump cp oprs env
    = setBlock $st_right
      doArgs   $st_right
      doEnv    $st_right
      doRegs   $st_right        || drop all but first 3 Regs and clear the frame register, to prevent space leak of dead reg values during GC
      doProf
      where
        setBlock      = stg_set stgCp cp $st_right stgLookupCode cp $st_bind (stg_set stgBlock . snd)
        doArgs        = when (isJust oprs) (st_mapM getValue (fromJust oprs) $st_bind stg_set stgArg)
        doEnv         = when (isJust env)  (stg_set stgEnv (fromJust env))
        doRegs        = when (isJust oprs \/ isJust env) (stg_over stgReg (take 3) $st_right stg_set stgFrm emptyFrm)
        doProf        = when profile (stg_over stgStat update)

        update (ExecStats cp' hist)
            = ExecStats cp (b_insert cp hist)

        when False f  = st_pure ()
        when True  f  = f

|| perform a return to a continuation
execRet :: state stgSt ()
execRet
    = stg_view stgStk $st_bind popCont
      where
        popCont ((cp, env) : cs)
            = stg_set stgStk cs $st_right execJump cp Nothing (Just env), if cp >= 0


        popCont x = st_pure ()  || no continuation to return to, or blocked by an io continuation; return to top level

|| perform an enter to a known closure
execEnter :: heapPtr -> [opr] -> state stgSt ()
execEnter hp oprs
    = setReg r2Ref (Haddr hp) $st_right stgLookupHeap hp $st_bind getCpEnv
      where
        getCpEnv (Hfun cp ar, env) = execJump cp (Just oprs) (Just env)
        getCpEnv (Hthunk cp, env)  = execJump cp (Just oprs) (Just env)
        getCpEnv obj               = error ("exec: bad object in Enter: " ++ showHeapObj obj)

|| Apply an unknown closure to the values of the oprs (common to Apply and ApplyToEnv) by looking up its heapInfo.
|| if the closure is under-applied, make an Hpap object and return it
|| if the closure is over-applied, push an applyToEnv continuation with the extra args, and apply the closure with
|| the correct number of args
|| also handle application of special heap objects Hfbalt and Hpap, and Word values
execApply :: opr -> [opr] -> state stgSt ()
execApply fOpr oprs
    = getValue fOpr $st_bind getObj
      where
        getObj (Haddr hp)
            = stgLookupHeap hp $st_bind checkObj hp oprs       || hp and oprs may change during processing
              where
                checkObj hp oprs (Hfun cp ar, env) = checkArity hp oprs ar cp env
                checkObj hp oprs (Hthunk cp, env)  = checkArity hp oprs 0  cp env

                || handle a partial-application closure by looking up its heapInfo and appending the oprs
                || to the existing environment, then re-checking the new heapObj
                checkObj hp oprs (Hpap hp', env)
                    = doRet hp,                                      if null oprs
                    = stgLookupHeap hp' $st_bind checkObj hp' oprs', otherwise
                      where
                        oprs' = map Imm env ++ oprs

                checkObj hp oprs hobj
                    = doRet hp, if null oprs
                    = err,      otherwise
                      where
                        err = error ("exec Apply: cannot apply " ++ showHeapObj hobj)

                || use the heapInfo to choose the correct runtime call form
                checkArity hp oprs ar cp env
                    = go oprs,                             if noprs == ar       || correct number of arguments
                    = pushAppEnv oprs2 $st_right go oprs1, if noprs > ar        || over-applied; push extra args and call with required number
                    = doRet hp,                            if null oprs         || under-applied with no args; 
                    = mkPap $st_bind doRet,                otherwise            || under-applied with args; make an Hpap and return it
                      where
                        noprs          = #oprs
                        (oprs1, oprs2) = splitAt ar oprs
                        go oprs        = setReg r2Ref (Haddr hp) $st_right execJump cp (Just oprs) (Just env)

                doRet hp = setReg r0Ref (Haddr hp) $st_right setReg r1Ref (Word 0) $st_right execRet

                || over applied: push a continuation to a generic codeObj which performs an ApplyToEnv on the remaining args
                pushAppEnv oprs
                    = st_mapM getValue oprs $st_bind go
                      where
                        go env = stg_over stgStk ((applyToEnvPtr, env) :)

                || under applied: make a partial-application heap object which holds the args and return its heapPtr
                || Note: heap allocation could trigger a GC which moves addrs of the oprs around, so temporarily mark
                || them so that the GC can track them
                mkPap
                    = st_mapM getValue (fOpr : oprs) $st_bind stgAllocHeapMarked sz $st_kbind updateHeap
                      where
                        sz = #oprs + 2

                        updateHeap (hp, Haddr hp' : env)
                            = stgUpdateHeap hp (Hpap hp', env) $st_right st_pure hp

        getObj v = error ("exec Apply: cannot apply " ++ showValue v)

|| perform a binary operator on word values (Haddr values also included so that they can be printed by showint)
execBinOp :: (num -> num -> num) -> ref -> opr -> opr -> state stgSt ()
execBinOp op r aOpr bOpr
    = st_bind2 (getValue aOpr) (getValue bOpr) go
      where
        rawValue (Word n)  = n
        rawValue (Haddr a) = a

        go a b = (setReg r . Word) (rawValue a $op rawValue b)

|| perform a bit-wise boolean operation on word values
|| if a value is negative, perform it as if it is represented as a 64-bit 2's complement value,
|| then convert it back to a signed representation
execBitOp :: (num -> num -> num) -> ref -> opr -> opr -> state stgSt ()
execBitOp op r aOpr bOpr
    = st_bind2 (getValue aOpr) (getValue bOpr) doBitOp
      where
        rawValue (Word n)
            = n + 2^64, if n < 0
            = n,        otherwise

        rawValue (Haddr a)
            = a + 2^64, if a < 0
            = a,        otherwise

        signed n
            = n - 2^64, if n >= 2^63
            = n,        otherwise

        bitwise op a b
            = go a b 64
              where
                go a b 0 = 0
                go a b n = op (a mod 2) (b mod 2) + (go (a div 2) (b div 2) (n - 1)) * 2

        doBitOp a b = (setReg r . Word . signed) (bitwise op (rawValue a) (rawValue b))

|| perform a shift operation on word values (Haddr values also included so that they can be printed by showint)
|| left shift is a * 2^b, right shift is a div 2^b
execShiftOp :: (num -> num -> num) -> ref -> opr -> opr -> state stgSt ()
execShiftOp op r aOpr bOpr
    = st_bind2 (getValue aOpr) (getValue bOpr) doShiftOp
      where
        rawValue (Word n)  = n
        rawValue (Haddr a) = a

        doShiftOp a b = (setReg r . Word) (rawValue a $op (2 ^ rawValue b))


|| execute an instruction
exec :: stgInsn -> state stgSt ()

|| tail call to a known codePtr (no free vars) with values of oprs
exec (Call cp oprs) = execJump cp (Just oprs) Nothing

|| enter a known closure with values of oprs
exec (Enter hp oprs) = execEnter hp oprs

|| enter a known closure indirectly through fopr with values of oprs
exec (EnterInd fOpr oprs)
    = getValue fOpr $st_bind go
      where
        go (Haddr hp) = execEnter hp oprs

|| apply an unknown closure
exec (Apply fOpr oprs) = execApply fOpr oprs

|| apply unknown closure in r0 to env
exec ApplyToEnv
    = stg_view stgEnv $st_bind (execApply r0Opr . map Imm)

|| fail to saved context in fopr, unwinding the stack to the saved stack
exec (Fail fOpr)
    = getValue fOpr $st_bind getObj
      where
        unwindTo sz
            = stg_over stgStk unwind
              where
                unwind stk = drop (#stk - sz) stk

        getObj (Haddr hp)
            = stgLookupHeap hp $st_bind doFail
              where
                doFail (Hfbalt cp sz, env) = unwindTo sz $st_right execJump cp Nothing (Just env)

|| return to a continuation, copying values of valOpr and arityOpr to r0, r1
exec (Ret valOpr arityOpr)
    = (getValue valOpr   $st_bind setReg r0Ref) $st_right
      (getValue arityOpr $st_bind setReg r1Ref) $st_right
      execRet

|| return to a continuation, copying the env to r0, r1, and args
exec RetEnv
    = stg_view stgEnv $st_bind go
      where
        go (v : a : args)
            = setReg r0Ref v      $st_right
              setReg r1Ref a      $st_right
              stg_set stgArg args $st_right
              execRet

|| vectored jump into a list of codePtrs, based upon iOpr value
exec (CaseVec iOpr cps)
    = getValue iOpr $st_bind go
      where
        go (Word idx) = execJump (cps ! idx) Nothing Nothing

|| unconditional jump to codePtr
exec (Jmp cp) = execJump cp Nothing Nothing

|| jump to codePtr if aOpr == bOpr
exec (Jeq aOpr bOpr cp)
    = st_bind2 (getValue aOpr) (getValue bOpr) go
      where
        go (Word a) (Word b)
            = execJump cp Nothing Nothing, if a == b
            = st_pure (),                  otherwise

|| jump to codePtr if aOpr < bOpr
exec (Jlt aOpr bOpr cp)
    = st_bind2 (getValue aOpr) (getValue bOpr) go
      where
        go (Word a) (Word b)
            = execJump cp Nothing Nothing, if a < b
            = st_pure (),                  otherwise

|| PopEnv has no effect in interp
exec (PopEnv x) = st_pure ()

|| push a continuation closure on the stack with codePtr and the values of oprs as the env
exec (PushCont cp oprs)
    = st_mapM getValue oprs $st_bind go
      where
        go env = env $seq stg_over stgStk ((cp, env) :)

|| allocate N contiguous heapPtrs in the heap and store their base address and size in the frame register
exec (AllocFrame n)
    = stgAllocHeap n $st_bind updateReg
      where
        updateReg hp = hp $seq stg_set stgFrm (FrmReg hp n)

|| make an Hfun closure in the heap
exec (Fclosure rf cp ar oprs) = makeClosure rf oprs (updateClosure (Hfun cp ar))

|| make an Hthunk closure in the heap
exec (Tclosure rf cp oprs) = makeClosure rf oprs (updateClosure (Hthunk cp))

|| make an Hfbalt closure in the heap
|| Note: getting stack before making the closure is ok, here, because we are only using its length
|| as a proxy for a stack pointer; the values in the stack (which could change due to a GC) are unused
exec (Sclosure rf cp oprs)
    = stg_view stgStk $st_bind go
      where
        go stk = makeClosure rf oprs (updateClosure (Hfbalt cp (#stk)))

|| make an value closure (Hthunk) in the heap, with r0, r1, args as the env
exec (Vclosure rf cp)
    = makeClosure rf [r0Opr, r1Opr] getArgs
      where
        getArgs hp env
            = stg_view stgArg $st_bind go
              where
                go args = stgUpdateHeap hp (Hthunk cp, env ++ args)

|| perform an update on the heapPtr, replacing its codePtr and its env with r0, r1, and current Args
|| check the update with a gcWriteBarrier to add it to the approprate roots table, if required
exec (Update hpOpr cpOpr)
    = st_bind5 (getValue hpOpr) (getValue cpOpr) (getValue r0Opr) (getValue r1Opr) (stg_view stgArg) doUpdate
      where
        doUpdate (Haddr hp) (Caddr cp) v0 v1 args
            = stgUpdateHeap hp (Hthunk cp, env) $st_right gcWriteBarrier hp env
              where
                env = v0 $seq v1 $seq args $seq v0 : v1 : args

|| perform a binary operation on word values
exec (Mov r v)     = getValue v $st_bind setReg r
exec (Add r va vb) = execBinOp (+)   r va vb
exec (Sub r va vb) = execBinOp (-)   r va vb
exec (Mul r va vb) = execBinOp (*)   r va vb
exec (Div r va vb) = execBinOp (div) r va vb
exec (Mod r va vb) = execBinOp (mod) r va vb

exec (Cmp r va vb)
    = execBinOp cmp r va vb
      where
        cmp a b
            = 0, if a == b
            = 1, if a < b
            = 2, otherwise

|| perform bit-wise boolean operations on word values
exec (Band r va vb)
    = execBitOp band r va vb
      where
        band a b = 0, if a == 0 \/ b == 0
                 = 1, otherwise

exec (Bor r va vb)
    = execBitOp bor r va vb
      where
        bor a b = 0, if a == 0 & b == 0
                = 1, otherwise

exec (Bxor r va vb)
    = execBitOp bxor r va vb
      where
        bxor a b = 0, if a == b
                 = 1, otherwise

exec (Bnot r va)    = exec (Bxor r va (Imm (Word (-1))))
exec (Bshl r va vb) = execShiftOp (*)   r va vb
exec (Bshr r va vb) = execShiftOp (div) r va vb

|| select an arg value based upon the index in iOpr
exec (Sel r iOpr)
    = st_bind2 (getValue iOpr) (stg_view stgArg) go
      where
        go (Word idx) args = setReg r (args ! idx)

|| allocate a new, empty Hstream in the heap
exec (AllocStream r fdOpr szOpr)
    = stgAllocHeap 1 $st_bind update    || Note: see size hack for Hstream in stg2 heapObjSize
      where
        update hp = setReg r (Haddr hp) $st_right stgUpdateHeap hp (Hstream [], [])

|| read a byte from a byte stream
exec (ReadStream r sOpr)
    = getValue sOpr $st_bind getStrm
      where
        getStrm (Haddr hp)
            = stgLookupHeap hp $st_bind doRead
              where
                doRead (Hstream [], x)       = setReg r (Word (-1))
                doRead (Hstream (c : cs), x) = setReg r (Word (code c)) $st_right stgUpdateHeap hp (Hstream cs, x)

|| write a byte to a byte stream
exec (WriteStream r sOpr cOpr)
    = getValue sOpr $st_bind doWrite
      where
        doWrite (Haddr hp)                 = st_bind2 (stgLookupHeap hp) (getValue cOpr) (update hp)
        update hp (Hstream cs, x) (Word c) = setReg r (Word 0) $st_right stgUpdateHeap hp (Hstream (cs ++ [decode c]), x)

|| allocate a new contiguous array of specified size (uninitialized)
exec (AllocArray r sOpr)
    = getValue sOpr $st_bind getHeap
      where
        getHeap (Word sz) = stgAllocHeap (sz + 1) $st_bind update sz
        update sz hp      = stgUpdateHeap hp (Harray sz, []) $st_right setReg r (Haddr hp)

|| fill an allocated array with a constant value
exec (FillArray aOpr vOpr)
    = getValue aOpr $st_bind getArray
      where
        getArray (Haddr hp)        = st_bind2 (stgLookupHeap hp) (getValue vOpr) (doFill hp)
        doFill hp (Harray sz, x) v = st_mapM_ (update v) [hp + 1 .. hp + sz]
        update v hp                = stgUpdateHeap hp (Hvalue, [v])

|| copy array contents from src array to dst array (memcpy)
exec (CopyArray dOpr sOpr)
    = st_bind2 (getValue dOpr) (getValue sOpr) getArrays
      where
        getArrays (Haddr da) (Haddr sa) = st_bind2 (stgLookupHeap da) (stgLookupHeap sa) (doCopy da sa)

        doCopy da sa (Harray dn, dx) (Harray sn, sx)
            = error "CopyArray: sizes don't match", if dn ~= sn
            = stgCopyHeap da sa,                    otherwise

        doCopy da sa do so = error ("copyArray: can't copy " ++ showHeapObj do ++ " <- " ++ showHeapObj so)

|| read a value from the array at the given index
exec (ReadArray r aOpr iOpr)
    = getValue aOpr $st_bind getArray
      where
        getArray (Haddr hp)                = st_bind2 (stgLookupHeap hp) (getValue iOpr) (doRead hp)

        doRead hp (Harray sz, x) (Word ix)
            = stgLookupHeap (hp + ix + 1) $st_bind update, if ix < sz
            = error "exec ReadArray index out of bounds",  otherwise

        update (Hvalue, [v]) = setReg r v
        update obj           = error "exwc ReadArray element not an Hvalue"

|| write a value to the array at the given index
exec (WriteArray aOpr iOpr vOpr)
    = getValue aOpr $st_bind getArray
      where
        getArray (Haddr hp) = st_bind3 (stgLookupHeap hp) (getValue iOpr) (getValue vOpr) (doWrite hp)

        doWrite hp (Harray sz, x) (Word ix) v
            = stgUpdateHeap (hp + ix + 1) (Hvalue, vs) $st_right gcWriteBarrier hp vs
              where
                vs = [v]

|| exec a system operation
exec (SystemOp n r sOpr)
    = getValue sOpr $st_bind go n
      where

        || exit (not implemented, yet)
        go 0 x = st_pure ()

        || openFileRead
        go 1 (Haddr hp)
            = stgLookupHeap hp $st_bind doReadFile hp
              where
                doReadFile hp (Hstream fn, x) = setReg r (Haddr hp) $st_right stgUpdateHeap hp (Hstream (read fn), x)

        || openFileWrite (no operation in interp)
        go 2 x = st_pure ()

        || openFileAppend (no operation in interp)
        go 3 x = st_pure ()

        || closeFile (no operation in interp)
        go 4 x = st_pure ()

        || readFile (openFile buffered the entire file, so readFile just returns an EOF (0)
        go 5 x = setReg r (Word 0)

        || writeFile (not implemented, yet)
        go 6 x = setReg r (Word 0)

        || mtimeFile (not implemented, yet)
        go 7 x = setReg r (Word 0)

        || getArg (not implemented, yet)
        go 8 x = setReg r (Word 0)

exec insn = error ("exec: cannot execute " ++ showStgInsn insn)


|| step the STG state by executing one instruction, or returning False if there are no more instructions
step :: num -> stgSt -> ios (bool, stgSt)
step cycles st
    = go (view stgBlock st)
      where
        go [] = io_pure (False, st)

        go (i : is)
            = io_msg traceMsg $io_right doExec, if debugTrace & cycles > 0
            = doExec,                           otherwise
              where 
                doExec   = (io_pure . pair True) $strictApply st_execState (stg_set stgBlock is $st_right exec i) st

                traceMsg = "\ncycles: " ++ shownum cycles ++ "\n" ++
                           showsLocInfo loc"\n" ++ showStgInsn i ++ "\n" ++
                           "Args: " ++ showValueVec (view stgArg st) ++ "\n" ++
                           "Regs: " ++ showValueVec (view stgReg st) ++ "\n" ++
                           "Env:  " ++ showValueVec (view stgEnv st) ++ "\n" ++
                           "Stk:  " ++ showStack    (view stgStk st) ++ "\n"
                loc            = fst (st_evalState (stg_view stgCp $st_bind stgLookupCode) st)
                showValueVec v = intercalate ", " (map showValue v)
                showStack    s = intercalate ", " (map showCont  s)
                showCont     c = showStackObj c

|| run the STG state by continually stepping, checking for status output or handling top-level IO
|| in between steps
run :: num -> stgSt -> ios (num, stgSt)
run cycles st
    = io_msg statusMsg $io_right go, if statusTrace & cycles > 0 & cycles mod statusFrequency == 0
    = io_msgc ""       $io_right go, if cycles > 0 & cycles mod 10000 == 0      || DEBUG; this cleans up out-of-heap errors when statusTrace == False
    = go,                            otherwise
      where
        go        = step cycles st $io_bind checkIO

        statusMsg = "\n<<cycles: " ++ shownum cycles ++ " minor GCs: " ++ shownum minorCnt ++ " major GCs: " ++ shownum majorCnt ++
                    " free: " ++ shownum free ++ " new roots: " ++ shownum rootsNew ++ " old roots: " ++ shownum rootsOld ++ ">>"

        cycles'   = cycles $seq (cycles + 1)
        minorCnt  = view stgGcMinorCnt st
        majorCnt  = view stgGcMajorCnt st
        anew      = view stgGcAllocNew st
        rootsNew  = s_size (view stgGcRootsNew st)
        rootsOld  = s_size (view stgGcRootsOld st)
        free      = maxHeap - anew

        || continue with stepping, if step says there is more code to execute, otherwise
        || execution has returned a value to the top level; check tag and arity to determine what to do
        checkIO (True, st') = run cycles' st'   || more insns in codeObj to execute; continue with execution
        checkIO (False, st')                    || no more insns; check tag and arity of returned value to see how to proceed
            = doCons   cycles' st' arg0 arg1,   if tag == 1 & arity == 2  || (:) constructor; eval arg0 to be output, then continue with arg1
            = doChar   cycles' st' stk  arg0,   if tag == 0 & arity == 1  || C# word#; output word# value as char, then continue
            = checkStk cycles' st' stk,         if tag == 0 & arity == 0  || empty list; end of output
              where
                args             = view stgArg st'
                regs             = view stgReg st'
                stk              = view stgStk st'
                Word tag         = regs ! 0
                Word arity       = regs ! 1
                arg0             = args ! 0
                arg1             = args ! 1

        || push a continuation to evaluate arg1, then an io continuation to force a return back to checkIO,
        || then continue with evaluation of arg0 to evaluate the head of the string (will eval to a C# char constructor)
        || by pushing the continuation for arg1 on the stack, its heapPtr gets tracked during any GC that might occur
        || during evaluation of arg0
        doCons cycles st (Haddr hp0) arg1
            = run cycles ((set stgBlock [Enter hp0 []] . over stgStk ([ioCont, (enterEnv0Ptr, [arg1])] ++)) st)

        || output a character, then check the stack for continuation or exit to top level
        doChar cycles st stk (Word ch)
            = io_msgc [decode ch] $io_right checkStk cycles st stk

        || if the continuation stack is empty, return to the top level, otherwise
        || pop the io continuation and execute a Ret to return to the next context
        checkStk cycles st stk
            = io_pure (cycles, st),                                      if null stk  || no context to return to; exit to top-level
            = run cycles ((set stgBlock [Ret z z] . over stgStk tl) st), otherwise    || pop the ioCont context and return
              where
                z = Imm (Word 0)

interp :: name -> (localEnv, stgCode, stgHeap) -> ios string
interp start (lenv, cd, hp)
    = io_msg ("running  " ++ showName start) $io_right run 0 initSt $io_bind showStats
      where
        Imm startVal   = fromJust (m_lookup start lenv)
        Haddr startPtr = startVal
        oldTop         = snd hp
        newTop         = oldTop + (maxHeap - oldTop) div 2

        initSt
            = StgSt
                [Enter startPtr []]
                (RegVec [] [] [] emptyFrm)
                []
                hp
                cd
                (GCInfo newTop oldTop s_empty s_empty [] 0 0 0)
                (ExecStats 0 b_empty)

        showStats (cycles, st)
            = io_msg ("\n*** STG simulation cycles: " ++ shownum cycles) $io_right
              showProfile st                                             $io_right
              io_pure []

        showProfile st
            = io_msg ("*** Execution profile:\n") $io_right
              io_mapM showEntry sortedEntries, if profile
            = io_pure [],                      otherwise
              where
                cm                = view stgCodeMap st
                b                 = view stgHist st
                (mb, tot)         = foldl merge (b_empty, 0) (b_toList b)
                filteredEntries   = filter highCount (b_toList mb)
                sortedEntries     = sortBy (descending snd) filteredEntries
                highCount (cp, n) = n * 100 >= tot
                showEntry (k, v)  = io_msg (shownum v ++ " (" ++ showPct v ++ "%) " ++ showsLocInfo k "")
                showPct n         = shownumDigits 2 p where p = (n * 100 / tot)

                merge (b, s) (cp, n)
                    = b' $seq s' $seq (b', s')
                      where
                        (loc, x) = fromJust (m_lookup cp cm)
                        b'       = b_insertTimes loc n b
                        s'       = s + n

                shownumDigits d n
                    = intDigits ++ "." ++ take d (fracDigits ++ repeat '0')
                      where
                        [intDigits, fracDigits] = split '.' (shownum n)

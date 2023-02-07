module Mark4 where

import Syntax
import Parser ( parse )
import Heap ( Addr, Heap, hInitial, hAlloc, hLookup, hUpdate, hAddresses, heapStatsNumAllocs, heapStatsNumUpdates, heapStatsNumFrees )
import Assoc ( Assoc, aLookup )
import TIStats
    ( tiStatGetSteps,
      tiStatInitial,
      tiStatIncSteps,
      TIStats,
      tiStatIncScReductions, tiStatIncCurStackDepth, tiStatDecCurStackDepth, tiStatGetScReductions, tiStatGetPrimitiveReductions, tiStatGetMaxStackDepth )
import UsefulFuns ( mapAccuml )
import ISeq

-------- TIState definition
-- the state contains the elements below
type TIState = (TIStack, TIDump, TIHeap, TIGlobals, TIStats)

-- the spine stack, a stack of head addresses
type TIStack = [Addr]

-- Dump is not used in mark, so its just a dummy
type TIDump = [TIStack]
initialTIDump :: TIDump
initialTIDump = []

type TIHeap = Heap Node
data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
          | NInd Addr
          | NPrim Name Primitive
    deriving (Show)

data Primitive = Neg | Add | Sub | Mul | Div deriving (Show)

-- TIGlobals associates each supercombinator with the address of a heap node containing its definition
type TIGlobals = Assoc Name Addr

-- TIStats is defined in TIStats.hs
applyToStats :: (TIStats -> TIStats) -> TIState -> TIState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)


--------- actual compiler
run :: CoreProgram -> String
run = showResults . eval . compile

compile :: CoreProgram -> TIState
compile program = (initial_stack, initialTIDump, initial_heap, globals, tiStatInitial)
    where
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        initial_stack = [address_of_main]
        address_of_main = aLookup globals "main" (error "main is not defined")

extraPreludeDefs = []
buildInitialHeap :: [CoreScDefn] -> (TIHeap, TIGlobals)
buildInitialHeap sc_defs = (heap2, sc_addrs ++ prim_addrs)
    where
        (heap1, sc_addrs) = mapAccuml allocateSc hInitial sc_defs
        (heap2, prim_addrs) = mapAccuml allocatePrim heap1 primitives

primitives :: Assoc Name Primitive
primitives = [ ("negate", Neg),
               ("+", Add), ("-", Sub),
               ("*", Mul), ("/", Div)]

allocatePrim :: TIHeap -> (Name, Primitive) -> (TIHeap, (Name, Addr))
allocatePrim heap (name, prim) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NPrim name prim)

allocateSc :: TIHeap -> CoreScDefn -> (TIHeap, (Name, Addr))
allocateSc heap (name, args, body) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NSupercomb name args body)

eval :: TIState -> [TIState]
eval state = state : rest_states
    where
        rest_states = if tiFinal state then [] else eval next_state
        next_state = doAdmin (step state)

doAdmin :: TIState -> TIState
doAdmin = applyToStats tiStatIncSteps

tiFinal :: TIState -> Bool
tiFinal ([sole_addr], [], heap, globals, stats) = isDataNode (hLookup heap sole_addr) -- dump must be empty
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TIState -> TIState
step state@(stack, dump, heap, globals, stats) = dispatch (hLookup heap (head stack))
    where
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NSupercomb sc args body) = scStep state sc args body
        dispatch (NInd addr) = indStep state addr
        dispatch (NPrim name prim) = primStep state prim

numStep :: TIState -> Int -> TIState
numStep ([node_addr], d:ds, heap, globals, stats) n = (d, ds, heap, globals, stats) -- pop top element of dump and use as new stack TODO check if top element is NNum?
numStep state _ = error (showResults [state] ++ "\n" ++  "Number applied as function!")

apStep :: TIState -> Addr -> Addr -> TIState
apStep (stack@(a:s), dump, heap, globals, stats) a1 a2 = getResult (hLookup heap a2)
    where
        getResult (NInd a3) = (stack, dump, hUpdate heap a (NAp a1 a3), globals, stats)
        getResult _ = (a1 : stack, dump, heap, globals, tiStatIncCurStackDepth stats)

scStep :: TIState -> Name -> [Name] -> CoreExpr -> TIState
scStep (stack, _, _, _, _) sc_name arg_names _ | length stack < length arg_names + 1 
    = error ("Too few arguments for supercombinator " ++ sc_name ++ "!")
scStep (stack, dump, heap, globals, stats) sc_name arg_names body 
    = (new_stack, dump, new_heap, globals, new_stats)
    where
        new_stack = drop (length arg_names) stack
        root_redex = head new_stack
        new_heap = instantiateAndUpdate body root_redex heap env
        env = arg_bindings ++ globals
        arg_bindings = zip arg_names (getArgs heap stack)
        new_stats = tiStatDecCurStackDepth (length arg_names + 1) (tiStatIncScReductions stats)

indStep :: TIState -> Addr -> TIState
indStep (ind_node_addr : rest_stack, dump, heap, global, stats) addr
    = (new_stack, dump, heap, global, stats)
    where
        new_stack = addr : rest_stack

primStep :: TIState -> Primitive -> TIState
primStep state Neg = primNeg state
primStep state Add = primArith state (+)
primStep state Sub = primArith state (-)
primStep state Mul = primArith state (*)
primStep state Div = primArith state div

primNeg :: TIState -> TIState
primNeg (stack, dump, heap, globals, stats) = getResult (isDataNode arg_node)
    where
        arg_addr = head (getArgs heap stack)
        arg_node = hLookup heap arg_addr
        getResult True = (new_stack, dump, new_heap, globals, stats)
            where
                new_stack = tail stack
                root_redex = head new_stack
                (NNum n) = arg_node
                new_heap = hUpdate heap root_redex (NNum (-n))
        getResult False = (new_stack, new_dump, heap, globals, stats)
            where
                new_dump = stack : dump -- push stack to dump
                new_stack = [arg_addr]  -- stack whose only element is the arg to neg
                -- TODO setStackDepth to 1 in stats!!

primArith :: TIState -> (Int -> Int -> Int) -> TIState
primArith (stack, dump, heap, globals, stats) fun = getResult (isDataNode a1_node) (isDataNode a2_node)
    where
        [a1_addr, a2_addr] = getArgs heap stack
        a1_node = hLookup heap a1_addr
        a2_node = hLookup heap a2_addr
        getResult True True = (new_stack, dump, new_heap, globals, stats)
            where
                new_stack = drop 2 stack
                root_redex = head new_stack
                (NNum n1) = a1_node
                (NNum n2) = a2_node
                new_heap = hUpdate heap root_redex (NNum (fun n1 n2))
        getResult False _ = (new_stack, new_dump, heap, globals, stats)
            where
                new_dump = stack : dump
                new_stack = [a1_addr]
                -- TODO setStackDepth to 1 in stats!!
        getResult _ False = (new_stack, new_dump, heap, globals, stats)
            where
                new_dump = stack : dump
                new_stack = [a2_addr]
                -- TODO setStackDepth to 1 in stats!!
                


getArgs :: TIHeap -> TIStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
    where
        get_arg addr = arg 
            where
                (NAp fun arg) = hLookup heap addr

instantiateAndUpdate :: CoreExpr -> Addr -> TIHeap -> Assoc Name Addr -> TIHeap
instantiateAndUpdate (ENum n) upd_addr heap env = hUpdate heap upd_addr (NNum n)
instantiateAndUpdate (EAp e1 e2) upd_addr heap env
    = hUpdate heap2 upd_addr (NAp a1 a2)
        where
            (heap1, a1) = instantiate e1 heap env
            (heap2, a2) = instantiate e2 heap1 env
instantiateAndUpdate (EVar v) upd_addr heap env = hUpdate heap upd_addr (hLookup heap (aLookup env v (error ("Undefined name " ++ show v))))
instantiateAndUpdate (EConstr tag arity) upd_addr heap env = instantiateConstr tag arity heap env
instantiateAndUpdate (ELet isrec defs body) upd_addr heap env = instantiateAndUpdate body upd_addr new_heap new_env
    where
        (new_heap, new_env) = foldr instantiateDef (heap, env) defs
        instantiateDef (name, expr) (heap, env) = (new_heap, (name, addr):env)
            where
                (new_heap, addr) = instantiate expr heap new_env
instantiateAndUpdate (ECase e alts) upd_addr heap env = fst (instantiate (ECase e alts) heap env)

instantiate :: CoreExpr -> TIHeap -> Assoc Name Addr -> (TIHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
    where
        (heap1, a1) = instantiate e1 heap env
        (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Mark 4 can't instantiate case exprs!"

instantiateConstr tag arity heap env = error "Mark 4 can't instantiate constructors!"
instantiateLet False defs body heap env = instantiate body new_heap new_env
    where
        (new_heap, new_env) = foldr instantiateDef (heap, env) defs
        instantiateDef (name, expr) (heap, env) = (new_heap, (name, addr):env)
            where
                (new_heap, addr) = instantiate expr heap env

instantiateLet True defs body heap env = instantiate body new_heap new_env
    where
        (new_heap, new_env) = foldr instantiateDef (heap, env) defs
        instantiateDef (name, expr) (heap, env) = (new_heap, (name, addr):env)
            where
                (new_heap, addr) = instantiate expr heap new_env

showResults :: [TIState] -> String
showResults states = iDisplay (iConcat [iLayn (map showState states), showStats (last states)])

showState :: TIState -> ISeq
showState (stack, dump, heap, globals, stats) = iConcat [ 
    showStack heap stack, iNewline,
    showHeap heap, iNewline ]

showHeap :: TIHeap -> ISeq
showHeap heap = iConcat [ iStr "Heap [", iIndent (iInterleave iNewline (map show_heap_item addrs)), iStr " ]"]
    where
        addrs = hAddresses heap
        show_heap_item addr = iConcat [ showFWAddr addr, iStr ": ", showNode (hLookup heap addr) ]

showStack :: TIHeap -> TIStack -> ISeq
showStack heap stack = iConcat [ iStr "Stk  [", iIndent (iInterleave iNewline (map show_stack_item stack)), iStr " ]"]
    where
        show_stack_item addr = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: TIHeap -> Node -> ISeq
showStkNode heap (NAp fun_addr arg_addr) = iConcat [ iStr "NAp ", showFWAddr fun_addr, iStr " ", showFWAddr arg_addr, iStr " (", showNode (hLookup heap arg_addr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: Node -> ISeq
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", Mark4.showAddr a1, iStr " ", Mark4.showAddr a2 ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n
showNode (NInd addr) = iStr "NNum " `iAppend` showAddr addr
showNode (NPrim name primtive) = iStr "NPrim " `iAppend` iStr name

showAddr :: Addr -> ISeq
showAddr addr = iStr (show addr)

showFWAddr :: Addr -> ISeq
showFWAddr addr = iStr (replicate (4 - length str) ' ' ++ str)
    where
        str = show addr

showStats :: TIState -> ISeq
showStats (stack, dump, heap, globals, stats) 
    = iConcat [ iNewline, iNewline, 
                iStr "Total number of steps = ", iNum (tiStatGetSteps stats), iNewline,
                iStr "Number of scReductions = ", iNum (tiStatGetScReductions stats), iNewline,
                iStr "Number of primitive reductions = ", iNum (tiStatGetPrimitiveReductions stats), iNewline,
                iStr "Maximum stack depth = ", iNum (tiStatGetMaxStackDepth stats), iNewline,
                iStr "Number of heap allocations = ", iNum (heapStatsNumAllocs heap), iNewline,
                iStr "Number of heap updates = ", iNum (heapStatsNumUpdates heap), iNewline,
                iStr "Number of heap frees = ", iNum (heapStatsNumFrees heap) ]

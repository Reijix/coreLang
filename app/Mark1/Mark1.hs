module Mark1 where

import Syntax
import Parser
import Heap
import Assoc
import TIStats
import UsefulFuns
import PrettyPrint
import GHC.IO.Handle (hLookAhead)

-------- TIState definition
-- the state contains the elements below
type TIState = (TIStack, TIDump, TIHeap, TIGlobals, TIStats)

-- the spine stack, a stack of head addresses
type TIStack = [Addr]

-- Dump is not used in mark1, so its just a dummy
data TIDump = DummyTIDump
    deriving (Show)
initialTIDump :: TIDump
initialTIDump = DummyTIDump

type TIHeap = Heap Node
data Node = NAp Addr Addr
          | NSupercomb Name [Name] CoreExpr
          | NNum Int
    deriving (Show)

-- TIGlobals associates each supercombinator with the address of a heap node containing its definition
type TIGlobals = Assoc Name Addr

-- TIStats is defined in TIStats.hs
applyToStats :: (TIStats -> TIStats) -> TIState -> TIState
applyToStats stats_fun (stack, dump, heap, sc_defs, stats) = (stack, dump, heap, sc_defs, stats_fun stats)


--------- actual compiler
run :: String -> String
run input = showResults . eval . compile $ fromLeft (parse input)

fromLeft :: Either CoreProgram String -> CoreProgram
fromLeft (Left prog) = prog
fromLeft (Right msg) = error msg  

compile :: CoreProgram -> TIState
compile program = (initial_stack, initialTIDump, initial_heap, globals, tiStatInitial)
    where
        sc_defs = program ++ preludeDefs ++ extraPreludeDefs
        (initial_heap, globals) = buildInitialHeap sc_defs
        initial_stack = [address_of_main]
        address_of_main = aLookup globals "main" (error "main is not defined")

extraPreludeDefs = []
buildInitialHeap :: [CoreScDefn] -> (TIHeap, TIGlobals)
buildInitialHeap = mapAccuml allocateSc hInitial

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
tiFinal ([sole_addr], dump, heap, globals, stats) = isDataNode (hLookup heap sole_addr)
tiFinal ([], dump, heap, globals, stats) = error "Empty stack!"
tiFinal state = False

isDataNode :: Node -> Bool
isDataNode (NNum _) = True
isDataNode _        = False

step :: TIState -> TIState
step state = dispatch (hLookup heap (head stack))
    where
        (stack, dump, heap, globals, stats) = state
        dispatch (NNum n) = numStep state n
        dispatch (NAp a1 a2) = apStep state a1 a2
        dispatch (NSupercomb sc args body) = scStep state sc args body

numStep :: TIState -> Int -> TIState
numStep state n = error "Number applied as function!"

apStep :: TIState -> Addr -> Addr -> TIState
apStep (stack, dump, heap, globals, stats) a1 a2 = (a1 : stack, dump, heap, globals, stats)

scStep :: TIState -> Name -> [Name] -> CoreExpr -> TIState
scStep (stack, dump, heap, globals, stats) sc_name arg_names body = (new_stack, dump, new_heap, globals, stats)
    where
        new_stack = result_addr : drop (length arg_names + 1) stack
        (new_heap, result_addr) = instantiate body heap env
        env = arg_bindings ++ globals
        arg_bindings = zip arg_names (getArgs heap stack)

getArgs :: TIHeap -> TIStack -> [Addr]
getArgs heap (sc:stack) = map get_arg stack
    where
        get_arg addr = arg 
            where
                (NAp fun arg) = hLookup heap addr

instantiate :: CoreExpr -> TIHeap -> Assoc Name Addr -> (TIHeap, Addr)
instantiate (ENum n) heap env = hAlloc heap (NNum n)
instantiate (EAp e1 e2) heap env = hAlloc heap2 (NAp a1 a2)
    where
        (heap1, a1) = instantiate e1 heap env
        (heap2, a2) = instantiate e2 heap1 env
instantiate (EVar v) heap env = (heap, aLookup env v (error ("Undefined name " ++ show v)))
instantiate (EConstr tag arity) heap env = instantiateConstr tag arity heap env
instantiate (ELet isrec defs body) heap env = instantiateLet isrec defs body heap env
instantiate (ECase e alts) heap env = error "Can't instantiate case exprs"

instantiateConstr tag arity heap env = error "Can't instantiate constructors yet"
instantiateLet isrec defs body heap env = error "Can't instantiate let(rec)s yet"

showResults :: [TIState] -> String
showResults states = iDisplay (iConcat [iLayn (map showState states), showStats (last states)] :: ISeqRep)

showState :: (ISeq b) => (Show b) => TIState -> b
showState (stack, dump, heap, globals, stats) = iConcat [ showStack heap stack, iNewline ]

printState :: TIState -> String
printState state = iDisplay (showState state :: ISeqRep)

showStack :: (ISeq b) => (Show b) => TIHeap -> TIStack -> b
showStack heap stack = iConcat [ iStr "Stk [", iIndent (iInterleave iNewline (map show_stack_item stack)), iStr " ]"]
    where
        show_stack_item addr = iConcat [ showFWAddr addr, iStr ": ", showStkNode heap (hLookup heap addr)]

showStkNode :: (ISeq b) => (Show b) => TIHeap -> Node -> b
showStkNode heap (NAp fun_addr arg_addr) = iConcat [ iStr "NAp ", showFWAddr fun_addr, iStr " ", showFWAddr arg_addr, iStr " (", showNode (hLookup heap arg_addr), iStr ")" ]
showStkNode heap node = showNode node

showNode :: (ISeq b) => (Show b) => Node -> b
showNode (NAp a1 a2) = iConcat [ iStr "NAp ", Mark1.showAddr a1, iStr " ", Mark1.showAddr a2 ]
showNode (NSupercomb name args body) = iStr ("NSupercomb " ++ name)
showNode (NNum n) = iStr "NNum " `iAppend` iNum n

showAddr :: (ISeq b) => Addr -> b
showAddr addr = iStr (show addr)

showFWAddr :: (ISeq b) => Addr -> b
showFWAddr addr = iStr (replicate (4 - length str) ' ' ++ str)
    where
        str = show addr

showStats :: (ISeq b) => (Show b) => TIState -> b
showStats (stack, dump, heap, globals, stats) = iConcat [ iNewline, iNewline, iStr "Total number of steps = ", iNum (tiStatGetSteps stats) ]

{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use zipWith" #-}
module TIM3 (run, fullRun) where

import Syntax
import Heap
import Assoc
import ISeq
import Debug.Trace (trace)
import UsefulFuns (mapAccuml)

-- definitions --------------------------------------------------------------------------

data Instruction
    = Take Int Int
    | Move Int TAMode
    | Enter TAMode
    | Push TAMode
    | PushV ValueAMode
    | Return
    | Op Op
    | Cond [Instruction] [Instruction]

data Op = Add | Sub | Mul | Div | Neg
        | Gt | Ge | Lt | Le | Eq | Ne deriving (Eq, Show)

data ValueAMode = FramePtr | IntVConst Int deriving (Show)

data TAMode
    = Arg Int
    | Label String
    | Code [Instruction]
    | IntConst Int

data TState = TState {
    tInstr :: [Instruction],
    tFramePtr :: FramePtr,
    tStack :: TStack,
    tVStack :: TVStack,
    tDump :: TDump,
    tHeap :: THeap,
    tCodeStore :: CodeStore,
    tStats :: TStats
    }

data FramePtr
    = FrameAddr Addr
    | FrameInt Int
    | FrameNull

type TStack = [Closure]
type Closure = ([Instruction], FramePtr)
type TVStack = [Int]
data TDump = DummyTDump
type Frame = [Closure]

type THeap = Heap Frame
fAlloc :: THeap -> [Closure] -> (THeap, FramePtr)
fGet :: THeap -> FramePtr -> Int -> Closure
fUpdate :: THeap -> FramePtr -> Int -> Closure -> THeap
fList :: Frame -> [Closure]
fAlloc heap xs = (heap', FrameAddr addr)
    where
        (heap', addr) = hAlloc heap xs
fGet heap (FrameAddr addr) n = f !! (n-1)
    where f = hLookup heap addr
fUpdate heap (FrameAddr addr) n closure = hUpdate heap addr newFrame
    where
        frame = hLookup heap addr
        newFrame = take (n-1) frame ++ [closure] ++ drop n frame
fUpdate _ fptr _ _ = error $ "fUpdate called with wrong fptr: " ++ iDisplay (showFramePtr fptr)
fList f = f

type CodeStore = Assoc Name [Instruction]
codeLookup :: CodeStore -> Name -> [Instruction]
codeLookup cstore l = aLookup cstore l (error $ "Attempt to jump to unknown label " ++ show l)

type TStats = Int
statInitial :: TStats
statIncSteps :: TStats -> TStats
statGetSteps :: TStats -> Int
statInitial = 0
statIncSteps s = s+1
statGetSteps s = s

-- compiler -----------------------------------------------------------------------------

compile :: CoreProgram -> TState
compile program = TState
    [Enter (Label "main")]
    FrameNull
    initialArgStack
    initialValueStack
    initialDump
    hInitial
    compiledCode
    statInitial
    where
        scDefs = preludeDefs ++ program
        compiledScDefs = map (compileSc initialEnv) scDefs
        compiledCode = compiledScDefs ++ compiledPrimitives
        initialEnv = [(name, Label name) | (name, args, body) <- scDefs] ++ [(name, Label name) | (name, code) <- compiledPrimitives]
initialArgStack :: TStack
initialArgStack = [([], FrameNull)]
initialValueStack :: TVStack
initialValueStack = []
initialDump :: TDump
initialDump = DummyTDump
compiledPrimitives :: [(Name, [Instruction])]
compiledPrimitives = [] -- map mkPrim2 binOps
    --where mkPrim2 (name, op) = (name, [Take 2, Push (Code [Push (Code [Op op, Return]), Enter (Arg 1)]), Enter (Arg 2)])

binOps :: Assoc Name Op
binOps = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("==", Eq), ("~=", Ne), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)]
type TCompilerEnv = [(Name, TAMode)]

compileSc :: TCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSc env (name, args, body) = (name, instructions')
    where
        instructions' = Take d n : is
        (d, is) = compileR body newEnv n
        n = length args
        newEnv = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TCompilerEnv -> Int -> (Int, [Instruction])
compileR e@(EAp (EAp (EAp (EVar "if") cond) i1) i2) env d = compileB cond env (dmax, [Cond ist ise])
    where
        (dt, ist) = compileR i1 env d
        (de, ise) = compileR i2 env d
        dmax = max dt de
compileR e@(EAp (EAp (EVar op) e1) e2) env d | op `elem` aDomain binOps = compileB e env (d, [Return])
compileR e@(ENum _) env d = compileB e env (d, [Return])
compileR e@(EAp (EVar "neg") e1) env d = compileB e env (d, [Return])
compileR (EAp e1 e2) env d = (d2, Push am : is)
    where
        (d1, am) = compileA e2 env d
        (d2, is) = compileR e1 env d1
compileR e@(EVar v) env d = (d', [Enter am])
    where (d', am) = compileA e env d
compileR (ELet isRec defs e) env d = (d', moves ++ is)
    where
        (dn, moves) = mapAccuml makeMove (d + length defs) (zip defs frameSlots)
        (d', is) = compileR e newEnv dn
        newEnv = zip (map fst defs) (map mkIndMode frameSlots) ++ env
        makeMove d ((name, rhs), frameSlot) = (d', Move frameSlot am) where (d', am) = compileA rhs rhsEnv d
        rhsEnv | isRec = newEnv
               | otherwise = env
        frameSlots = [d+1..d+length defs]
compileR e env n = error $ "compileR: not implemented yet: " ++ show e

compileA :: CoreExpr -> TCompilerEnv -> Int -> (Int, TAMode)
compileA (EVar v) env d | v `elem` aDomain env = (d, aLookup env v (error "compileA: cant happen"))
compileA (ENum n) env d = (d, IntConst n)
compileA e env d = (d', Code is)
    where (d', is) = compileR e env d

compileB :: CoreExpr -> TCompilerEnv -> (Int, [Instruction]) -> (Int, [Instruction])
compileB (EAp (EAp (EVar op) e1) e2) env (d, cont) = compileB e2 env $ compileB e1 env (d, Op (aLookup binOps op (error "compileB: Op not found")) : cont)
compileB (EAp (EVar "neg") e1) env (d, cont) = compileB e1 env (d, Op Neg : cont)
compileB (ENum n) env (d, cont) = (d, PushV (IntVConst n) : cont)
compileB e env (d, cont) = (d', Push (Code cont) : is)
    where (d', is) = compileR e env d

mkIndMode :: Int -> TAMode
mkIndMode n = Code [Enter (Arg n)]

-- evaluator ----------------------------------------------------------------------------

eval :: TState -> [TState]
eval state | trace ("State is:\n" ++ iDisplay (showState state)) True = state : restStates
    where
        restStates | tFinal state = []
                   | otherwise = eval nextState
        nextState = doAdmin (step state)

doAdmin :: TState -> TState
doAdmin = applyToStats statIncSteps

tFinal :: TState -> Bool
tFinal state | null $ tInstr state = True
             | otherwise = False

applyToStats :: (TStats -> TStats) -> TState -> TState
applyToStats fun state = state {tStats = fun $ tStats state}

step :: TState -> TState
step (TState (Take t n : instr) fptr stack vstack dump heap cstore stats) | length stack >= n && t >= n = TState instr fptr' (drop n stack) vstack dump heap' cstore stats
                                                                          | otherwise = error "step: Too few args for Take instructions"
    where
        closures = take n stack 
        (heap', fptr') = fAlloc heap (closures ++ replicate (t-n) cDummy)
        cDummy = ([], FrameNull)
step (TState (Move i a : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr stack vstack dump heap' cstore stats
    where
        heap' = fUpdate heap fptr i (amToClosure a fptr heap cstore)
step (TState [Enter am] fptr stack vstack dump heap cstore stats) = TState instr' fptr' stack vstack dump heap cstore stats
    where (instr', fptr') = amToClosure am fptr heap cstore
step (TState (Push am : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr (amToClosure am fptr heap cstore : stack) vstack dump heap cstore stats
step (TState (PushV FramePtr : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr stack (getNum fptr:vstack) dump heap cstore stats
    where
        getNum FrameNull = error "PushV called with fptr = FrameNull, expected FrameInt"
        getNum (FrameInt n) = n
        getNum (FrameAddr a) = error "PushV called with fptr = FrameAddr, expected FrameInt"
step (TState ((PushV (IntVConst n)) : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr stack (n:vstack) dump heap cstore stats
step (TState [Return] fptr stack vstack dump heap cstore stats) = TState i' f' s vstack dump heap cstore stats
    where (i', f'):s = stack
step (TState (Op Neg : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr stack (negate n1 : vs) dump heap cstore stats
    where (n1:vs) = vstack
step (TState (Op op : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr stack (f n1 n2 : vs) dump heap cstore stats
    where
        (n1:n2:vs) = vstack
        f = aLookup builtInOps op (error $ "Op: no such builtin " ++ show op)
step (TState [Cond i1 i2] fptr stack vstack dump heap cstore stats) = TState i' fptr stack vs dump heap cstore stats
    where 
        (v:vs) = vstack
        i' = if v == 0 then i1 else i2
step st = error $ "step: found\n" ++ iDisplay (showInstructions Full (tInstr st))

amToClosure :: TAMode -> FramePtr -> THeap -> CodeStore -> Closure
amToClosure (Arg n) fptr heap cstore = fGet heap fptr n
amToClosure (Code il) fptr heap cstore = (il, fptr)
amToClosure (Label l) fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = [PushV FramePtr, Return]

builtInOps :: Assoc Op (Int -> Int -> Int)
builtInOps = [
    (Add, (+)), (Sub, (-)), (Mul, (*)), (Div, div), (Eq, comp (==)), (Ne, comp (/=)), (Lt, comp (<)), (Le, comp (<=)), (Gt, comp (>)), (Ge, comp (>=))
    ]
    where 
        comp :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
        comp op a b = if op a b then 0 else 1


-- printing -----------------------------------------------------------------------------
showFullResults :: [TState] -> String
showFullResults states = iDisplay (iConcat [
    iStr "Supercombinator definitions", iNewline, iNewline,
    showScDefns firstState, iNewline, iNewline,
    iStr "State transitions", iNewline,
    iLayn (map showState states), iNewline, iNewline,
    showStats (last states)
    ])
    where (firstState:restState) = states

showResults states = iDisplay (iConcat [
    showState lastState, iNewline, iNewline, showStats lastState
    ])
    where lastState = last states

showScDefns :: TState -> ISeq
showScDefns (TState instr fptr stack vstack dump heap cstore stats) = iInterleave iNewline (map showSc cstore)

showSc :: (Name, [Instruction]) -> ISeq
showSc (name, il) = iConcat [
    iStr "Code for ", iStr name, iStr ":", iNewline,
    iStr "  ", showInstructions Full il, iNewline, iNewline
    ]

showState :: TState -> ISeq
showState (TState instr fptr stack vstack dump heap cstore stats) = iConcat [
    iStr "Code: ", showInstructions Full instr, iNewline,
    showFrame heap fptr,
    showStack stack,
    showValueStack vstack,
    showDump dump,
    iNewline
    ]

showFrame :: THeap -> FramePtr -> ISeq
showFrame heap FrameNull = iStr "Null frame ptr" `iAppend` iNewline
showFrame heap (FrameAddr addr) = iConcat [
    iStr "Frame: <",
    iIndent (iInterleave iNewline (map showClosure (fList (hLookup heap addr)))),
    iStr ">", iNewline
    ]
showFrame heap (FrameInt n) = iConcat [iStr "Frame ptr (int): ", iNum n, iNewline]

showStack :: TStack -> ISeq
showStack stack = iConcat [
    iStr "Arg stack: [",
    iIndent (iInterleave iNewline (map showClosure stack)),
    iStr "]", iNewline
    ]

showValueStack :: TVStack -> ISeq
showValueStack vstack = iConcat [
    iStr "Value stack: [",
    iIndent (iInterleave (iStr ", ") (map (iStr . show) vstack)),
    iStr "]", iNewline
    ]

showDump :: TDump -> ISeq
showDump dump = iNil

showClosure :: Closure -> ISeq
showClosure (i,f) = iConcat [
    iStr "(", showInstructions Terse i, iStr ", ", showFramePtr f, iStr ")"
    ]

showFramePtr :: FramePtr -> ISeq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats :: TState -> ISeq
showStats (TState instr fptr stack vstack dump heap code stats) = iConcat [
    iStr "Steps taken = ", iNum (statGetSteps stats), iNewline,
    iStr "No of frames allocated = ", iNum (hSize heap),
    iNewline
    ]

data HowMuchToPrint = Full | Terse | None
showInstructions :: HowMuchToPrint -> [Instruction] -> ISeq
showInstructions None il = iStr "{..}"
showInstructions Terse il = iConcat [
    iStr "{", iIndent (iInterleave (iStr ", ") body), iStr "}"
    ]
    where
        instrs = map (showInstruction None) il
        body | length il <= nTerse = instrs
             | otherwise = take nTerse instrs ++ [iStr ".."]
showInstructions Full il = iConcat [
    iStr "{ ", iIndent (iInterleave sep instrs), iStr " }"
    ]
    where
        sep = iStr "," `iAppend` iNewline
        instrs = map (showInstruction Full) il
nTerse :: Int
nTerse = 3

showInstruction :: HowMuchToPrint -> Instruction -> ISeq
showInstruction d (Take t n) = iConcat [iStr "Take ", iNum t, iStr " ", iNum n]
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x
showInstruction d (PushV mode) = iStr "PushV " `iAppend` iStr (show mode)
showInstruction d Return = iStr "Return"
showInstruction d (Op op) = iStr "Op " `iAppend` iStr (show op)
showInstruction d (Cond i1 i2) = iConcat [iStr "Cond ", showInstructions Terse i1, iStr " ", showInstructions Terse i2]
showInstruction d (Move i a) = iConcat [iStr "Move ", iNum i, iStr " ", showArg Terse a]

showArg :: HowMuchToPrint -> TAMode -> ISeq
showArg d (Arg m) = iStr "Arg " `iAppend` iNum m
showArg d (Code il) = iStr "Code " `iAppend` showInstructions d il
showArg d (Label s) = iStr "Label " `iAppend` iStr s
showArg d (IntConst n) = iStr "IntConst " `iAppend` iNum n

-- runner -------------------------------------------------------------------------------
run :: CoreProgram -> String
run = showResults . eval . compile

fullRun :: CoreProgram -> String
fullRun = showFullResults . eval . compile
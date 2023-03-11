module TIM5 (run, fullRun) where

import Syntax
import Heap
import Assoc
import ISeq
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
    | PushMarker Int
    | UpdateMarkers Int
    | Switch [(Int, [Instruction])]
    | ReturnConstr Int
    | Print
    deriving (Show)

data Op = Add | Sub | Mul | Div | Neg
        | Gt | Ge | Lt | Le | Eq | Ne deriving (Eq, Show)

data ValueAMode = FramePtr | IntVConst Int deriving (Show)

data TAMode
    = Arg Int
    | Label String
    | Code [Instruction]
    | IntConst Int
    | Data Int 
    deriving (Show)

data TState = TState {
    tInstr :: [Instruction],
    tFramePtr :: FramePtr,
    tDataFramePtr :: FramePtr,
    tStack :: TStack,
    tVStack :: TVStack,
    tDump :: TDump,
    tHeap :: THeap,
    tCodeStore :: CodeStore,
    tStats :: TStats,
    tOut :: [Int]
    }

data FramePtr
    = FrameAddr Addr
    | FrameInt Int
    | FrameNull

type TStack = [Closure]
type Closure = ([Instruction], FramePtr)
type TVStack = [Int]
type Frame = [Closure]
type TDump = [(FramePtr, Int, TStack)]

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

preludeDatatypes :: CoreProgram
preludeDatatypes = [
    ("cons", [], EConstr 2 2),
    ("nil", [], EConstr 1 0),
    ("true", [], EConstr 2 0),
    ("false", [], EConstr 1 0)
    ]

compile :: CoreProgram -> TState
compile program = TState
    [Enter (Label "main")]
    FrameNull
    FrameNull
    [(topCont, frame)]
    initialValueStack
    initialDump
    initialHeap
    compiledCode
    statInitial
    []
    where
        (initialHeap, frame) = fAlloc hInitial [([], FrameNull), ([], FrameNull)]
        scDefs = preludeDefs ++ preludeDatatypes ++ program
        compiledScDefs = map (compileSc initialEnv) scDefs
        compiledCode = compiledScDefs ++ compiledPrimitives
        initialEnv = [(name, Label name) | (name, args, body) <- scDefs] ++ [(name, Label name) | (name, code) <- compiledPrimitives]
initialValueStack :: TVStack
initialValueStack = []
initialDump :: TDump
initialDump = []
compiledPrimitives :: [(Name, [Instruction])]
compiledPrimitives = [] -- map mkPrim2 binOps
    --where mkPrim2 (name, op) = (name, [Take 2, Push (Code [Push (Code [Op op, Return]), Enter (Arg 1)]), Enter (Arg 2)])
topCont :: [Instruction]
topCont = [Switch [(1, []),
                   (2, [Move 1 (Data 1), Move 2 (Data 2),
                        Push (Code headCont),
                        Enter (Arg 1)])]]
headCont :: [Instruction]
headCont = [Print, Push (Code topCont), Enter (Arg 2)]
binOps :: Assoc Name Op
binOps = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("==", Eq), ("~=", Ne), ("<", Lt), ("<=", Le), (">", Gt), (">=", Ge)]
type TCompilerEnv = [(Name, TAMode)]

compileSc :: TCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSc env (name, args, body) = (name, instructions')
    where
        instructions' = mkUpd n ++ mkTake d n ++ is
        mkUpd n = [UpdateMarkers n | n /= 0]
        mkTake d n = [Take d n | d /= 0 || n /= 0]
        (d, is) = compileR body newEnv n
        n = length args
        newEnv = zip args (map mkUpdIndMode [1..]) ++ env

compileR :: CoreExpr -> TCompilerEnv -> Int -> (Int, [Instruction])
compileR e@(EAp (EAp (EAp (EVar "if") cond) i1) i2) env d = compileB cond env (dmax, [Cond ist ise])
    where
        (dt, ist) = compileR i1 env d
        (de, ise) = compileR i2 env d
        dmax = max dt de
compileR e@(EAp (EAp (EVar op) e1) e2) env d | op `elem` aDomain binOps = compileB e env (d, [Return])
compileR e@(ENum _) env d = compileB e env (d, [Return])
compileR e@(EAp (EVar "neg") e1) env d = compileB e env (d, [Return])
compileR (EAp e1 e2) env d | isAtomicExpr e2 = (d2, Push am : is)
    where
        am = compileA e2 env
        (d2, is) = compileR e1 env d
compileR (EAp e1 e2) env d = (d2, Move (d+1) amArg : Push (Code [Enter (Arg (d+1))]) : isFun)
    where
        (d1, amArg) = compileU e2 (d+1) env (d+1)
        (d2, isFun) = compileR e1 env d1
compileR e@(EVar v) env d = (d, mkEnter am)
    where am = compileA e env
compileR (ELet isRec defs e) env d = (d', moves ++ is)
    where
        n = length defs
        (dn, moves) = mapAccuml makeMove (d + n) (zip defs frameSlots)
        (d', is) = compileR e newEnv dn
        newEnv = zip (map fst defs) (map mkIndMode frameSlots) ++ env
        makeMove d ((name, rhs), frameSlot) = (d', Move frameSlot am) where (d', am) = compileU rhs frameSlot rhsEnv d
        rhsEnv | isRec = newEnv
               | otherwise = env
        frameSlots = [d+1..d+length defs]
compileR (EConstr t a) env d = if a == 0 then (d, [ReturnConstr t]) else (d, [UpdateMarkers a, Take a a, ReturnConstr t])
compileR (ECase e alts) env d = (d', Push (Code [Switch (map snd results)]) : ise)
    where
        results = map (\alt -> compileE alt env d) alts
        maxD = maximum $ map fst results
        (d', ise) = compileR e env maxD
compileR e env n = error $ "compileR: not implemented yet: " ++ show e

compileA :: CoreExpr -> TCompilerEnv -> TAMode
compileA (EVar v) env | v `elem` aDomain env = aLookup env v (error "compileA: cant happen")
compileA (ENum n) env = IntConst n
compileA e env = error $ "CompileA called with " ++ show e ++ " env is " ++ show env

compileB :: CoreExpr -> TCompilerEnv -> (Int, [Instruction]) -> (Int, [Instruction])
compileB (EAp (EAp (EVar op) e1) e2) env (d, cont) = compileB e2 env $ compileB e1 env (d, Op (aLookup binOps op (error "compileB: Op not found")) : cont)
compileB (EAp (EVar "neg") e1) env (d, cont) = compileB e1 env (d, Op Neg : cont)
compileB (ENum n) env (d, cont) = (d, PushV (IntVConst n) : cont)
compileB e env (d, cont) = (d', Push (Code cont) : is)
    where (d', is) = compileR e env d

compileE :: CoreAlt -> TCompilerEnv -> Int -> (Int, (Int, [Instruction]))
compileE (t, args, body) env d = (d', (t, isMoves ++ isBody))
    where
        isMoves = map (\x -> Move (d+x) (Data x)) [1..n]
        n = length args
        (d', isBody) = compileR body newEnv (d+n)
        newEnv = zip args (map Arg [d+1..d+n]) ++ env

compileU :: CoreExpr -> Int -> TCompilerEnv -> Int -> (Int, TAMode)
compileU (ENum n) u env d = (d, IntConst n)
compileU e u env d = (d', Code (PushMarker u : is))
    where (d', is) = compileR e env d

mkIndMode :: Int -> TAMode
mkIndMode n = Code [Enter (Arg n)]

mkUpdIndMode :: Int -> TAMode
mkUpdIndMode n = Code [PushMarker n, Enter (Arg n)]

mkEnter :: TAMode -> [Instruction]
mkEnter (Code i) = i
mkEnter other_am = [Enter other_am]

-- evaluator ----------------------------------------------------------------------------

eval :: TState -> [TState]
eval state = state : restStates
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
step (TState (Take t n : instr) fptr dfptr stack vstack dump heap cstore stats out) | length stack >= n && t >= n = TState instr fptr' dfptr (drop n stack) vstack dump heap' cstore stats out
                                                                          | otherwise = error "step: Too few args for Take instructions"
    where
        closures = take n stack 
        (heap', fptr') = fAlloc heap (closures ++ replicate (t-n) cDummy)
        cDummy = ([], FrameNull)
step (TState (Move i a : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack vstack dump heap' cstore stats out
    where
        heap' = fUpdate heap fptr i (amToClosure a fptr dfptr heap cstore)
step (TState [Enter am] fptr dfptr stack vstack dump heap cstore stats out) = TState instr' fptr' dfptr stack vstack dump heap cstore stats out
    where (instr', fptr') = amToClosure am fptr dfptr heap cstore
step (TState (Push am : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr (amToClosure am fptr dfptr heap cstore : stack) vstack dump heap cstore stats out
step (TState (PushV FramePtr : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack (getNum fptr:vstack) dump heap cstore stats out
    where
        getNum FrameNull = error "PushV called with fptr = FrameNull, expected FrameInt"
        getNum (FrameInt n) = n
        getNum (FrameAddr a) = error "PushV called with fptr = FrameAddr, expected FrameInt"
step (TState ((PushV (IntVConst n)) : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack (n:vstack) dump heap cstore stats out
step (TState [Return] fptr dfptr [] vstack ((fu, x, s):d) heap cstore stats out) = TState [Return] fptr dfptr s vstack d h' cstore stats out
    where
        n:ns = vstack
        h' = fUpdate heap fu x (intCode, FrameInt n)

step (TState [Return] fptr dfptr stack vstack dump heap cstore stats out) = TState i' f' dfptr s vstack dump heap cstore stats out
    where (i', f'):s = stack
step (TState (Op Neg : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack (negate n1 : vs) dump heap cstore stats out
    where (n1:vs) = vstack
step (TState (Op op : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack (f n1 n2 : vs) dump heap cstore stats out
    where
        (n1:n2:vs) = vstack
        f = aLookup builtInOps op (error $ "Op: no such builtin " ++ show op)
step (TState [Cond i1 i2] fptr dfptr stack vstack dump heap cstore stats out) = TState i' fptr dfptr stack vs dump heap cstore stats out
    where 
        (v:vs) = vstack
        i' = if v == 0 then i1 else i2
step (TState (PushMarker x : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr [] vstack ((fptr, x, stack) : dump) heap cstore stats out
step (TState i@(UpdateMarkers n : instr) fptr dfptr stack vstack dump heap cstore stats out) | m < n = TState i fptr dfptr (stack ++ s) vstack ds heap'' cstore stats out
    where
        m = length stack
        ((fu, x, s):ds) = dump
        (heap', f') = fAlloc heap stack
        heap'' = fUpdate heap' fu x (i', f')
        i' = map (Push . Arg) (reverse [1..m]) ++ UpdateMarkers n : instr
step (TState (UpdateMarkers n : instr) fptr dfptr stack vstack dump heap cstore stats out) = TState instr fptr dfptr stack vstack dump heap cstore stats out
step (TState [Switch list] fptr dfptr stack (t:vstack) dump heap cstore stats out) = TState i fptr dfptr stack vstack dump heap cstore stats out
    where
        i = aLookup list t (error $ "Switch: no instructions found for case " ++ show t)
step (TState [ReturnConstr t] fptr dfptr ((i,f'):stack) vstack dump heap cstore stats out) = TState i f' fptr stack (t:vstack) dump heap cstore stats out
step (TState [ReturnConstr t] fptr dfptr [] vstack ((fu,x,s):dump) heap cstore stats out) = TState [ReturnConstr t] fptr dfptr s vstack dump heap' cstore stats out
    where
        heap' = fUpdate heap fu x ([ReturnConstr t], fptr)
step (TState (Print : instr) fptr dfptr stack (t:vstack) dump heap cstore stats out) = TState instr fptr dfptr stack vstack dump heap cstore stats (out ++ [t]) 
step st = error $ "step: found\n" ++ iDisplay (showInstructions Full (tInstr st))

amToClosure :: TAMode -> FramePtr -> FramePtr -> THeap -> CodeStore -> Closure
amToClosure (Arg n) fptr dfptr heap cstore = fGet heap fptr n
amToClosure (Code il) fptr dfptr heap cstore = (il, fptr)
amToClosure (Label l) fptr dfptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr dfptr heap cstore = (intCode, FrameInt n)
amToClosure (Data n) fptr dfptr heap cstore = fGet heap dfptr n

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

showResults :: [TState] -> String
showResults states = iDisplay (showR 0 states)
    where
        showR n [state] = iConcat [iNewline, showStats state]
        showR n (state:states)
            | length out == n = iStr "." `iAppend` showR n states
            | otherwise = iConcat [iNewline, iNum (last out), showR (n+1) states ]
            where
                (TState _ _ _ _ _ _ _ _ _ out) = state

showScDefns :: TState -> ISeq
showScDefns (TState instr fptr dfptr stack vstack dump heap cstore stats out) = iInterleave iNewline (map showSc cstore)

showSc :: (Name, [Instruction]) -> ISeq
showSc (name, il) = iConcat [
    iStr "Code for ", iStr name, iStr ":", iNewline,
    iStr "  ", showInstructions Full il, iNewline, iNewline
    ]

showState :: TState -> ISeq
showState (TState instr fptr dfptr stack vstack dump heap cstore stats out) = iConcat [
    iStr "Code: ", showInstructions Full instr, iNewline,
    showFrame heap fptr,
    showStack stack,
    showValueStack vstack,
    showDump dump,
    showOut out,
    iNewline
    ]

showOut :: [Int] -> ISeq
showOut ns = iConcat [iStr "Output: ", iInterleave (iStr ", ") (map iNum ns)]

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
showDump dump = iConcat [iStr "Dump:    [",
                         iIndent (iInterleave (iStr ", ") (map showDumpItem dump)),
                         iStr "]", iNewline]
    where
        showDumpItem (fptr, slot, stack) = iConcat [
            iStr "(", showFramePtr fptr, iStr ",",
            iNum slot, iStr ",",
            iStr "<stk size ", iNum (length stack), iStr ">)"
            ]

showClosure :: Closure -> ISeq
showClosure (i,f) = iConcat [
    iStr "(", showInstructions Terse i, iStr ", ", showFramePtr f, iStr ")"
    ]

showFramePtr :: FramePtr -> ISeq
showFramePtr FrameNull = iStr "null"
showFramePtr (FrameAddr a) = iStr (show a)
showFramePtr (FrameInt n) = iStr "int " `iAppend` iNum n

showStats :: TState -> ISeq
showStats (TState instr fptr dfptr stack vstack dump heap code stats out) = iConcat [
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
showInstruction d (PushMarker x) = iStr "PushMarker " `iAppend` iNum x
showInstruction d (UpdateMarkers n) = iStr "UpdateMarkers " `iAppend` iNum n
showInstruction d (Switch list) = iConcat [
        iStr "Switch [", 
        iIndent (iInterleave iNewline (map showListItem list)),
        iStr "]"]
    where showListItem (n, instrs) = iConcat [iNum n, iStr " -> ", showInstructions Terse instrs]
showInstruction d (ReturnConstr t) = iStr "ReturnConstr " `iAppend` iNum t
showInstruction d Print = iStr "Print"

showArg :: HowMuchToPrint -> TAMode -> ISeq
showArg d (Arg m) = iStr "Arg " `iAppend` iNum m
showArg d (Code il) = iStr "Code " `iAppend` showInstructions d il
showArg d (Label s) = iStr "Label " `iAppend` iStr s
showArg d (IntConst n) = iStr "IntConst " `iAppend` iNum n
showArg d (Data n) = iStr "Data " `iAppend` iNum n

-- runner -------------------------------------------------------------------------------
run :: CoreProgram -> String
run = showResults . eval . compile

fullRun :: CoreProgram -> String
fullRun = showFullResults . eval . compile
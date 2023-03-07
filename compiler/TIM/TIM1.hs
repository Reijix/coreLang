module TIM1 (run, fullRun) where

import Syntax
import Heap
import Assoc
import ISeq

-- definitions --------------------------------------------------------------------------

data Instruction
    = Take Int
    | Enter TAMode
    | Push TAMode

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
data TVStack = DummyTVStack
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
initialArgStack = []
initialValueStack :: TVStack
initialValueStack = DummyTVStack
initialDump :: TDump
initialDump = DummyTDump
compiledPrimitives :: [(Name, [Instruction])]
compiledPrimitives = []

type TCompilerEnv = [(Name, TAMode)]

compileSc :: TCompilerEnv -> CoreScDefn -> (Name, [Instruction])
compileSc env (name, args, body) = (name, instructions')
    where
        instructions' = if argCount /= 0 then Take (length args) : instructions else instructions
        instructions = compileR body newEnv
        argCount = length args
        newEnv = zip args (map Arg [1..]) ++ env

compileR :: CoreExpr -> TCompilerEnv -> [Instruction]
compileR (EAp e1 e2) env = Push (compileA e2 env) : compileR e1 env
compileR e@(EVar v) env = [Enter (compileA e env)]
compileR e@(ENum n) env = [Enter (compileA e env)]
compileR e env = error $ "compileR: not implemented yet: " ++ show e

compileA :: CoreExpr -> TCompilerEnv -> TAMode
compileA (EVar v) env = aLookup env v (error ("Unknown variable " ++ v))
compileA (ENum n) env = IntConst n
compileA e env = Code (compileR e env)

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
step (TState (Take n : instr) fptr stack vstack dump heap cstore stats) | length stack >= n = TState instr fptr' (drop n stack) vstack dump heap' cstore stats
                                                                        | otherwise = error "step: Too few args for Take instructions"
    where (heap', fptr') = fAlloc heap (take n stack)
step (TState [Enter am] fptr stack vstack dump heap cstore stats) = TState instr' fptr' stack vstack dump heap cstore stats
    where (instr', fptr') = amToClosure am fptr heap cstore
step (TState (Push am : instr) fptr stack vstack dump heap cstore stats) = TState instr fptr (amToClosure am fptr heap cstore : stack) vstack dump heap cstore stats

amToClosure :: TAMode -> FramePtr -> THeap -> CodeStore -> Closure
amToClosure (Arg n) fptr heap cstore = fGet heap fptr n
amToClosure (Code il) fptr heap cstore = (il, fptr)
amToClosure (Label l) fptr heap cstore = (codeLookup cstore l, fptr)
amToClosure (IntConst n) fptr heap cstore = (intCode, FrameInt n)

intCode :: [Instruction]
intCode = []

-- printing -----------------------------------------------------------------------------
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
    iStr "Code: ", showInstructions Terse instr, iNewline,
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
showValueStack vstack = iNil

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
nTerse = 3

showInstruction d (Take m) = iStr "Take " `iAppend` iNum m
showInstruction d (Enter x) = iStr "Enter " `iAppend` showArg d x
showInstruction d (Push x) = iStr "Push " `iAppend` showArg d x

showArg d (Arg m) = iStr "Arg " `iAppend` iNum m
showArg d (Code il) = iStr "Code " `iAppend` showInstructions d il
showArg d (Label s) = iStr "Label " `iAppend` iStr s
showArg d (IntConst n) = iStr "IntConst" `iAppend` iNum n

-- runner -------------------------------------------------------------------------------
run :: CoreProgram -> String
run = showResults . eval . compile

fullRun :: CoreProgram -> String
fullRun = showFullResults . eval . compile
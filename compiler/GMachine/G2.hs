module G2 where

import Parser
import Heap
import Syntax
import Assoc ( Assoc, aDomain, aLookup )
import Control.Monad.State.Strict
    ( gets, modify, evalState, MonadState(get), State )
import UsefulFuns (mapAccuml)
import ISeq (iDisplay, iInterleave, iNewline, iLayn, iConcat, iStr, iIndent, ISeq, iAppend, iNum)

-- data structures
data Instruction
    = Unwind
    | Pushglobal Name
    | Pushint Int
    | Push Int
    | Mkap
    | Update Int
    | Pop Int deriving (Show)

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int [Instruction]
    | NInd Addr deriving (Show)

data GState = GState {
    gCode :: [Instruction],
    gStack :: [Addr],
    gHeap :: Heap Node,
    gGlobals :: Assoc Name Addr,
    gStats :: Stats
    }

-- state monad
type GMonad = State GState

-- functions for stats
type Stats = Int
statInitial :: Stats
statInitial = 0
statIncSteps :: Stats -> Stats
statIncSteps = (+) 1
statGetSteps :: Stats -> Int
statGetSteps = id

-- getter / setter for state
getCode :: GMonad [Instruction]
getCode = gets gCode
putCode :: [Instruction] -> GMonad ()
putCode code = modify (\state -> state {gCode = code})
getStack :: GMonad [Addr]
getStack = gets gStack
putStack :: [Addr] -> GMonad ()
putStack stack = modify (\state -> state {gStack = stack})
getHeap :: GMonad (Heap Node)
getHeap = gets gHeap
putHeap :: Heap Node -> GMonad ()
putHeap heap = modify (\state -> state {gHeap = heap})
getGlobals :: GMonad (Assoc Name Addr)
getGlobals = gets gGlobals
putGlobals :: Assoc Name Addr -> GMonad ()
putGlobals globals = modify (\state -> state {gGlobals = globals})
getStats :: GMonad Stats
getStats = gets gStats
putStats :: Stats -> GMonad ()
putStats stats = modify (\state -> state {gStats = stats})

doAdmin :: GMonad ()
doAdmin = getStats >>= putStats . statIncSteps

gFinal :: GMonad Bool
gFinal = null <$> getCode

step :: GMonad ()
step = do
    is <- getCode
    putCode $ tail is
    dispatch $ head is

dispatch :: Instruction -> GMonad ()
dispatch (Pushglobal f) = pushglobal f
dispatch (Pushint n) = pushint n
dispatch Mkap = mkap
dispatch (Push n) = push n
dispatch Unwind = unwind
dispatch (Update n) = update n
dispatch (Pop n) = pop n

-- instructions
pushglobal :: Name -> GMonad ()
pushglobal name = do
    stack <- getStack
    globals <- getGlobals
    let a = aLookup globals name (error $ "undeclared global " ++ name)
    putStack $ a : stack
pushint :: Int -> GMonad ()
pushint num = do
    stack <- getStack
    globals <- getGlobals
    heap <- getHeap
    let a = aLookup globals (show num) (-1)
    if a == -1
        -- node wasnt allocated yet, so now we do it
        then do
            let (heap', a) = hAlloc heap (NNum num)
            putStack (a : stack)
            putHeap heap'
            putGlobals ((show num, a) : globals)
        else do
            putStack (a : stack)
mkap :: GMonad ()
mkap = do
    stack <- getStack
    heap <- getHeap
    let (heap', a) = hAlloc heap (NAp (head stack) (head $ tail stack))
    putStack (a : drop 2 stack)
    putHeap heap'
push :: Int -> GMonad ()
push num = do
    stack <- getStack
    heap <- getHeap
    let a = getArg (hLookup heap (stack !! (num+1)))
    putStack $ a:stack
    where
        getArg :: Node -> Addr
        getArg (NAp a1 a2) = a2
        getArg _ = error "getArg called on non application!"
unwind :: GMonad ()
unwind = do
    heap <- getHeap
    stack <- getStack
    let node = hLookup heap (head stack)
    case node of
        NNum n -> return ()
        NAp a1 a2 -> putStack (a1 : stack) >> putCode [Unwind]
        NGlobal n c -> if length stack - 1 < n
                            then error "Unwinding with too few arguments!"
                            else putCode c
        NInd a1 -> putStack (a1 : tail stack) >> putCode [Unwind]
update :: Int -> GMonad ()
update n = do
    stack <- getStack
    heap <- getHeap
    -- TODO indirection just doesn't get created...
    let heap' = hUpdate heap (stack !! (n + 1)) (NInd (head stack))
    let node = hLookup heap' (stack !! (n + 1))
    putHeap heap'
    putStack $ tail stack

pop :: Int -> GMonad ()
pop n = do
    stack <- getStack
    putStack (drop n stack)

-- runners
run :: CoreProgram -> String
run prog = showResults $ evalState eval (GState initialCode [] heap globals statInitial)
    where
        (heap, globals) = buildInitialHeap prog
        initialCode = [Pushglobal "main", Unwind]

eval :: GMonad [GState]
eval = reverse <$> evalHelper []
    where
        evalHelper :: [GState] -> GMonad [GState]
        evalHelper states = do
            step
            doAdmin
            final <- gFinal
            state <- get
            stack <- getStack
            heap <- getHeap
            if final
                then return $ state : states
                else evalHelper (state : states)

buildInitialHeap :: CoreProgram -> (Heap Node, Assoc Name Addr)
buildInitialHeap prog = mapAccuml allocateSc hInitial compiled
    where
        compiled = map compileSc (preludeDefs ++ prog) ++ compiledPrimitives

allocateSc :: Heap Node -> (Name, Int, [Instruction]) -> (Heap Node, (Name, Addr))
allocateSc heap (name, nargs, instns) = (heap', (name, addr))
    where
        (heap', addr) = hAlloc heap (NGlobal nargs instns)

compileSc :: (Name, [Name], CoreExpr) -> (Name, Int, [Instruction])
compileSc (name, env, body) = (name, length env, compileR body (zip env [0..]))

compileR :: CoreExpr -> Assoc Name Int -> [Instruction]
compileR e env = compileC e env ++ [Update n, Pop n, Unwind]
    where n = length env

compileC :: CoreExpr -> Assoc Name Int -> [Instruction]
compileC (EVar v) env = if v `elem` aDomain env
    then [Push (aLookup env v (error "cant happen"))]
    else [Pushglobal v]
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
    where
        argOffset :: Int -> Assoc Name Int -> Assoc Name Int
        argOffset n env = [(v, n+m) | (v, m) <- env]
compileC _ _ = error "compileC invalid call"



compiledPrimitives :: [(Name, Int, [Instruction])]
compiledPrimitives = []


            
-- printing results
showResults :: [GState] -> String
showResults states = iDisplay (iConcat [
    iStr "Supercombinator definitions", iNewline,
    iInterleave iNewline (map (showSC s) (gGlobals s)),
    iNewline, iNewline, iStr "State transitions", iNewline, iNewline,
    iLayn (map showState states),
    iNewline, iNewline,
    showStats (last states)])
    where (s:ss) = states

showSC :: GState -> (String, Addr) -> ISeq
showSC s (name, addr) = iConcat [ iStr "Code for ", iStr name, iNewline,
                                  showInstructions code, iNewline, iNewline]
    where
        (NGlobal arity code) = hLookup (gHeap s) addr

showInstructions :: [Instruction] -> ISeq
showInstructions is = iConcat [iStr "  Code:{",
                               iIndent (iInterleave iNewline (map showInstruction is)),
                               iStr "}", iNewline]
showInstruction :: Instruction -> ISeq
showInstruction Unwind = iStr "Unwind"
showInstruction (Pushglobal f) = iStr "Pushglobal " `iAppend` iStr f
showInstruction (Push n) = iStr "Push " `iAppend` iNum n
showInstruction (Pushint n) = iStr "Pushint " `iAppend` iNum n
showInstruction Mkap = iStr "Mkap"
showInstruction (Update n) = iStr "Update " `iAppend` iNum n
showInstruction (Pop n) = iStr "Pop " `iAppend` iNum n

showState :: GState -> ISeq
showState s = iConcat [showStack s, iNewline, showInstructions (gCode s), iNewline]

showStack :: GState -> ISeq
showStack s = iConcat [iStr " Stack:[", iIndent (iInterleave iNewline (map (showStackItem s) (reverse (gStack s)))), iStr "]"]

showStackItem :: GState -> Addr -> ISeq
showStackItem s a = iConcat [iStr (showAddr a), iStr ": ", showNode s a node]
    where node = hLookup (gHeap s) a

showNode :: GState -> Addr -> Node -> ISeq
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
    where
        v = head [n | (n,b) <- gGlobals s, a == b]
showNode s a (NAp a1 a2) = iConcat [iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode s a (NInd a1) = iConcat [iStr "Ind ", iStr (showAddr a1)]

showStats :: GState -> ISeq
showStats s = iConcat [iStr "Steps taken = ", iNum (statGetSteps (gStats s))]
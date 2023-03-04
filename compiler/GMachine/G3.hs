module G3 where

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
    | Pop Int
    | Slide Int
    | Alloc Int deriving (Show)

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int [Instruction]
    | NInd Addr deriving (Show)

type GCode = [Instruction]
type GStack = [Addr]
type GHeap = Heap Node
type GGlobals = Assoc Name Addr
type GStats = Int

data GState = GState {
    gCode :: GCode,
    gStack :: GStack,
    gHeap :: GHeap,
    gGlobals :: GGlobals,
    gStats :: GStats
    }

-- state monad
type GMonad = State GState

-- functions for stats
statInitial :: GStats
statInitial = 0
statIncSteps :: GStats -> GStats
statIncSteps = (+) 1
statGetSteps :: GStats -> Int
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
getStats :: GMonad GStats
getStats = gets gStats
putStats :: GStats -> GMonad ()
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
dispatch (Slide n) = slide n
dispatch (Alloc n) = alloc n

rearrange :: Int -> GMonad ()
rearrange n = do
    heap <- getHeap
    stack <- getStack
    let as' = map (getArg . hLookup heap) (tail stack)
    putStack $ take n as' ++ drop n stack

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
    let a = stack !! num
    putStack $ a:stack

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
                            else putCode c >> rearrange n
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

slide :: Int -> GMonad ()
slide num = do
    stack <- getStack
    putStack (head stack : drop (num + 1) stack)

alloc :: Int -> GMonad ()
alloc n = do
    nodes <- allocNodes n
    stack <- getStack
    putStack $ nodes ++ stack

allocNodes :: Int -> GMonad [Addr]
allocNodes 0 = return []
allocNodes n = do
    as <- allocNodes (n - 1)
    heap <- getHeap
    let (heap'', a) = hAlloc heap (NInd hNull)
    putHeap heap''
    return $ a : as

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

-- compiler

type GEnvironment = Assoc Name Int
type GCompiler = CoreExpr -> GEnvironment -> GCode

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

compileR :: GCompiler
compileR e env = compileC e env ++ [Update n, Pop n, Unwind]
    where n = length env

compileC :: GCompiler
compileC (EVar v) env = if v `elem` aDomain env
    then [Push (aLookup env v (error "cant happen"))]
    else [Pushglobal v]
compileC (ENum n) env = [Pushint n]
compileC (EAp e1 e2) env = compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
compileC (ELet True defs e) env = compileLetrec compileC defs e env
compileC (ELet False defs e) env = compileLet compileC defs e env 
compileC _ _ = error "compileC invalid call"

argOffset :: Int -> Assoc Name Int -> Assoc Name Int
argOffset n env = [(v, n+m) | (v, m) <- env]

compileLet :: GCompiler -> [(Name, CoreExpr)] -> GCompiler
compileLet comp defs expr env = compileLet' defs env ++ comp expr env' ++ [Slide (length defs)]
    where env' = compileArgs defs env

compileLet' :: [(Name, CoreExpr)] -> GEnvironment -> GCode
compileLet' [] env = []
compileLet' ((name, expr) : defs) env = compileC expr env ++ compileLet' defs (argOffset 1 env)

compileLetrec :: GCompiler -> [(Name, CoreExpr)] -> GCompiler
compileLetrec comp defs expr env = [Alloc n] ++ compileLetrec' defs env' (n-1) ++ comp expr env' ++ [Slide n]
    where 
        env' = compileArgs defs env
        n = length defs

compileLetrec' :: [(Name, CoreExpr)] -> GEnvironment -> Int -> GCode
compileLetrec' [] env n = []
compileLetrec' ((name, expr) : defs) env n = compileC expr env ++ [Update n] ++ compileLetrec' defs env (n - 1)

compileArgs :: [(Name, CoreExpr)] -> GEnvironment -> GEnvironment
compileArgs defs env = zip (map fst defs) [n-1, n-2 .. 0] ++ argOffset n env
    where n = length defs

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
showInstruction (Slide n) = iStr "Slide " `iAppend` iNum n
showInstruction (Alloc n) = iStr "Alloc " `iAppend` iNum n

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
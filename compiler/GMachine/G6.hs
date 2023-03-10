module G6 where

import Parser
import Heap
import Syntax
import Assoc ( Assoc, aDomain, aLookup )
import Control.Monad.State.Strict
    ( gets, modify, evalState, MonadState(get), State )
import UsefulFuns (mapAccuml)
import ISeq (iDisplay, iInterleave, iNewline, iLayn, iConcat, iStr, iIndent, ISeq, iAppend, iNum)
import Data.Char (digitToInt)
import Data.List.Extra ( headDef )

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
    | Alloc Int 
    | Eval
    | Add | Sub | Mul | Div | Neg
    | Eq | Ne | Lt | Le | Gt | Ge
    | Cond GCode GCode
    | Pack Int Int
    | Casejump [(Int, GCode)]
    | Split Int
    | Print
    deriving (Show)

data Node
    = NNum Int
    | NAp Addr Addr
    | NGlobal Int [Instruction]
    | NInd Addr
    | NConstr Int [Addr] deriving (Show)

type GCode = [Instruction]
type GStack = [Addr]
type GHeap = Heap Node
type GGlobals = Assoc Name Addr
type GStats = Int
type GDump = [GDumpItem]
type GDumpItem = (GCode, GStack)
type GOutput = String


data GState = GState {
    gOutput :: GOutput,
    gCode :: GCode,
    gStack :: GStack,
    gDump :: GDump,
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
getOutput :: GMonad String
getOutput = gets gOutput
putOutput :: String -> GMonad ()
putOutput out = modify (\state -> state {gOutput = out})
getCode :: GMonad [Instruction]
getCode = gets gCode
putCode :: [Instruction] -> GMonad ()
putCode code = modify (\state -> state {gCode = code})
getStack :: GMonad [Addr]
getStack = gets gStack
putStack :: [Addr] -> GMonad ()
putStack stack = modify (\state -> state {gStack = stack})
getDump :: GMonad GDump
getDump = gets gDump
putDump :: GDump -> GMonad ()
putDump dump = modify (\state -> state {gDump = dump})
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
    dispatch $ headDef (error "step: no instruction, head failed") is

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
dispatch Eval = doEval
dispatch Add = arithmetic2 (+)
dispatch Sub = arithmetic2 (-)
dispatch Mul = arithmetic2 (*)
dispatch Div = arithmetic2 div
dispatch Neg = arithmetic1 negate
dispatch Eq = comparison (==)
dispatch Ne = comparison (/=)
dispatch Le = comparison (<=)
dispatch Lt = comparison (<)
dispatch Ge = comparison (>=)
dispatch Gt = comparison (>)
dispatch (Cond c1 c2) = cond c1 c2
dispatch (Pack t as) = pack t as
dispatch (Casejump ts) = casejump ts
dispatch (Split n) = split n
dispatch Print = doPrint

rearrange :: Int -> GMonad ()
rearrange n = do
    heap <- getHeap
    stack <- getStack
    let as' = map (getArg . hLookup heap) (tail stack)
    putStack $ take n as' ++ drop n stack

-- instructions
pushglobal :: Name -> GMonad ()
pushglobal name@['P', 'a', 'c', 'k', '{', t, ',', n, '}'] = do
    stack <- getStack
    globals <- getGlobals
    heap <- getHeap
    let a = aLookup globals name (-1)
    a' <- case a of
        (-1) -> do
            let (heap', a') = hAlloc heap (NGlobal (digitToInt n) [Pack (digitToInt t) (digitToInt n), Update 0, Unwind])
            putHeap heap'
            putGlobals $ (name, a') : globals
            return a'
        _ -> return a
    putStack $ a' : stack
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
    let (heap', a) = hAlloc heap (NAp (headDef (error "mkap no head found") stack ) (headDef (error "mkap no head . tail found") (tail stack)))
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
    (i', s') <- headDef (error "unwind: no head of dump found") <$> getDump
    d' <- tail <$> getDump
    let a = headDef (error "unwind: no head of stack found") stack
    let node = hLookup heap a
    case node of
        NNum n -> putCode i' >> putStack (a : s') >> putDump d'
        NAp a1 a2 -> putStack (a1 : stack) >> putCode [Unwind]
        NGlobal n c -> if length stack - 1 >= n
                            then putCode c >> rearrange n
                            else putCode i' >> putStack (last stack : s') >> putDump d'
        NInd a1 -> putStack (a1 : tail stack) >> putCode [Unwind]
        NConstr n as -> putStack (headDef (error "unwind: no head of stack found") stack : s') >> putCode i' >> putDump d'

update :: Int -> GMonad ()
update n = do
    stack <- getStack
    heap <- getHeap
    -- TODO indirection just doesn't get created...
    let heap' = hUpdate heap (stack !! (n + 1)) (NInd (headDef (error "update: no head of stack found") stack))
    let node = hLookup heap' (stack !! (n + 1))
    putHeap heap'
    putStack $ tail stack

pop :: Int -> GMonad ()
pop n = do
    stack <- getStack
    putStack (drop n stack)

doEval :: GMonad ()
doEval = do
    i <- getCode
    s <- getStack
    d <- getDump
    putDump $ (i, tail s) : d
    putStack [headDef (error "doEval: no head of stack found") s]
    putCode [Unwind]

slide :: Int -> GMonad ()
slide num = do
    stack <- getStack
    putStack (headDef (error "slide: no head of stack found") stack : drop (num + 1) stack)

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

cond :: GCode -> GCode -> GMonad ()
cond c1 c2 = do
    stack <- getStack
    code <- getCode
    b <- unboxBoolean $ headDef (error "cond: no head of stack found") stack
    putStack $ tail stack
    if b
        then putCode $ c1 ++ code
        else putCode $ c2 ++ code

pack :: Int -> Int -> GMonad ()
pack t n = do
    stack <- getStack
    heap <- getHeap
    let args = take n stack
    let (heap', a) = hAlloc heap (NConstr t args)
    putHeap heap'
    putStack $ a : drop n stack

casejump :: [(Int, GCode)] -> GMonad ()
casejump ts = do
    stack <- getStack
    heap <- getHeap
    code <- getCode
    let node = hLookup heap (headDef (error "casejump: no head of stack found") stack)
    case node of
        (NConstr t ss) -> do
            let i' = aLookup ts t $ error "casejump: tag of constructor not found in casejumps list!"
            putCode $ i' ++ code
        o -> error $ "casejump: node on top of stack isnt constructor!! Instead: " ++ show o 

split :: Int -> GMonad ()
split n = do
    stack <- getStack
    heap <- getHeap
    let node = hLookup heap (headDef (error "split: no head of stack found") stack)
    case node of
        (NConstr t ss) -> putStack $ ss ++ tail stack
        _ -> error "split: node on top of stack isnt constructor!!"

doPrint :: GMonad ()
doPrint = do
    stack <- getStack
    heap <- getHeap
    code <- getCode
    output <- getOutput
    let node = hLookup heap (headDef (error "doPrint: no head of stack found") stack)
    case node of
        (NNum n) -> do
            putOutput (output ++ show n)
            putStack $ tail stack
        (NConstr t as) -> do
            let n = length as
            putCode $ concatMap (const [Eval, Print]) [1..n] ++ code
            putStack $ as ++ tail stack

boxBoolean :: Bool -> GMonad ()
boxBoolean b = do
    stack <- getStack
    heap <- getHeap
    let b' = if b then 2 else 1
    let (h', a) = hAlloc heap (NConstr b' [])
    putHeap h'
    putStack $ a : stack

unboxBoolean :: Addr -> GMonad Bool
unboxBoolean a = do
    h <- getHeap
    return $ ub (hLookup h a)
    where
        ub (NConstr 2 []) = True
        ub (NConstr 1 []) = False
        ub _ = error "Unboxing a non-boolean!"

boxInteger :: Int -> GMonad()
boxInteger n = do
    s <- getStack
    h <- getHeap
    let (h', a) = hAlloc h (NNum n)
    putHeap h'
    putStack $ a : s

unboxInteger :: Addr -> GMonad Int
unboxInteger a = do
    h <- getHeap
    return $ ub (hLookup h a)
    where
        ub (NNum i) = i
        ub _ = error "Unboxing a non-integer!"

primitive1 :: (b -> GMonad ())
           -> (Addr -> GMonad a)
           -> (a -> b)
           -> GMonad ()
primitive1 box unbox op = do
    s <- getStack
    putStack $ tail s
    ub <- unbox $ headDef (error "primitive1: no head of stack found") s
    box $ op ub

primitive2 :: (b -> GMonad ())
           -> (Addr -> GMonad a)
           -> (a -> a -> b)
           -> GMonad ()
primitive2 box unbox op = do
    s <- getStack
    putStack $ drop 2 s
    ub0 <- unbox $ headDef (error "primitive2: no head of stack found") s
    ub1 <- unbox . headDef (error "primitive2: no head of tail stack found") $ tail s
    box $ op ub0 ub1

arithmetic1 :: (Int -> Int) -> GMonad ()
arithmetic1 = primitive1 boxInteger unboxInteger

arithmetic2 :: (Int -> Int -> Int) -> GMonad ()
arithmetic2 = primitive2 boxInteger unboxInteger

comparison :: (Int -> Int -> Bool) -> GMonad ()
comparison = primitive2 boxBoolean unboxInteger

builtInDyadic :: Assoc Name Instruction
builtInDyadic = [("+", Add), ("-", Sub), ("*", Mul), ("/", Div), ("==", Eq), ("~=", Ne), (">=", Ge), ("<=", Le), (">", Gt), ("<", Lt)]

-- runners
run :: CoreProgram -> String
run prog = showResults $ evalState eval (GState [] initialCode [] [] heap globals statInitial)
    where
        (heap, globals) = buildInitialHeap prog
        initialCode = [Pushglobal "main", Eval, Print]

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
compileR e env = compileE e env ++ [Update n, Pop n, Unwind]
    where n = length env

compileE :: GCompiler
compileE (ENum n) env = [Pushint n]
compileE (ELet False defs e) env = compileLet compileE defs e env
compileE (ELet True defs e) env = compileLetrec compileE defs e env
compileE (EAp (EAp (EVar f) e0) e1) env | f `elem` aDomain builtInDyadic = compileE e1 env ++ compileE e0 env' ++ [aLookup builtInDyadic f (error $ "couldn't find build int dyadic: " ++ f)]
    where env' = argOffset 1 env
compileE (EAp (EVar "negate") e) env = compileE e env ++ [Neg]
compileE (EAp (EAp (EAp (EVar "if") e0) e1) e2) env = compileE e0 env ++ [Cond (compileE e1 env) (compileE e2 env)]
compileE (ECase e alts) env = compileE e env ++ [Casejump (compileAlts compileE' alts env)]
compileE e env = compileC e env ++ [Eval]

compileE' :: Int -> GCompiler
compileE' offset expr env = [Split offset] ++ compileE expr env ++ [Slide offset]

compileC :: GCompiler
compileC (EVar v) env = if v `elem` aDomain env
    then [Push (aLookup env v (error "cant happen"))]
    else [Pushglobal v]
compileC (ENum n) env = [Pushint n]
compileC (EConstr t a) env = [Pushglobal $ "Pack{" ++ show t ++ "," ++ show a ++ "}"]
--compileC (EConstr t 0) env = [Pack t 0]
compileC (EAp e1 e2) env
    | saturatedCons spine = compileCS (reverse spine) env
    | otherwise =  compileC e2 env ++ compileC e1 (argOffset 1 env) ++ [Mkap]
    where
        spine = makeSpine (EAp e1 e2)
        saturatedCons (EConstr t a:es) = a == length es
        saturatedCons (e:es) = False
compileC (ELet True defs e) env = compileLetrec compileC defs e env
compileC (ELet False defs e) env = compileLet compileC defs e env
compileC e _ = error $ "compileC invalid call: " ++ show e

compileCS [EConstr t a] env = [Pack t a]
compileCS (e:es) env = compileC e env ++ compileCS es (argOffset 1 env)

makeSpine (EAp e1 e2) = makeSpine e1 ++ [e2]
makeSpine e = [e]

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
compiledPrimitives = [("+", 2, [Push 1, Eval, Push 1, Eval, Add, Update 2, Pop 2, Unwind]),
                      ("-", 2, [Push 1, Eval, Push 1, Eval, Sub, Update 2, Pop 2, Unwind]),
                      ("*", 2, [Push 1, Eval, Push 1, Eval, Mul, Update 2, Pop 2, Unwind]),
                      ("/", 2, [Push 1, Eval, Push 1, Eval, Div, Update 2, Pop 2, Unwind]),
                      ("negate", 1, [Push 0, Eval, Neg, Update 1, Pop 1, Unwind]),
                      ("==", 2, [Push 1, Eval, Push 1, Eval, Eq, Update 2, Pop 2, Unwind]),
                      ("~=", 2, [Push 1, Eval, Push 1, Eval, Ne, Update 2, Pop 2, Unwind]),
                      ("<", 2, [Push 1, Eval, Push 1, Eval, Lt, Update 2, Pop 2, Unwind]),
                      ("<=", 2, [Push 1, Eval, Push 1, Eval, Le, Update 2, Pop 2, Unwind]),
                      (">", 2, [Push 1, Eval, Push 1, Eval, Gt, Update 2, Pop 2, Unwind]),
                      (">=", 2, [Push 1, Eval, Push 1, Eval, Ge, Update 2, Pop 2, Unwind]),
                      ("if", 3, [Push 0, Eval, Cond [Push 1] [Push 2], Update 3, Pop 3, Unwind])]

compileAlts :: (Int -> GCompiler)
            -> [CoreAlt]
            -> GEnvironment
            -> [(Int, GCode)]
compileAlts comp alts env = [(tag, comp (length names) body (zip names [0..] ++ argOffset (length names) env)) | (tag, names, body) <- alts]

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
showInstruction Eval = iStr "Eval"
showInstruction Add = iStr "Add"
showInstruction Sub = iStr "Sub"
showInstruction Mul = iStr "Mul"
showInstruction Div = iStr "Div"
showInstruction Neg = iStr "Neg"
showInstruction Eq = iStr "Eq"
showInstruction Ne = iStr "Ne" 
showInstruction Lt = iStr "Lt"
showInstruction Le = iStr "Le"
showInstruction Gt = iStr "Gt"
showInstruction Ge = iStr "Ge"
showInstruction (Cond c1 c2) = iConcat [iStr "Cond [1: ", shortShowInstructions 2 c1, iStr ", 0: ", shortShowInstructions 2 c2, iStr "]"]
showInstruction (Pack n1 n2) = iConcat [iStr "Pack ", iNum n1, iStr " ", iNum n2]
showInstruction (Casejump ts) = iConcat [iStr "Casejump [", iIndent (iInterleave iNewline (map showCase ts)), iStr "]"]
    where showCase (tag, code) = iConcat [iNum tag, iStr ": ", shortShowInstructions 3 code]
showInstruction (Split n) = iStr "Split " `iAppend` iStr (show n)
showInstruction Print = iStr "Print"

showState :: GState -> ISeq
showState s = iConcat [showOutput s, iNewline,
                       showStack s, iNewline,
                       showDump s, iNewline,
                       showInstructions (gCode s), iNewline]
 
showOutput :: GState -> ISeq
showOutput s = iConcat [iStr "Output:\"", iStr (gOutput s), iStr "\""]

showDump :: GState -> ISeq
showDump s = iConcat [iStr " Dump:[", iIndent (iInterleave iNewline (map showDumpItem (reverse (gDump s)))), iStr "]"]

showDumpItem :: GDumpItem -> ISeq
showDumpItem (code, stack) = iConcat [iStr "<", shortShowInstructions 3 code, iStr ", ", shortShowStack stack, iStr ">"]

shortShowInstructions :: Int -> GCode -> ISeq
shortShowInstructions number code = iConcat [iStr "{", iInterleave (iStr "; ") dotcodes, iStr "}"]
    where
        codes = map showInstruction (take number code)
        dotcodes = if length code > number
                    then codes ++ [iStr "..."]
                    else codes

shortShowStack :: GStack -> ISeq
shortShowStack stack = iConcat [iStr "[", iInterleave (iStr ", ") (map (iStr . showAddr) stack), iStr "]"]

showStack :: GState -> ISeq
showStack s = iConcat [iStr " Stack:[", iIndent (iInterleave iNewline (map (showStackItem s) (reverse (gStack s)))), iStr "]"]

showStackItem :: GState -> Addr -> ISeq
showStackItem s a = iConcat [iStr (showAddr a), iStr ": ", showNode s a node]
    where node = hLookup (gHeap s) a

showNode :: GState -> Addr -> Node -> ISeq
showNode s a (NNum n) = iNum n
showNode s a (NGlobal n g) = iConcat [iStr "Global ", iStr v]
    where
        v = headDef (error "showNode: no head of globals with fitting name found") [n | (n,b) <- gGlobals s, a == b]
showNode s a (NAp a1 a2) = iConcat [iStr "Ap ", iStr (showAddr a1), iStr " ", iStr (showAddr a2)]
showNode s a (NInd a1) = iConcat [iStr "Ind ", iStr (showAddr a1)]
showNode s a (NConstr t as) = iConcat [iStr "Cons ", iNum t, iStr " [", iInterleave (iStr ", ") (map (iStr . showAddr) as), iStr "]"]

showStats :: GState -> ISeq
showStats s = iConcat [iStr "Steps taken = ", iNum (statGetSteps (gStats s))]
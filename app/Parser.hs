module Parser where

import Data.Char (isDigit, isLetter, isSpace)
import PrettyPrint
import Syntax
import System.IO
import Prelude hiding (lex, (<*>))

-- linenumber, text
type Token = (Int, String)

------------- LEXER -------------

twoCharOps = ["==", "~=", "<=", ">=", "->"]

lex :: Int -> String -> [Token]
lex line (c : cs) | c == '\n' = lex (line + 1) cs
lex line (c : cs) | isSpace c = lex line cs
lex line (c : cs) | c == '#' = lex (line + 1) (tail (dropWhile (/= '\n') cs))
lex line (c : d : rest) | [c, d] `elem` twoCharOps = (line, [c, d]) : lex line rest
lex line (c : cs) | isDigit c = (line, num_tok) : lex line rest_cs
  where
    num_tok = c : takeWhile isDigit cs
    rest_cs = dropWhile isDigit cs
lex line (c : cs) | isLetter c = (line, var_tok) : lex line rest_cs
  where
    var_tok = c : takeWhile isIdChar cs
    rest_cs = dropWhile isIdChar cs
    isIdChar c = isLetter c || isDigit c || c == '_'
lex line (c : cs) = (line, [c]) : lex line cs
lex _ [] = []

------------- PARSER ------------
-- =====Predefinitions======== --
type Parser a = [Token] -> [(a, [Token])]

keywords = ["let", "letrec", "case", "in", "of", "Pack"]

pSat :: (String -> Bool) -> Parser String
pSat test ((line, lit) : toks) | test lit = [(lit, toks)]
pSat _ _ = []

pLit :: String -> Parser String
pLit s = pSat (== s)

pVar :: Parser String
pVar = pSat (\str -> isLetter (head str) && str `notElem` keywords)

pNum :: Parser Int
pNum = pApply (pSat isNum) read
  where
    isNum :: String -> Bool
    isNum = foldl (\b a -> b && isDigit a) True

{-
 - Implementation for arbitrary number types
pNum = pApply (pSat (isNum False)) read
  where
    isNum :: Bool -> String -> Bool
    isNum foundDot (c : cs) | isDigit c = isNum foundDot cs
    isNum False ('.' : cs) = isNum True cs
    isNum _ _ = False
-}

pAlt :: Parser a -> Parser a -> Parser a
pAlt p1 p2 toks = p1 toks ++ p2 toks

(<|>) = pAlt

pThen :: (a -> b -> c) -> Parser a -> Parser b -> Parser c
pThen combine p1 p2 toks =
  [ (combine v1 v2, toks2) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1
  ]

pThen3 :: (a -> b -> c -> d) -> Parser a -> Parser b -> Parser c -> Parser d
pThen3 combine p1 p2 p3 toks =
  [ (combine v1 v2 v3, toks3) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2
  ]

pThen4 :: (a -> b -> c -> d -> e) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e
pThen4 combine p1 p2 p3 p4 toks =
  [ (combine v1 v2 v3 v4, toks4) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2, (v4, toks4) <- p4 toks3
  ]

pThen5 :: (a -> b -> c -> d -> e -> f) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f
pThen5 combine p1 p2 p3 p4 p5 toks =
  [ (combine v1 v2 v3 v4 v5, toks5) | (v1, toks1) <- p1 toks, (v2, toks2) <- p2 toks1, (v3, toks3) <- p3 toks2, (v4, toks4) <- p4 toks3, (v5, toks5) <- p5 toks4
  ]

pEmpty :: a -> Parser a
pEmpty elem toks = [(elem, toks)]

pOneOrMore :: Parser a -> Parser [a]
pOneOrMore p = pThen (:) p (pZeroOrMore p)

pZeroOrMore :: Parser a -> Parser [a]
pZeroOrMore p = pOneOrMore p <|> pEmpty []

pApply :: Parser a -> (a -> b) -> Parser b
pApply p f toks =
  [ (f v1, toks1) | (v1, toks1) <- p toks
  ]

pOneOrMoreWithSep :: Parser a -> Parser b -> Parser [a]
pOneOrMoreWithSep p1 p2 = pThen (:) p1 (pZeroOrMore (pThen (\a b -> b) p2 p1))

-- ========Other defns=========--
pProgram :: Parser CoreProgram
pProgram = pOneOrMoreWithSep pSc (pLit ";")

pSc :: Parser CoreScDefn
pSc = pThen4 mk_sc pVar (pZeroOrMore pVar) (pLit "=") pExpr
  where
    mk_sc :: String -> [String] -> String -> CoreExpr -> CoreScDefn
    mk_sc name args _ expr = (name, args, expr)

pAExpr :: Parser CoreExpr
pAExpr = pEVar <|> pENum <|> pEConstr <|> pParensExpr
  where
    pEConstr = pThen5 (\_ n1 _ n2 _ -> EConstr n1 n2) (pLit "Pack{") pNum (pLit ",") pNum (pLit "}")
    pENum = pApply pNum ENum
    pEVar = pApply pVar EVar
    pParensExpr = pThen3 (\l expr r -> expr) (pLit "(") pExpr (pLit ")")

pExpr :: Parser CoreExpr
pExpr = pAp <|> pLocDef <|> pLocRecDef <|> pCase <|> pLam <|> pBinAp <|> pAExpr
  where
    pAp = pApply (pOneOrMore pAExpr) (foldr1 EAp)
    pBinAp = pThen3 (\l op r -> EAp (EAp (EVar op) l) r) pExpr pBinOp pExpr
    pLocDef =
      pThen4
        (\_ defns _ expr -> ELet nonRecursive defns expr)
        (pLit "let")
        pDefns
        (pLit "in")
        pExpr
    pLocRecDef =
      pThen4
        (\_ defns _ expr -> ELet recursive defns expr)
        (pLit "letrec")
        pDefns
        (pLit "in")
        pExpr
    pCase =
      pThen4
        (\_ expr _ alts -> ECase expr alts)
        (pLit "case")
        pExpr
        (pLit "of")
        pAlts
    pLam = pThen4 (\_ vars _ expr -> ELam vars expr) (pLit "\\ ") (pOneOrMore pVar) (pLit ".") pExpr
    pBinOp = pArithOp <|> pRelOp <|> pBoolOp
    pArithOp = pLit "+" <|> pLit "-" <|> pLit "*" <|> pLit "/"
    pRelOp = pLit "<" <|> pLit "<=" <|> pLit "==" <|> pLit "~=" <|> pLit ">=" <|> pLit ">"
    pBoolOp = pLit "&" <|> pLit "|"
    pDefns = pOneOrMoreWithSep (pThen3 (\var _ expr -> (var, expr)) pVar (pLit "=") pExpr) (pLit ";")
    pAlts =
      pOneOrMoreWithSep
        ( pThen4
            (\num vars _ expr -> (num, vars, expr))
            (pThen3 (\_ num _ -> num) (pLit "<") pNum (pLit ">"))
            (pZeroOrMore pVar)
            (pLit "->")
            pExpr
        )
        (pLit ";")

------------- RESULT ------------
syntax :: [Token] -> CoreProgram
syntax = take_first_parse . pProgram
  where
    take_first_parse ((prog, []) : other_parses) = prog
    take_first_parse other = error ("Syntax error" ++ show other)

parse :: String -> CoreProgram
parse = syntax . lex 0

------------- TESTING -----------
main :: IO ()
main = do
  handle <- openFile "test-programs/simpl.cl" ReadMode
  contents <- hGetContents handle
  let parsed = parse contents
  let lexed = lex 0 contents
  print parsed
  putStrLn (iDisplay (pprProgram parsed :: ISeqRep))
  return ()
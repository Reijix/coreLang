module ISeq (ISeq, iNil, iStr, iAppend, iNewline, iIndent, iDisplay, iConcat, iInterleave, iNum, iFWNum, iLayn) where

import Data.List ( intersperse )

-- Interfaces
iNil :: ISeq
iStr :: String -> ISeq
iAppend :: ISeq -> ISeq -> ISeq
iNewline :: ISeq
iIndent :: ISeq -> ISeq
iDisplay :: ISeq -> String

-- Helper functions
iConcat :: [ISeq] -> ISeq
iConcat = foldl iAppend iNil

iInterleave :: ISeq -> [ISeq] -> ISeq
iInterleave x xs = iConcat (intersperse x xs)

iNum :: Int -> ISeq
iNum n = iStr (show n)

iFWNum :: Int -> Int -> ISeq
iFWNum width n = iStr (replicate (width - length digits) ' ' ++ digits)
  where
    digits = show n

iLayn :: [ISeq] -> ISeq
iLayn seqs = iConcat (zipWith (curry lay_item) [1..] seqs)
  where
    lay_item (n, seq) = iConcat [ iFWNum 4 n, iStr ") ", iIndent seq, iNewline ]


type ISeq = ISeqImpl -- this line 'chooses' a implementation

-- Implementations
data ISeqImpl
  = INil
  | IStr [Char]
  | IAppend ISeqImpl ISeqImpl
  | IIndent ISeqImpl
  | INewline
  deriving (Show)

iNil = INil
iStr str = iConcat2 (map (\c -> if c == "\n" then INewline else IStr c) (split2 str))
    where
    split2 str = case break (== '\n') str of
        (a, '\n' : b) -> a : "\n" : split2 b
        (a, "") -> [a]
    iConcat2 = foldl IAppend INil
iAppend seq1 INil = seq1
iAppend INil seq2 = seq2
iAppend seq1 seq2 = IAppend seq1 seq2
iNewline = INewline
iIndent = IIndent
iDisplay seq = flatten 0 [(seq, 0)]
    where
        flatten :: Int -> [(ISeqImpl, Int)] -> String
        flatten col [] = ""
        flatten col (x : xs) = case x of
            (INil, ind) -> flatten col xs
            (IStr s, ind) -> s ++ flatten (col + length s) xs
            (IAppend seq1 seq2, ind) -> flatten col ((seq1, ind) : (seq2, ind) : xs)
            (INewline, ind) -> "\n" ++ spaces ind ++ flatten ind xs
            (IIndent seq, ind) -> flatten col ((seq, col) : xs)
        spaces :: Int -> String
        spaces num = replicate num ' '

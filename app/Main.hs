module Main where

import Data.List (genericTake)
import Data.Semigroup ((<>))
import Options.Applicative
import Parser (parse)
import PrettyPrint (iDisplay, prettyProgram)
import System.IO

data CmdOption = CmdOption
  { sourceFile :: String,
    destinationFile :: String,
    prettyPrint :: Bool
  }

cmdOption :: Parser CmdOption
cmdOption =
  CmdOption
    <$> argument
      str
      ( metavar "<source file>"
      )
    <*> strOption
      ( short 'o'
          <> metavar "<destination file>"
          <> help "Place the output into <destination file>."
          <> showDefault
          <> value "a.out"
      )
    <*> switch
      ( short 'p'
          <> help "PrettyPrint the parsed program"
          <> showDefault
      )

main :: IO ()
main = run =<< execParser opts
  where
    opts =
      info
        (cmdOption <**> helper)
        ( fullDesc
            <> progDesc "Compiles a given coreLang source-code."
            <> header "This is a compiler for the coreLang as presented in \"Implementing Functional Langauges A Tutorial\""
        )

run :: CmdOption -> IO ()
-- no ppr
run (CmdOption sourceFile _ False) = do
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  let parsed = parse sourceText
  case parsed of
    Left prog -> print prog
    Right err -> putStrLn err

-- do ppr
run (CmdOption sourceFile _ True) = do
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  let parsed = parse sourceText
  case parsed of
    Left prog -> putStrLn . iDisplay . prettyProgram $ prog
    Right err -> putStrLn err

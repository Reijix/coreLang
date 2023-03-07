module Main where

import Data.List (genericTake)
import Data.Semigroup ((<>))
import Control.Monad (when)
import Options.Applicative
    ( (<**>),
      argument,
      fullDesc,
      header,
      help,
      info,
      metavar,
      progDesc,
      short,
      showDefault,
      str,
      strOption,
      switch,
      value,
      execParser,
      helper,
      Parser )
import Parser (parse)
import PrettyPrint (pprProgram)
import ISeq (iDisplay)
import System.IO ( hGetContents, openFile, IOMode(ReadMode) )
import Mark1 ( run )
import Mark2 ( run )
import Mark3 ( run )
import Mark4 ( run )
import G1 ( run )
import G2 ( run )
import G3 ( run )
import G4 ( run )
import G5 ( run )
import G6 ( run )
import G7 ( run )
import TIM1 ( run, fullRun )

data CmdOption = CmdOption
  { sourceFile :: String,
    destinationFile :: String,
    prettyPrint :: Bool,
    backend :: String
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
    <*> strOption
      ( short 'b'
        <> metavar "<backend>"
        <> help "Compile using <backend>\nSupported backend: [Mark1, Mark2, Mark3, Mark4, G1, G2, G3, G4, G5, G6, G7, TIM1]"
        <> showDefault
        <> value "TIM1"
      )

main :: IO ()
main = Main.run =<< execParser opts
  where
    opts =
      info
        (cmdOption <**> helper)
        ( fullDesc
            <> progDesc "Compiles a given coreLang source-code."
            <> header "This is a compiler for the coreLang as presented in \"Implementing Functional Langauges A Tutorial\""
        )

run :: CmdOption -> IO ()
-- only lex and parse
run (CmdOption sourceFile _ ppr backend) = do
  -- read sourceFile
  source <- openFile sourceFile ReadMode
  sourceText <- hGetContents source

  -- do lex and parse
  let parsed = parse sourceText
  let prog = case parsed of
            Left prog -> prog
            Right err -> error err

  -- optionally prettyprint
  --when ppr $ putStrLn . iDisplay . pprProgram $ prog
  when ppr $ print prog

  -- compile, eval, showResults
  case backend of 
    "Mark1" -> putStrLn (Mark1.run prog)
    "Mark2" -> putStrLn (Mark2.run prog)
    "Mark3" -> putStrLn (Mark3.run prog)
    "Mark4" -> putStrLn (Mark4.run prog)
    "Mark5" -> error "Mark5 not implemented yet"
    "G1" -> putStrLn (G1.run prog)
    "G2" -> putStrLn (G2.run prog)
    "G3" -> putStrLn (G3.run prog)
    "G4" -> putStrLn (G4.run prog)
    "G5" -> putStrLn (G5.run prog)
    "G6" -> putStrLn (G6.run prog)
    "G7" -> putStrLn (G7.run prog)
    "TIM1" -> putStrLn (TIM1.fullRun prog)
    _ -> error "Backend doesn't exist!"

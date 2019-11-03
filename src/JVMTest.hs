module Main where

import Data.List ( intercalate )
import System.Environment ( getArgs )
import System.FilePath.Posix ( replaceExtension, takeDirectory, takeBaseName )
import System.Process
import Control.Monad.State ( runState )

import JVM
import ErrM
import LexInstant
import ParInstant
import AbsInstant

jasminCmd = "java -jar lib/jasmin.jar -d"


type ParseFun a = [Token] -> Err a
run :: ParseFun Program -> String -> FilePath -> IO ()
run p s file = do
           let (Ok tree) = p $ myLexer s
           let (jvm, _)  = runState (compile tree $ takeBaseName file) initialCS
           let jFile     = replaceExtension file "j"
           let dir       = takeDirectory file
           writeFile jFile jvm
           callCommand $ intercalate " " [ jasminCmd
                                         , dir
                                         , jFile ]

main :: IO ()
main = do
  args  <- getArgs
  input <- readFile  $ head args
  run pProgram input $ head args

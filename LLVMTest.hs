module Main where

import Data.List ( intercalate )
import System.Environment ( getArgs )
import System.FilePath.Posix ( replaceExtension )
import System.Process
import Control.Monad.State ( runState )

import LLVM
import ErrM
import LexInstant
import ParInstant
import AbsInstant

llvmCmd     = "llvm-as -o"
llvmLink    = "llvm-link -o"
runtimeFile = "lib/Runtime.class"

llvmLinkCmd :: String -> String -> String -> String
llvmLinkCmd out f s = intercalate " " [ llvmLink
                                      , out
                                      , f
                                      , s]

type ParseFun a = [Token] -> Err a
run :: ParseFun Program -> String -> FilePath -> IO ()
run p s file = do
           let (Ok tree) = p $ myLexer s
           let (llvm, _) = runState (compile tree) initialCS
           let llFile    = replaceExtension file "ll"
           let tmpFile   = replaceExtension file "tmp"
           let bFile     = replaceExtension file "bc"
           writeFile llFile llvm
           callCommand $ intercalate " " [ llvmCmd
                                         , tmpFile
                                         , llFile ]
           callCommand $ llvmLinkCmd bFile tmpFile runtimeFile
           callCommand $ "rm " ++ tmpFile

main :: IO ()
main = do
  args  <- getArgs
  input <- readFile  $ head args
  run pProgram input $ head args

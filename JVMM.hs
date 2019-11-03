module Main where
import JVM
import ErrM
import LexInstant
import ParInstant
import Data.List
import System.Environment ( getArgs )
import System.FilePath.Posix ( replaceExtension, takeDirectory, takeBaseName  )
import System.Process

 run p s file = do
           let (Ok tree) = p $ myLexer s
           let (jvm, _)  = runState (JVM.compile tree $ takeBaseName file) initialCS
           let jFile     = replaceExtension file "j"
           let dir = takeDirectory file
           writeFile jFile jvm
           callCommand $ intercalate " " [ "java -jar lib/jasmin.jar -d"
                                         , dir
                                         , jFile ]

main :: IO ()
main = do
  args  <- getArgs
  input <- readFile $ head args
  run pProgram input $ head args

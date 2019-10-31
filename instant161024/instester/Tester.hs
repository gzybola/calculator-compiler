{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
module Main where
import Shelly
import Prelude hiding(FilePath,unwords)
import System.Environment
import qualified Data.Text as LT
import Data.Text(Text)
import Data.String(IsString,fromString)
import Control.Monad(forM)
import qualified Filesystem.Path.CurrentOS as FP    

default (LT.Text)
  
type MyResult a = Either Text a

infix 3 `orDie`
orDie :: Sh Bool -> Text -> Sh ()
c `orDie` msg = unlessM c (errorExit msg)

die :: Text -> Sh a
die t = errorExit t >> return undefined

requireDir :: FilePath -> Sh ()
requireDir d = test_d d `orDie` 
               LT.unwords ["Required dir ",toTextIgnore d, " does not exist"]
main = do
  args <- getArgs
  case args of
    [projectDir, testDir] -> tester (fromString projectDir) (fromString testDir)
    _ -> putStrLn "Usage: tester projectDir testDir"
    

tester :: FilePath -> FilePath -> IO () 
tester projectDir testDir = shelly $ verbosely $ do
  test_d projectDir `orDie` "No project directory"
  -- requireDir (testDir </> "good") 
  let newTestDir = projectDir </> "testerTests"
  cp_r testDir newTestDir
  cd projectDir
  (opts, archive) <- findArchive 
  inspect archive
  tar opts archive
  test_f "Makefile" `orDie` "No Makefile"
  run_ "make" []
  test_f "insc_jvm" `orDie` "insc_jvm executable not found"
  test_f "insc_llvm" `orDie` "insc_llvm executable not found"         
  let relTestDir = "testerTests"
  testJvm projectDir relTestDir
  testLlvm relTestDir
  return ()

testJvm :: FilePath -> FilePath -> Sh ()
testJvm projectDir newTestDir = do
  requireDir newTestDir
  let goodDir = newTestDir
  let libDir = projectDir </> "lib"
  t1 <- toTextWarn goodDir
  t2 <- toTextWarn libDir
  -- let classpath = LT.concat [t1,":", t2]
  let classpath = LT.concat [t1,":", "lib"]
  goodFiles <- (ls goodDir >>= return . havingExt "ins")
  results <- forM goodFiles (testJvmOne classpath)
  if (and results) then echo "JVM tests passed" 
                   else echo "JVM tests failed"  

testLlvm :: FilePath -> Sh ()
testLlvm newTestDir = do
  requireDir newTestDir
  let goodDir = newTestDir
  goodFiles <- (ls goodDir >>= return . havingExt "ins")
  results <- forM goodFiles testLlvmOne
  if (and results) then echo "LLVM tests passed" 
                   else echo "LLVM tests failed"  
                        
testJvmOne :: Text -> FilePath -> Sh Bool
testJvmOne classpath fp = do
  insc <- absPath "insc_jvm"
  ft <- toTextWarn fp
  cmd insc ft
  let filename = FP.basename fp
  let dir = FP.dirname fp
  -- filenameText <- toTextWarn filename
  let expectedOutput = dir </> filename <.> "output"
  cmd "java" "-cp" classpath filename -|- cmd "diff" "-"  expectedOutput
  return True

testLlvmOne :: FilePath -> Sh Bool
testLlvmOne fp = do
  insc <- absPath "insc_llvm"
  ft <- toTextWarn fp
  cmd insc ft
  let filename = FP.basename fp
  let dir = FP.dirname fp
  let bcname = dir </> filename <.> "bc"
  let expectedOutput = dir </> filename <.> "output"
  cmd "lli" bcname -|- cmd "diff" "-"  expectedOutput
  return True
         
testBadOne :: FilePath -> Sh Bool
testBadOne fp = do
  latc <- absPath "latc"
  ft <- toTextWarn fp
  -- echo "Expect ERROR"
  cmd latc ft
  trace "stderr:"
  lastStderr >>= trace
  lastStderrHeadIs "ERROR"

lastStderrHead :: Sh (MyResult Text)
lastStderrHead = do
  text <- lastStderr
  return $ case LT.lines text of
    [] -> Left "empty stderr"
    (l:_) -> Right l
    
lastStderrHeadIs :: Text -> Sh Bool
lastStderrHeadIs expected = do
  text <- lastStderr
  case LT.lines text of
    [] -> echo "empty stderr" >> return False
    (got:_) | got == expected -> return True
            | otherwise -> do
                echo $ LT.unwords ["Expected",expected,"got",got]
                return False
                
findArchive :: Sh (Text,FilePath)
findArchive = do
  allFiles <- ls "."
  echo "All project files:"
  inspect allFiles
  let archives = [(opts, s) | 
                    (opts, ext) <- [("xf", ".tar.gz"), ("xf", ".tgz"), ("xf", ".tar.bz2"), ("xf", ".tar.bzip2"), ("xf", ".tbz"), ("xf", ".tar")],
                    s <- Prelude.filter (isSuffixOfTFP ext) allFiles]
  echo "Archives:"
  inspect archives
  case archives of
    [a] -> return a
    [] -> die $ "No archive found"
    _ -> die "Multiple archives found"
    
        
isSuffixOfTFP :: Text -> FilePath -> Bool
isSuffixOfTFP t fp = LT.isSuffixOf t (toTextIgnore fp)

tar :: Text -> FilePath -> Sh ()
tar opts archive = do
  a <- toTextWarn archive
  cmd "tar" opts a
  
havingExt :: Text -> [FilePath] -> [FilePath]
havingExt ext = Prelude.filter (hasExt ext)

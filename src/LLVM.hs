module LLVM where

import Control.Monad.State
import Data.Map as Map 
import qualified Data.Text as T
import Data.List 
import System.Environment ( getArgs )
import System.FilePath.Posix ( replaceExtension )

import AbsInstant
import ErrM
import LexInstant
import ParInstant

type CompilerState = (Integer, Map String Integer)
type LLVMProgram = String

instance Show Calculation where
        show Add = "add"
        show Sub = "sub"
        show Mul = "mul"
        show Div = "sdiv"

initialCS = (0, Map.empty)
identation = "    "

next :: Integer -> Integer
next a = a + 1

line :: [String] -> String
line strgs = concat [ identation
                    , (intercalate " " strgs)
                    ,"\n"]

tmpRegister :: Integer -> String
tmpRegister int = "%_" ++ (show int)

varRegister :: String -> Integer -> String
varRegister var n = "%" ++ var ++ "." ++ (show n)

compileExp_ :: Calculation -> Exp -> Exp -> State CompilerState LLVMProgram
compileExp_ name exp1 exp2 = do
        cmd1 <- compileExp exp1
        (countL, vars) <- get
        cmd2 <- compileExp exp2
        (countR, _) <- get
        put (next countR, vars)
        return $ concat [cmd1, cmd2, line [ tmpRegister $ next countR
                                          , "="
                                          , show name
                                          , "i32"
                                          , tmpRegister countL
                                          , ","
                                          , tmpRegister countR]]         

ass :: String -> String -> String
ass var val = line [ var
                   , "= add i32 0,"
                   , val]

compileExp :: Exp -> State CompilerState LLVMProgram
compileExp (ExpAdd exp1 exp2) = compileExp_ Add exp1 exp2
compileExp (ExpSub exp1 exp2) = compileExp_ Sub exp1 exp2
compileExp (ExpMul exp1 exp2) = compileExp_ Mul exp1 exp2
compileExp (ExpDiv exp1 exp2) = compileExp_ Div exp1 exp2

compileExp (ExpLit int) = do
        (count, vars) <- get
        put (next count, vars)
        return $ ass (tmpRegister $ next count) $ show int

compileExp (ExpVar (Ident var)) = do
        (count, vars) <- get
        let (Just countVar) = Map.lookup var vars 
        put (next count, vars)
        return $ ass (tmpRegister $ next count) $ varRegister var countVar 

compileStmt :: Stmt -> State CompilerState LLVMProgram
compileStmt (SAss (Ident id) exp) = do
        res <- compileExp exp
        (countExp, vars) <- get
        let countVar = Map.findWithDefault 0 id vars
        put (countExp, Map.insert id (next countVar) vars) 
        return $ res ++ (ass (varRegister id $ next countVar) $ tmpRegister countExp) 

compileStmt (SExp exp) = do
        res <- compileExp exp
        (countExp, _) <- get
        return $ res ++ (line ["call void @printInt(i32 " ++ (tmpRegister countExp) ++ ")"])

compile :: Program -> State CompilerState LLVMProgram
compile (Prog stmts) = do
        prgs <- forM stmts compileStmt
        return $ first ++ (concat prgs) ++ end

first = "declare void @printInt(i32)\n\
        \define i32 @main() {\n"
end = "    ret i32 0\n\
      \}\n"


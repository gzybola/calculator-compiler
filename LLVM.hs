module LLVM where

import Control.Monad.State
import Data.Map as Map 
import qualified Data.Text as T
import Data.List 
import AbsInstant

type CompilerState = (Integer, Map String Integer)
type LLVMProgram = String
instance Show Calculation where
        show Add = "add"
        show Sub = "sub"
        show Mul = "mul"
        show Div = "div"

initialCS = (0, Map.empty)

next :: Integer -> Integer
next a = a + 1

compileExp_ :: Calculation -> Exp -> Exp -> State CompilerState LLVMProgram
compileExp_ name exp1 exp2 = do
        cmd1 <- compileExp exp1
        (countL, vars) <- get
        cmd2 <- compileExp exp2
        (countR, _) <- get
        put (next countR, vars)
        return $ intercalate " " [cmd1, cmd1, (intercalate " " [show name
                                             , "i32"
                                             , show countL
                                             , show countR])]         

compileExp :: Exp -> State CompilerState LLVMProgram
compileExp (ExpAdd exp1 exp2) = compileExp_ Add exp1 exp2
compileExp (ExpSub exp1 exp2) = compileExp_ Sub exp1 exp2
compileExp (ExpMul exp1 exp2) = compileExp_ Mul exp1 exp2
compileExp (ExpDiv exp1 exp2) = compileExp_ Div exp1 exp2

compileExp (ExpLit int) = do
        (count, vars) <- get
        put (next count, vars)
        return $ intercalate " " ["%_"
                                 , (show $ next count)
                                 ,  " = %add i32 0, "
                                 ,  show int
                                 , "\n"]

compileExp (ExpVar (Ident var)) = do
        (count, vars) <- get
        let (Just countVar) = Map.lookup var vars 
        put (next count, vars)
        return $ intercalate " " ["%_"
                                 , var
                                 , show $ next count
                                 , " = %add i32 0, "
                                 , var] 


compileStmt :: Stmt -> State CompilerState LLVMProgram
compileStmt (SAss (Ident id) exp) = do
        res <- compileExp exp
        (countExp, vars) <- get
        let countVar = Map.findWithDefault 0 id vars
        put (countExp, Map.insert id (next countVar) vars) 
        return $ intercalate " " [res
                                 , "%_"
                                 , id
                                 , show (next countVar)
                                 , "=" 
                                 , res] 
compileStmt (SExp exp) = compileExp exp
compile :: Program -> State CompilerState LLVMProgram
compile (Prog stmts) = do
        prgs <- forM stmts compileStmt
        return $ concat prgs

--declare void @printInt(i32)
--define i32 @main() {
--        %i1 = add i32 2, 2
--        call void @printInt(i32 %i1)
---        ret i32 2
---}

 
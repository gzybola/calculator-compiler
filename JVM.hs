module JVM where
import Control.Monad.State
import Data.Map as Map

import AbsInstant

type JVMProgram = String
type CompilerState = Map String Int
type DepthStmt = DepthExp 
data DepthExp = None | DExp { count :: Integer
                            , left :: DepthExp
                            , right :: DepthExp
                            }
instance Show Calculation where
        show Add = "iadd"
        show Sub = "isub"
        show Div = "idiv"
        show Mul = "imul"

initialCS = Map.empty

store :: Int -> String
store n 
  | (n <= 3)   = "iload_" ++ (show n)
  |  otherwise = "iload " ++ (show n)  

nodeDepth :: Integer -> Integer -> Integer
nodeDepth a b = max ((min a b) + 1) $ max a b 

countExpDepth_ :: Exp -> Exp -> DepthExp
countExpDepth_ exp1 exp2 = (DExp (nodeDepth dL dR) (DExp dL exp1L exp1R) (DExp dL exp2L exp2R))
                           where
                               (DExp dL exp1L exp1R) = countExpDepth exp1
                               (DExp dR exp2L exp2R) = countExpDepth exp2 

countExpDepth :: Exp -> DepthExp
countExpDepth (ExpLit int)       = DExp 1 None None
countExpDepth (ExpVar id)        = DExp 1 None None
countExpDepth (ExpAdd exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpSub exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpMul exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpDiv exp1 exp2) = countExpDepth_ exp1 exp2
                                   
countStmtDepth :: Stmt -> DepthStmt
countStmtDepth (SAss id exp) = countExpDepth exp
countStmtDepth (SExp exp)    = countExpDepth exp

compileExp_ :: Calculation -> Exp -> Exp -> DepthExp -> State CompilerState JVMProgram
compileExp_ c exp1 exp2 (DExp _ d1 d2)  
  |  count d1 > count d2 = do 
         p1 <- compileExp exp1 d1
         p2 <- compileExp exp2 d2
         return $ concat [p1
                         , p2
                         , (show c) ++ "\n"]
  |  otherwise = do 
         p1 <- compileExp exp2 d2
         p2 <- compileExp exp1 d1
         return $ concat [p1
                         , p2
                         , "swap\n"
                         , (show c) ++ "\n"]


compileExp :: Exp -> DepthExp -> State CompilerState JVMProgram
compileExp (ExpAdd exp1 exp2) dExp = compileExp_ Add exp1 exp2 dExp
compileExp (ExpSub exp1 exp2) dExp = compileExp_ Sub exp1 exp2 dExp
compileExp (ExpMul exp1 exp2) dExp = compileExp_ Mul exp1 exp2 dExp
compileExp (ExpDiv exp1 exp2) dExp = compileExp_ Div exp1 exp2 dExp
compileExp (ExpLit int) _ = return $ "istore " ++ (show int) ++ "\n"

compileStmt :: (Stmt, DepthExp) -> State CompilerState JVMProgram
compileStmt ((SAss (Ident ident) exp), dExp) = do
             vars <- get
             let id = case (Map.lookup ident vars) of
                           Nothing    -> (Map.size vars) + 1  
                           (Just num) -> num
             put (Map.insert ident id vars)
             return $ store id

compileStmt ((SExp e), dExp) = compileExp e dExp 

compile :: Program -> State CompilerState JVMProgram
compile (Prog stmts) = do
        let dStmts = Prelude.map countStmtDepth stmts
        prgs <- forM (zip stmts dStmts) compileStmt
        return $ concat prgs 

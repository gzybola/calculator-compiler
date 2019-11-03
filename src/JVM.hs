module JVM where
import Control.Monad.State
import Data.Map as Map

import AbsInstant

type JVMProgram = String
type CompilerState = Map String Integer
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

istore :: Integer -> String
istore n 
  | (n <= 3)   = "istore_" ++ (show n)
  |  otherwise = "istore " ++ (show n)  

iconst :: Integer -> String
iconst n
  | n == -1              = "iconst_m1"
  | (n >= 0 && n <= 5)   = "iconst_" ++ (show n)
  | otherwise            = "bipush " ++ (show n)

iload :: Integer -> String
iload n 
  | (n >= 0 && n <= 3) = "iload_" ++ (show n)
  | otherwise          = "iload " ++ (show n)


--------------------------STACK DEPTH------------------------------
nodeDepth :: Integer -> Integer -> Integer
nodeDepth a b = max ((min a b) + 1) $ max a b 

countExpDepth_ :: Exp -> Exp -> DepthExp
countExpDepth_ e1 e2 = (DExp (nodeDepth dL dR) (DExp dL e1L e1R) (DExp dR e2L e2R))
                       where
                           (DExp dL e1L e1R) = countExpDepth e1
                           (DExp dR e2L e2R) = countExpDepth e2 

countExpDepth :: Exp -> DepthExp
countExpDepth (ExpLit int)       = DExp 1 None None
countExpDepth (ExpVar id)        = DExp 1 None None
countExpDepth (ExpAdd exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpSub exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpMul exp1 exp2) = countExpDepth_ exp1 exp2
countExpDepth (ExpDiv exp1 exp2) = countExpDepth_ exp1 exp2
                                   
countStmtDepth :: Stmt -> DepthStmt
countStmtDepth (SAss id exp) = countExpDepth exp
countStmtDepth (SExp exp)    = (DExp (count + 1) d1 d2)
                               where 
                                   (DExp count d1 d2) = countExpDepth exp 


------------------------COMPILATION--------------------------------
identation = "  "
line :: String -> String
line strg = identation ++ strg ++ "\n"

compileExp_ :: Calculation -> Exp -> Exp -> DepthExp -> State CompilerState JVMProgram
compileExp_ c exp1 exp2 (DExp _ d1 d2)  
  |  count d1 >= count d2 = do 
         p1 <- compileExp exp1 d1
         p2 <- compileExp exp2 d2
         return $ concat [ p1
                         , p2
                         , line $ show c ]
  |  otherwise = do 
         p1 <- compileExp exp2 d2
         p2 <- compileExp exp1 d1
         return $ concat [ p1
                         , p2 
                         , line "swap"
                         , line $ show c ]


mSize :: CompilerState -> Integer
mSize map = toInteger $ Map.size map

compileExp :: Exp -> DepthExp -> State CompilerState JVMProgram
compileExp (ExpAdd exp1 exp2) dExp  = compileExp_ Add exp1 exp2 dExp
compileExp (ExpSub exp1 exp2) dExp  = compileExp_ Sub exp1 exp2 dExp
compileExp (ExpMul exp1 exp2) dExp  = compileExp_ Mul exp1 exp2 dExp
compileExp (ExpDiv exp1 exp2) dExp  = compileExp_ Div exp1 exp2 dExp
compileExp (ExpLit int) _           = return $ line $ iconst int
compileExp (ExpVar (Ident ident)) _ = do 
             vars <- get
             let (Just id) = Map.lookup ident vars
             return $ line $ iload id 

compileStmt :: (Stmt, DepthExp) -> State CompilerState JVMProgram
compileStmt ((SAss (Ident ident) exp), dExp) = do
             prog <- compileExp exp dExp 
             vars <- get
             let (mId, newVars) = Map.insertLookupWithKey (\_ _ old -> old) ident (mSize vars + 1) vars
             let id = case mId of
                    Nothing    -> mSize newVars
                    (Just num) -> num
             put newVars
             return $ concat [ prog
                             , line $ istore id]

compileStmt ((SExp e), dExp) = do 
             prog <- compileExp e dExp 
             return $ concat [ streamLine 
                             , prog
                             , printLine]  

compile :: Program -> String -> State CompilerState JVMProgram
compile (Prog stmts) file = do
        let dStmts = Prelude.map countStmtDepth stmts
        prgs <- forM (zip stmts dStmts) compileStmt
        vars <- get
        return $ concat [ classLine file
                        , start
                        , stack $ maxStack dStmts
                        , varNumber $ Map.size vars
                        , concat prgs
                        , returnLine
                        , end] 


------------------------COMMANDS --------------------------
maxStack :: [DepthExp] -> Integer
maxStack []                    = 0
maxStack ((DExp count _ _):xs) = max count $ maxStack xs
maxStack (None:xs)             = maxStack xs 

stack :: Integer -> String
stack n =".limit stack " ++ (show n) ++ "\n"

varNumber :: Int -> String
varNumber n = ".limit locals " ++ (show $ n + 1) ++ "\n"

classLine :: String -> String
classLine name = concat [ ".class public "
                     ,  name
                     , "\n" ]

streamLine = line "getstatic java/lang/System/out Ljava/io/PrintStream;"
printLine  = line "invokevirtual java/io/PrintStream/println(I)V"
returnLine = line "return"
end        = ".end method"
start      = ".super java/lang/Object\n\
             \.method public static main([Ljava/lang/String;)V\n"

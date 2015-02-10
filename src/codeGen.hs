{--
expr: xor_expr ('|' xor_expr)*
xor_expr: and_expr ('^' and_expr)*
and_expr: shift_expr ('&' shift_expr)*
shift_expr: arith_expr (('<<'|'>>') arith_expr)*
arith_expr: term (('+'|'-') term)*
term: factor (('*'|'/'|'%'|'//') factor)*
factor: ('+'|'-'|'~') factor | power


<Exp> ::= <Exp> + <Term> |
    <Exp> - <Term> | 
    <Term>

<Term> ::= <Term> * <Factor> |
    <Term> / <Factor> | 
    <Factor>

<Factor> ::= x | y | ... |
    ( <Exp> ) | 
    - <Factor> 
--}
import Data.Monoid
import Data.HashMap.Lazy as M
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans.State.Lazy
import Control.Applicative
import Prelude
--import Data.Traversable
type Statements = [Statement]
type Expressions = [Expression]
data Statement = Assignment Expression Expression
               | Pass
               | Break
               | Continue
               | Return Expression
               | If Expression [Statements]
               | While Expression Statements
               | For Expressions Expressions Statements
               | Print Expression
               deriving (Show)

data Expression = Binop Operator Expression Expression
                | Name String
                | Number Numeral
                deriving (Show)

data Operator = Add | Sub | Mul | Div | And | Or deriving (Show)

data Numeral = Integ Integer
             | Flt  Float
               deriving (Show)


{--
data  = Binop String ASTNode ASTNode
             | Terminal String
             | InParens ASTNode
             | Unary String ASTNode
             | Assign ASTNode ASTNode
--}
data Val = Str String
         | Id String
         | Decimal Float
         | Intgr Integer
         | Bool String
         | None
         | Function [String]
         | Void
         | NotFound String   --identifier not found
           deriving (Eq)

instance Show Val where
  show (Str str) = str
  show (Id str)  = str
  show (Decimal f) = show f
  show (Intgr i) = show i
  show (Bool "T") = "True"
  show (Bool "F") = "False"
  show None = "None"
  show _    = "not implemented"

type Env = M.HashMap String Val
type EvalResult = StateT Env (ErrorT String IO)

envUpdate :: String -> Val -> Env -> Env
envUpdate var val env = M.insert var val env

compatibleValType :: Val -> Val -> Bool
compatibleValType val1 val2 = case (val1, val2) of
  (Str _, Str _) -> True
  (Intgr _, Intgr _) -> True
  (Decimal _, Decimal _) -> True
  (Intgr _, Decimal _) -> True
  (Decimal _, Intgr _) -> True
--  (Digits _, Bool _) -> True
  (None, _     ) -> False
  (Function _ , _) -> False
  (Void, _) -> False
  _         -> False
  
  

evalExpr :: Expression -> EvalResult Val
evalExpr (Name identifier) = do
 
  env <- get
  case M.lookup identifier env of
   Just val -> return val
   _        -> return $ NotFound identifier

evalExpr (Number n) = do
  case n of
   Integ i -> return $ Intgr i
   Flt   f -> return $ Decimal f
  
evalExpr (Binop op left right) = do
  lRes <- evalExpr left
  rRes <- evalExpr right
  case (lRes,rRes) of
   (NotFound l, _) -> throwError $ "Not in scope: " <> l
   (_, NotFound r) -> throwError $ "Not in scope: " <> r
   (_, _)          -> 
     if (compatibleValType lRes rRes)
     then doBinop op lRes rRes
     else throwError $ "Incompatible types: " <> show lRes <> " and " <> show rRes
         where
           doBinop Add (Str l) (Str r) = return $ Str $ l <> r
           doBinop Add (Intgr l) (Intgr r) = return $ Intgr $ l + r
           doBinop Add (Decimal l) (Intgr r) = return $ Decimal $ l + (fromInteger r)
           doBinop Add (Intgr l) (Decimal r) = return $ Decimal $ (fromInteger l) + r

           doBinop Sub (Intgr l) (Intgr r) = return $ Intgr $ l - r
           doBinop Sub (Decimal l) (Intgr r) = return $ Decimal $ l - (fromInteger r)
           doBinop Sub (Intgr l) (Decimal r) = return $ Decimal $ (fromInteger l) - r

           doBinop Mul (Intgr l) (Intgr r) = return $ Intgr $ l * r
           doBinop Mul (Decimal l) (Intgr r) = return $ Decimal $ l * (fromInteger r)
           doBinop Mul (Intgr l) (Decimal r) = return $ Decimal $ (fromInteger l) * r
           
           doBinop Div (Intgr l) (Intgr r) = return $ Intgr $ l `div` r
           doBinop Div (Decimal l) (Intgr r) = return $ Decimal $ l / (fromInteger r)
           doBinop Div (Intgr l) (Decimal r) = return $ Decimal $ (fromInteger l) / r


           doBinop _     _           _     = throwError "Invalid types for binop!"


evalStatements :: Statements -> EvalResult Val
evalStatements stmts = do
  mapM_ evalStatement stmts
  return Void


evalStatement :: Statement -> EvalResult Val
evalStatement (Assignment left right) = do
  lval <-  evalLeft left
  rval <- evalExpr right
  env <- get
  case lval of
   Id name -> do
              put $ envUpdate name rval env
              return Void
   _       -> throwError "not defined!"
   where
     evalLeft :: Expression -> EvalResult Val
     evalLeft (Name identifier) = return $ Id identifier
     evalLeft _                 = throwError "Invalid left side of an assignment!"

evalStatement (If ifTest stmtGroups) = case stmtGroups of
  [] -> throwError "Empty if!"
  (x:xs) -> do
    ifRes <- evalExpr ifTest
    case ifRes of
     None -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     Bool "F" -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     _ ->  evalStatements x

evalStatement (Print expr) = do
  contents <- evalExpr expr
  case contents of
   NotFound name -> throwError $ "identifier " <> name <> " not found!"
   _             -> do
                    liftIO $ print contents
                    return Void
  


program :: Statements
program = [Assignment (Name "a") (Number $ Integ 1), Assignment (Name "b") (Number $ Integ 3), Print $ Binop Add (Name "a") (Name "b"), Print $ Binop Div (Name "a") (Name "c"), Print $ Name "a", Assignment (Name "b") (Number $ Flt 1.5), Print $ Name "b"]


interpret :: Env -> EvalResult Val -> IO (Either String (Val, Env))
interpret env res = runErrorT $ runStateT res env
main = do
  evalRes <- interpret M.empty (evalStatements program)
  case evalRes of
   Right _ -> return ()
   Left err -> print err
  
--evalStatement (Name str) = return $ Str str

{--
if test:
  blahblah
elif test2:
  yadayada
elif test3:
  boooo
else:
  yayyyy

If test 
--}

{--
if test:
  blahblah
else:
  if test2:
    yadayada
  else:
    if test3:
       booo
    else:
       yayyy
-}
     

--eval (InParens node) = "(" <> (codeGen node) <> ")"
--eval (Unary op node) = op <> (codeGen node)


-- b / (c + d)                         
--example = Binop "/" (Terminal "b") $ InParens $ Binop "+" (Terminal "c") (Terminal "d")


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
                | StrLiteral String
                deriving (Show)

data Operator = Add | Sub | Mul | Div | And | Or | Eq | Ne | Gt | Lt | Ge | Le deriving (Show)

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
data Val = Id String
         | Decimal Float
         | Intgr Integer
         | Boolean Bool
         | Str String
         | None
         -- Function Env String Statements
         | Void
         | NotFound String   --identifier not found
           deriving (Eq, Ord)
  

instance Show Val where
  show (Str str) = str
  show (Id str)  = str
  show (Decimal f) = show f
  show (Intgr i) = show i
  show (Boolean b) = show b
  show None = "None"
  show _    = "not implemented"

type Env = M.HashMap String Val
type EvalResult = StateT Env (ErrorT String IO)

envUpdate :: String -> Val -> Env -> Env
envUpdate var val env = M.insert var val env


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

evalExpr (StrLiteral s) = do
  return $ Str s
  
evalExpr (Binop op left right) = do
  lRes <- evalExpr left
  rRes <- evalExpr right
  case (lRes,rRes) of
   (NotFound l, _) -> throwError $ "NameError : name " <> l <> " is not defined"
   (_, NotFound r) -> throwError $ "NameError : name " <> r <> " is not defined"
   (_, _)          -> doBinop op lRes rRes
     where
       compatibleValType :: Val -> Val -> Bool
       compatibleValType val1 val2 = case (val1, val2) of
         (Str _, Str _) -> True
         (Intgr _, Intgr _) -> True
         (Decimal _, Decimal _) -> True
         (Intgr _ , Decimal _) -> True
         (Decimal _, Intgr _)  -> True
         (None, None)  -> True
         (Void, Void) -> True
         (Boolean _, Boolean _) -> True
         _         -> False
  
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
       doBinop Mul (Intgr l) (Str s)     = return $ Str $ concat $ replicate (fromInteger l) s
       doBinop Mul str@(Str _) int@(Intgr _)     = doBinop Mul int str 
           
       doBinop Div (Intgr l) (Intgr r) = return $ Intgr $ l `div` r
       doBinop Div (Decimal l) (Intgr r) = return $ Decimal $ l / (fromInteger r)
       doBinop Div (Intgr l) (Decimal r) = return $ Decimal $ (fromInteger l) / r

       doBinop Eq  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) == r
       doBinop Eq  (Decimal l) (Intgr r) = return $ Boolean $ l == (fromInteger r)
       doBinop Eq  left right  = return $ Boolean $ left == right
       doBinop Ne  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) /= r
       doBinop Ne  (Decimal l) (Intgr r) = return $ Boolean $ l /= (fromInteger r)
       doBinop Ne  left right  = return $ Boolean $ left /= right
       
         
       doBinop Gt  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) > r
       doBinop Gt  (Decimal l) (Intgr r) = return $ Boolean $ l > (fromInteger r)
       doBinop Gt  left        right     = return $ Boolean $ left > right
       doBinop Lt  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) < r
       doBinop Lt  (Decimal l) (Intgr r) = return $ Boolean $ l < (fromInteger r)
       doBinop Lt  left        right     = return $ Boolean $ left < right
       doBinop Le  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) <= r
       doBinop Le  (Decimal l) (Intgr r) = return $ Boolean $ l <= (fromInteger r)
       doBinop Le  left        right     = return $ Boolean $ left < right

       doBinop Ge  (Intgr l) (Decimal r) = return $ Boolean $ (fromInteger l) >= r
       doBinop Ge  (Decimal l) (Intgr r) = return $ Boolean $ l >= (fromInteger r)
       doBinop Ge  left        right     = return $ Boolean $ left >= right

       doBinop _     _           _     = throwError $ "TypeError: Unsupported operand types(s) for " <> show op <> show lRes <> " and " <> show rRes

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
     Boolean False -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     _ ->  evalStatements x

evalStatement (Print expr) = do
  contents <- evalExpr expr
  case contents of
   NotFound name -> throwError $ "NameError: name " <> name <> " is not defined!"
   _             -> do
                    liftIO $ print contents
                    return Void
  
program :: Statements
program = [Assignment (Name "a") (Number $ Integ 5),
           Assignment (Name "b") (Number $ Flt 2.0),
           If (Binop Eq (Name "a") (Name "b"))
              [[Print $ StrLiteral "equal"],
               [If (Binop Ge (Name "a") (Number $ Integ 2))
                   [[Assignment (Name "c") (Binop Add (Name "a") (Name "b")),
                     Print $ Name "c"],
                    [If (Binop Eq (Name "b") (Number $ Flt 2.0))
                       [[Assignment (Name "d") (Binop Div (Name "a") (Name "b")),
                         Print $ Binop Sub (Name "d") (Name "a")],

                        [Print $ StrLiteral "blah"]]]]]]]


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


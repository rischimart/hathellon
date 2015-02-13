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
import Control.Monad.Trans.Either
import Control.Applicative
import Prelude
import Debug.Trace
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
               | FunDef String [Argument] Statements

               deriving (Show)

data Expression = Binop Operator Expression Expression
                | FunApp String [Expression]
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

data Argument = Identifier String
              | Default Expression Expression
              | Vararg
              | Kwargs
                deriving (Show)

data Val = Id String
         | Decimal Float
         | Intgr Integer
         | Boolean Bool
         | Str String
         | None
         | Function Env String [Argument] Statements
         | Void
         | NotFound String   --identifier not found
         | Ret Val
         | Brk

instance Eq Val where
  (Intgr i)  == (Intgr j) = i == j
  (Decimal f) == (Decimal g) = f == g
  (Decimal f) == (Intgr i) = f == (fromInteger i)
  (Intgr i) == (Decimal f) = (fromInteger i) == f
  (Str s1)  ==  (Str s2)   = s1 == s2
  (Boolean b1) == (Boolean b2)  = b1 == b2
  None      == None       = True
  (==) _ _                = False                        
  
instance Ord Val where
  (Intgr i)  < (Intgr j) = i < j
  (Decimal f) < (Decimal g) = f < g
  (Decimal f) < (Intgr i) = f < (fromInteger i)
  (Intgr i) < (Decimal f) = (fromInteger i) < f
  (Str s1)  <  (Str s2)   = s1 < s2
  (Boolean b1) < (Boolean b2) = b1 < b2
  (Str _)  <  (Decimal _) = False
  (Str _) <   (Intgr _)   = False
  (<) _ _                 = False


instance Show Val where
  show (Str str) = str
  show (Id str)  = str
  show (Decimal f) = show f
  show (Intgr i) = show i
  show (Boolean b) = show b
  show None = "None"
  show (Ret v) = show v
  show _    = "not implemented"

type Env = M.HashMap String Val
type EvalResult = StateT Env (ErrorT String IO)

envUpdate :: String -> Val -> Env -> Env
envUpdate var val env = M.insert var val env

nameError :: String -> String
nameError name = "name " <> "' " <> name <> " '" <> " is not defined"

makeErrorMsg :: String -> String -> String
makeErrorMsg t err = t <> ": " <> err

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


evalExpr (FunApp funName args) = do
  env <- get
  case M.lookup funName env of
   Just (Function defenv name args' body) -> do
     let expectedLen = length args'
         givenLen = length args
     if expectedLen /= givenLen
       then let err = funName <> " takes exactly " <>
                      show expectedLen <> "arguments (" <>
                      show givenLen <> " given)"
            in throwError $ makeErrorMsg "TypeError" err
       else do
         vals <- mapM evalExpr args
         traceM $ "the vals of args are: " <> show vals
         let unpackedargs = Prelude.foldr evalArg [] args'
         defaults <- foldM evalDefaults M.empty args'
         let localEnv = (M.fromList (zip unpackedargs vals) `M.union`
                         defaults) `M.union` env
         put localEnv
         appVal <- evalStatements body
         traceM "evaluation done"
         traceM ("the value of " <> show funName <> " is: " <> show appVal)
         put env
         return appVal
           where
             evalArg (Identifier n) lst = n : lst 
             evalArg   _            lst = lst
      
             evalDefaults e (Default left right) = do
               l <- evalExpr left
               r <- evalExpr right
               case l of
                NotFound var -> return $ envUpdate var r e
                Id  var -> return $ envUpdate var r e
                _ ->  return e
             evalDefaults e _ = return e
    
   _  -> throwError $ nameError funName

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


       doBinop Eq l r = return $ Boolean $ l == r
       doBinop Ne l r = return $ Boolean $ l /= r
       
       doBinop Gt l r = return $ Boolean $ l > r
       doBinop Lt l r = return $ Boolean $ l < r
       doBinop Le l r = return $ Boolean $ l < r
       doBinop Ge l r = return $ Boolean $ l >= r

       doBinop _  _ _ = throwError $ "TypeError: Unsupported operand types(s) for " <> show op <> show lRes <> " and " <> show rRes

evalStatements :: Statements -> EvalResult Val
evalStatements stmts = do
  rest <- liftM (dropWhile (not . shouldExit)) $ mapM evalStatement stmts
  case rest of
   [] -> return Void
   x:_ -> do
     traceM ("the value of  is: " <> show x)
     return x
  where
    shouldExit :: Val -> Bool
    shouldExit (Ret _) = True
    shouldExit Brk     = True
    shouldExit _       = False

evalStatement :: Statement -> EvalResult Val
evalStatement (Assignment left right) = do
  lval <- evalLeft left
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


evalStatement (FunDef name args body) = do
  env <- get
  put $ envUpdate name (Function env name args body) env
  return Void

         
evalStatement (Return expr) = do
  val <- evalExpr expr
  return $ Ret val
  
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
program = [FunDef "fib" [Identifier "n"]
                        [If (Binop Le (Name "n") (Number $ Integ 0))
                            [[Return $ Number $ Integ 1]],
                         If (Binop Eq (Name "n") (Number $ Integ 1))
                            [[Return $ Number $ Integ 2]],
                         Return $ Binop Add (FunApp "fib" [Binop Sub (Name "n") (Number $ Integ 2)]) (FunApp "fib" [Binop Sub (Name "n") (Number $ Integ 1)])],
                         
           Print $ FunApp "fib" [Number $ Integ 3]]


interpret :: Env -> EvalResult Val -> IO (Either String (Val, Env))
interpret env res = runErrorT $ runStateT res env
main = do
  evalRes <- interpret M.empty (evalStatements program)
  case evalRes of
   Right _ -> return ()
   Left err -> print err
  

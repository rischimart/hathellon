import Data.Monoid
--import Data.Foldable
import Data.Traversable (traverse, sequence, sequenceA)
import Data.HashMap.Lazy as M
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.Trans.State.Lazy
--import Control.Monad.Trans.Either
import Control.Monad.Cont
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
               | For Expression Expression Statements
               | Print Expression
               | FunDef String [Argument] Statements

               deriving (Show)

data Expression = Binop Operator Expression Expression
                | FunApp String [Expression]
                | Name String
                | Number Numeral
                | StrLiteral String
                | List [Expression]
                | Tuple [Expression]
                | Null
                deriving (Show)

data Operator = Add | Sub | Mul | Div | Mod | And | Or | Eq | Ne | Gt | Lt | Ge | Le deriving (Show)

data Numeral = Integ Integer
             | Flt  Float
               deriving (Show)


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
         | Iterable {kind :: String, elements :: [Val]}
         | None
         | Function Env String [Argument] Statements
         | Void
         | NotFound String   --identifier 
         | Ret Val
         | Brk
         | Cnt

instance Eq Val where
  (Intgr i)  == (Intgr j) = i == j
  (Decimal f) == (Decimal g) = f == g
  (Decimal f) == (Intgr i) = f == (fromInteger i)
  (Intgr i) == (Decimal f) = (fromInteger i) == f
  (Str s1)  ==  (Str s2)   = s1 == s2
  (Boolean b1) == (Boolean b2)  = b1 == b2
  None      == None       = True
  _   ==  _               = False                        
  
instance Ord Val where
  (Intgr i)  < (Intgr j) = i < j
  (Decimal f) < (Decimal g) = f < g
  (Decimal f) < (Intgr i) = f < (fromInteger i)
  (Intgr i) < (Decimal f) = (fromInteger i) < f
  (Str s1)  <  (Str s2)   = s1 < s2
  (Boolean b1) < (Boolean b2) = b1 < b2
  (Str _)  <  (Decimal _) = False
  (Str _) <   (Intgr _)   = False
  _       <    _          = False
  left   <=   right      = left < right || left == right
  left   >=   right      = left > right || left == right

instance Show Val where
  show (Str str) = str
  show (Id str)  = str
  show (Decimal f) = show f
  show (Intgr i) = show i
  show (Boolean b) = show b
  show None = "None"
  show (Ret v) = show v
  show Void    = ""
  show (Iterable t vals) = show vals

type Env = M.HashMap String Val
type EvalResult a = ContT Val (StateT Env (ErrorT String IO)) a

envUpdate :: String -> Val -> Env -> Env
envUpdate var val env = M.insert var val env

nameError :: String -> String
nameError name = "name " <> "' " <> name <> " '" <> " is not defined"

makeErrorMsg :: String -> String -> String
makeErrorMsg t err = t <> ": " <> err


evalExpr :: Expression -> EvalResult Val
evalExpr Null = return Void

evalExpr (Name identifier) = do
  env <- lift get
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
  env <- lift get
  case M.lookup funName env of
   Just (Function defenv name args' body) -> do
     let expectedLen = length args'
         givenLen = length args
     if expectedLen /= givenLen
       then let err = funName <> " takes exactly " <>
                      show expectedLen <> "arguments (" <>
                      show givenLen <> " given)"
            in lift $ throwError $ makeErrorMsg "TypeError" err
       else do
         vals <- mapM evalExpr args
         --traceM $ "the vals of args are: " <> show vals
         let unpackedargs = Prelude.foldr evalArg [] args'
         defaults <- foldM evalDefaults M.empty args'
         let localEnv = (M.fromList (zip unpackedargs vals) `M.union`
                         defaults) `M.union` env
         lift $ put localEnv
         appVal <- evalStatements body
         --traceM "evaluation done"
         --traceM ("the value of " <> show funName <> " is: " <> show appVal)
         lift $ put env
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
    
   _  -> lift $ throwError $ nameError funName

evalExpr (Binop op left right) = do
  lRes <- evalExpr left
  rRes <- evalExpr right
  case (lRes,rRes) of
   (NotFound l, _) -> lift $ throwError $ "NameError : name " <> l <> " is not defined"
   (_, NotFound r) -> lift $ throwError $ "NameError : name " <> r <> " is not defined"
   (Ret l, Ret r) -> doBinop op l r
   (Ret l, rhs)   -> doBinop op l rhs
   (lhs, Ret r)  -> doBinop op lhs r
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

       doBinop Add (Iterable lkind lvals) (Iterable rkind rvals)
         | lkind == rkind = return $ Iterable lkind (lvals ++ rvals)
         | otherwise = lift $ throwError $ makeErrorMsg "TypeError" "Cannot concatenate " <> lkind <> " and " <> rkind
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
       doBinop Le l r = return $ Boolean $ l <= r
       doBinop Ge l r = return $ Boolean $ l >= r

       doBinop o l r = lift $ throwError $ "TypeError: Unsupported operand types(s) for " <> show o <> ": " <> show l <> " and " <> show r

evalExpr (List exprs) = do
  vals <- traverse evalExpr exprs 
  return $ Iterable "list" vals

evalExpr (Tuple exprs) = do
  vals <- traverse evalExpr exprs 
  return $ Iterable "tuple" vals

evalStatements :: Statements -> EvalResult Val
evalStatements stmts = do
  --mapM_ evalStatement stmts

  results <- callCC $ \brk -> do
                              forM stmts $ \stmt -> do
                                res <- evalStatement stmt
                                when (shouldExit res) $ brk [res]
                                return res
  case results of
   (r@(Ret _) : []) -> return r
   (b@(Brk) : [])  -> return b
   (c@(Cnt) : []) -> return c
   _  -> return Void

  {--
  rest <- (dropWhile (not . shouldExit)) <$> mapM evalStatement stmts
  case rest of
   [] -> return Void
   x:_ -> do
     traceM ("the value of  is: " <> show x)
     return x
--}


evalStatement :: Statement -> EvalResult Val
evalStatement (Assignment left right) = do
  lval <- evalLeft left
  rval <- evalExpr right
  env <- lift get
  case lval of
   Id name -> do
              lift $ put $ envUpdate name rval env
              return Void
   _       -> lift $ throwError "not defined!"

evalStatement (FunDef name args body) = do
  env <- lift get
  lift $ put $ envUpdate name (Function env name args body) env
  return Void

         
evalStatement (Return expr) = do
  val <- evalExpr expr
  return $ Ret val

evalStatement Break  = return Brk
evalStatement Continue = return Cnt
  
evalStatement (If ifTest stmtGroups) = case stmtGroups of
  [] -> lift $ throwError "Empty if!"
  (x:xs) -> do
    ifRes <- evalExpr ifTest
    case ifRes of
     None -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     Boolean False -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     _ ->  evalStatements x

evalStatement (Print expr) = do
  contents <- evalExpr expr
  case contents of
   NotFound name -> lift $ throwError $ makeErrorMsg "NameError" name <> " is not defined!"
   _             -> do
                    liftIO $ print contents
                    return Void

  

evalStatement (For iterator iterable body) = do
  itr <- evalExpr iterator
  itrbl <- evalExpr iterable
  if not (isIterable itrbl)
    then lift $ throwError $ makeErrorMsg "TypeError" (show itrbl) <> " object is not iterable"
    else do
         results <- callCC $ \exit -> do
           forM (elements itrbl) $ \elemt -> do
             bindings <- makeBindings itr elemt
             env <- lift get
             let localEnv = (fromList bindings) `M.union` env
             lift $ put localEnv
             res <- evalStatements body
             lift $ put env
             when (shouldBreak res) $ exit [res]
             return res
             
         case results of
          (r@(Ret _) : []) -> return r
          _  -> return Void
    
  where
    isIterable (Iterable _ _) = True
    isIterable _              = False


evalStatement w@(While predicate body) = do
  p <- evalExpr predicate
  case p of
   Boolean False -> return Void
   None -> return Void
   _ -> do
     res <- evalStatements body
     case res of
      r@(Ret _) -> return r
      Brk -> return Void
      _ -> evalStatement w

evalLeft :: Expression -> EvalResult Val
evalLeft (Name identifier) = return $ Id identifier
evalLeft _                 = lift $ throwError "Invalid left value!"

shouldExit :: Val -> Bool
shouldExit (Ret _) = True
shouldExit Brk     = True
shouldExit Cnt    = True
shouldExit _      = False

shouldBreak :: Val -> Bool
shouldBreak (Ret _) = True
shouldBreak  Brk    = True
shouldBreak  _      = False

makeBindings :: Val -> Val -> EvalResult [(String, Val)]
makeBindings (NotFound name) val = return [(name, val)]
makeBindings (Id name) val = return [(name, val)]
makeBindings (Iterable _ keys) (Iterable _ vals)
  | (length keys) > (length vals) = lift $ throwError $ makeErrorMsg "ValueError" "too few values to unpack"
  | (length keys) < (length vals) = lift $ throwError $ makeErrorMsg "ValueError" "too many values to unpack"
  | otherwise = do
      b <- traverse (uncurry makeBindings) (zip keys vals) 
      return $ concat b
makeBindings (Iterable _ _) v = lift $ throwError $ makeErrorMsg "TypeError" (show v) <> " object is not iterable"
program :: Statements
program = [FunDef "fib" [Identifier "n"]
                        [If (Binop Le (Name "n") (Number $ Integ 0))
                            [[Return $ Number $ Integ 1]],
                         If (Binop Eq (Name "n") (Number $ Integ 1))
                            [[Return $ Number $ Integ 1]],
                         Return $ Binop Add (FunApp "fib" [Binop Sub (Name "n") (Number $ Integ 2)]) (FunApp "fib" [Binop Sub (Name "n") (Number $ Integ 1)])],
                         
           Print $ FunApp "fib" [Number $ Integ 10]]


program2 :: Statements
program2 = [FunDef "test" [Identifier "n"]
                          [If (Binop Eq (Name "n") (Number $ Integ 0))
                            [[Return $ Number $ Integ 0]],
                           Return $ Binop Add (Name "n") (FunApp "test" [Binop Sub (Name "n") (Number $ Integ 1)])],
 
            Print $ FunApp "test" [Number $ Integ 10]]
{--

for a, b in [(1, 2), (3, 4), (5, 6), (0, 8)]:
 if a > 3:
   continue
 print a
 print b * "*"
 print "yayyy!"

--}
program3 :: Statements
program3 = [For (Tuple [Name "a", Name "b"])
             (List [Tuple [Number $ Integ 1, Number $ Integ 2], Tuple [Number $ Integ 3, Number $ Integ 4], Tuple [Number $ Integ 5, Number $ Integ 6], Tuple [Number $ Integ 0, Number $ Integ 8]])
             [If (Binop Gt (Name "a") (Number $ Integ 3))
              [[Continue]],
              Print $ Name "a",
              Print $ Binop Mul (Name "b") (StrLiteral "*")],
            Print $ StrLiteral "Yayyyy!"]


{--
for c in [1, 2, 4, 8]:
  for a, b in [(1, 2), (3, 4), (5, 6), (7, 8)]:
   if a > 3:
    break
   print a + c
   print b * "*"

--}
program4 :: Statements
program4 = [For (Name "c" ) (List [Number $ Integ 1, Number $ Integ 2, Number $ Integ 4, Number $ Integ 8])
            [For (Tuple [Name "a", Name "b"])
             (List [Tuple [Number $ Integ 1, Number $ Integ 2], Tuple [Number $ Integ 3, Number $ Integ 4], Tuple [Number $ Integ 5, Number $ Integ 6]])
             [If (Binop Gt (Name "a") (Number $ Integ 3))
              [[Break]],
              Print $ Binop Add (Name "a")  (Name "c"),
              Print $ Binop Mul (Name "b") (StrLiteral "*")]],
            Print $ StrLiteral "Yayyyy!"]


{--
a = 2
while a < 10 :
  print a
  if a == 2:
    a = a + 4
    continue
  a = a + 1
--}
program5 :: Statements
program5 = [Assignment (Name "a") (Number $ Integ 2),
            While (Binop Lt (Name "a") (Number $ Integ 10))
                  [If (Binop Eq (Name "a") (Number $ Integ 2))
                   [[Assignment (Name "a") (Binop Add (Name "a") (Number $ Integ 4)),
                     Continue]],
                   Print $ Name "a",
                   Assignment (Name "a") (Binop Add (Name "a") (Number $ Integ 2))],
            Print $ StrLiteral "Done!"]
                                   

interpret :: Env -> EvalResult Val -> IO (Either String (Val, Env))
interpret env res = runErrorT $ runStateT (runContT res return) env
main = do
  evalRes <- interpret M.empty (evalStatements program5)
  case evalRes of
   Right _ -> return ()
   Left err -> print err

  --bindings <- makeBindings (Id "a") (Iterable "Tuple" [Intgr 1, Intgr 2])
  --print bindings
     
     

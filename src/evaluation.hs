module Evaluation where
import Data.Monoid
import Data.Traversable (traverse)
import Data.HashMap.Lazy as M
import Control.Monad.Reader
import Control.Monad.Error
import Control.Monad.Trans.State.Lazy
import Control.Monad.Cont
import Control.Monad.Coroutine
import Prelude

import Grammar


data Val = Id String
         | Decimal Float
         | Intgr Integer
         | Boolean Bool
         | Str String
         | Iterable {kind :: String, elements :: [Val]}
         | None
         | Function Env String [Argument] Statements
         | Generator {env :: Env,
                      init :: Statements,
                      rep :: Statements,
                      gen :: Gen Val [] Val}
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

type Gen a m x = Coroutine ((,) a) m x

envUpdate :: String -> Val -> Env -> Env
envUpdate var val env = M.insert var val env

throw :: String -> EvalResult a
throw = lift . throwError

{--
getEnv :: EvalResult Env
getEnv = lift . get
--}

nameError :: String -> String
nameError name = "name " <> "' " <> name <> " '" <> " is not defined"

makeErrorMsg :: String -> String -> String
makeErrorMsg t err = t <> ": " <> err

yield :: Monad m => Val -> Gen Val m ()
yield a = suspend (a, return ())

getNext :: (Monad m) => Gen Val m Val -> m (Val, Gen Val m Val)
getNext prev = do
  -- return x = Coroutine (return (Right x))
  resume prev >>= either (\(a, g) -> return (a, g)) (\x -> return (x, return x))
  -- Generator {env :: Env, init :: Statements, rep :: Statements}
  
    

runToEnd :: Val -> [Val]
runToEnd (Generator env init rep _) = []


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
            in throw $ makeErrorMsg "TypeError" err
       else do
         vals <- mapM evalExpr args
         let unpackedargs = Prelude.foldr evalArg [] args'
         defaults <- foldM evalDefaults M.empty args'
         let localEnv = (M.fromList (zip unpackedargs vals) `M.union`
                         defaults) `M.union` env
         lift $ put localEnv
         appVal <- evalStatements body
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
    
   _  -> throw $ nameError funName

evalExpr (Binop op left right) = do
  lRes <- evalExpr left
  rRes <- evalExpr right
  case (lRes,rRes) of
   (NotFound l, _) -> throw $ "NameError : name " <> l <> " is not defined"
   (_, NotFound r) -> throw $ "NameError : name " <> r <> " is not defined"
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
         | otherwise = throw $ makeErrorMsg "TypeError" "Cannot concatenate " <> lkind <> " and " <> rkind
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

       doBinop o l r = throw $ "TypeError: Unsupported operand types(s) for " <> show o <> ": " <> show l <> " and " <> show r

evalExpr (List exprs) = do
  vals <- traverse evalExpr exprs 
  return $ Iterable "list" vals

evalExpr (Tuple exprs) = do
  vals <- traverse evalExpr exprs 
  return $ Iterable "tuple" vals

evalStatements :: Statements -> EvalResult Val
evalStatements stmts = do
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


evalStatement :: Statement -> EvalResult Val
evalStatement (Assignment left right) = do
  lval <- evalLeft left
  rval <- evalExpr right
  env <- lift get
  case lval of
   Id name -> do
              lift $ put $ envUpdate name rval env
              return Void
   _       -> throw "not defined!"

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
  [] -> throw "Empty if!"
  (x:xs) -> do
    ifRes <- evalExpr ifTest
    case ifRes of
     None -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     Boolean False -> if not (Prelude.null xs) then evalStatements (head xs) else return Void
     _ ->  evalStatements x

evalStatement (Print expr) = do
  contents <- evalExpr expr
  case contents of
   NotFound name -> throw $ makeErrorMsg "NameError" name <> " is not defined!"
   _             -> do
                    liftIO $ print contents
                    return Void

evalStatement (For iterator iterable body) = do
  itr <- evalExpr iterator
  itrbl <- evalExpr iterable
  if not (isIterable itrbl)
    then throw $ makeErrorMsg "TypeError" (show itrbl) <> " object is not iterable"
    else do
         results <- callCC $ \exit -> do
           forM (getElements itrbl) $ \elemt -> do
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

    getElements itr@(Iterable _ _) = elements itr
    

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
evalLeft _                 = throw "Invalid left value!"

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
  | (length keys) > (length vals) = throw $ makeErrorMsg "ValueError" "too few values to unpack"
  | (length keys) < (length vals) = throw $ makeErrorMsg "ValueError" "too many values to unpack"
  | otherwise = do
      b <- traverse (uncurry makeBindings) (zip keys vals) 
      return $ concat b
makeBindings (Iterable _ _) v = throw $ makeErrorMsg "TypeError" (show v) <> " object is not iterable"


genSeq :: a -> (a -> a) -> [a]
genSeq initVal next =
  let f = initVal : (fmap next f) in f



program2 :: Statements
program2 = [FunDef "test" [Identifier "n"]
                          [If (Binop Eq (Name "n") (Number $ Integ 0))
                            [[Return $ Number $ Integ 0]],
                           Return $ Binop Add (Name "n") (FunApp "test" [Binop Sub (Name "n") (Number $ Integ 1)])],
 
            Print $ FunApp "test" [Number $ Integ 10]]


interpret :: Env -> EvalResult Val -> IO (Either String (Val, Env))
interpret env res = runErrorT $ runStateT (runContT res return) env
main = do
  evalRes <- interpret M.empty (evalStatements program2)
  case evalRes of
   Right _ -> return ()
   Left err -> print err

     
     

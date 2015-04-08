module Grammar where

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
               | Next Expression

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


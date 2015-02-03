{-# LANGUAGE FlexibleContexts #-}
module Lexer where
import           Control.Applicative           hiding (many, (<|>))
import           Control.Concatenative
import           Control.Monad.State           as St
import           System.Environment
import           Text.Parsec.Combinator
import           Text.Parsec.Indent
import           Text.Parsec.Language          as Lang
import           Text.Parsec.Prim              as Prm hiding (State)
import           Text.Parsec.Token             as Tk
import           Text.ParserCombinators.Parsec as Pasc

keywords = [
  "False",      "class",      "finally",    "is",         "return",
  "None",       "continue",   "for",        "lambda",     "try",
  "True",       "def",        "from",       "nonlocal",   "while",
  "and",        "del",        "global",     "not",        "with",
  "as",         "elif",       "if",         "or",         "yield",
  "assert",     "else",       "import",     "pass",       "break",
  "except",     "in",         "raise"]

headerWords = ["class", "def", "elif", "else", "except", "finally",
               "for", "if", "try", "while", "with", "yield"]

--newtype Line = Line String
--data CodeBlock = Singleton Line | Snippet Line CodeBlock
delimiters = [
  "(",       ")",       "[",       "]",       "{",       "}",
  ",",       ":",       ".",       ";",       "@",       "=",
  "+=",      "-=",      "*=",      "/=",      "//=",     "%=",
  "&=",      "|=",      "^=",      ">>=",     "<<=",     "**="]

operators = ["+", "-", "*", "**", "/", "^", "&", "|", "%", "=", "+=",
             "-=","*=","/=","//=", "%=", "&=", "|=", "^=", ">>=", "<<=", "**="]

--nonSpacing = noneOf " \t\n\r"
--validIDSymbols = oneOf "_?!"
pythonLangDef = emptyDef {
  commentLine = "#",
  identStart = letter <|> char '_',
  identLetter = alphaNum <|> char '_',
  commentStart = "'''",
  commentEnd = "'''",
  reservedNames = keywords,
  reservedOpNames = operators,

  caseSensitive = True
}


pythonTokenLexer = Tk.makeTokenParser pythonLangDef
idParser = Tk.parens pythonTokenLexer
keywordParser = Tk.reserved pythonTokenLexer
operatorParser = Tk.reservedOp pythonTokenLexer
stringParser = Tk.stringLiteral pythonTokenLexer
--naturalParer =

data Token = Keyword String
           | Identifier String
           | Literal String
           | Indent
           | Dedent
           | Operator String
           | Delimiter String
           | Comment
           | EOF
             deriving(Eq, Show)


type IndParser a = IndentParser String () a

data CodeSnippet = SingleLine [String] | Bloc [String] [CodeSnippet]

instance Show CodeSnippet where
  show (SingleLine str) = "single " ++ show str
  show (Bloc header snippets) = "{" ++ (show header) ++ ":\n" ++ unlines (fmap show snippets) ++ "}"

parseSingleComment :: IndParser String
parseSingleComment = do
  spaces
  char '#'
  manyTill (noneOf "\n\r") (oneOf "\n\r")



parseHead :: IndParser [String]
parseHead = do
  headerWord <- choice $ (Prm.try . string) <$> headerWords
  spaces
  toks <- (many1 $ noneOf "\n\r :")  `sepBy` spaces
  char ':'
  skipMany $ oneOf "\n\r \t"
  return (headerWord : toks)


checkBlockIndent :: (Stream s (St.State SourcePos) z) => IndentParser s u ()
checkBlockIndent = do
    s <- get
    p <- getPosition
    if biAp sourceColumn (==) p s then parserFail "block not indented" else return ()


parseLineInBlock :: IndParser CodeSnippet
parseLineInBlock = do
  many $ oneOf "\n\r"
  notFollowedBy spaces
  line <- many1 $ noneOf "\n\r"
  many1 $ oneOf "\n\r"  <* spaces
  return $ SingleLine $ words line

parseBlock :: IndParser CodeSnippet
parseBlock = do
  header <- parseHead
  checkBlockIndent
  body <- block $ parseBlock <|> parseLineInBlock
  return $ Bloc header body


parseMultiSnippets :: IndParser [CodeSnippet]
parseMultiSnippets = do
  snippets <- block $ parseBlock <|> parseLineInBlock
  eof
  return snippets

main = do
  [filename] <- getArgs
  file <- readFile filename
  let result = runIndent filename $ runParserT parseMultiSnippets () filename file
  case result of
   Left err -> print err
   Right val -> sequence_ $ map print val


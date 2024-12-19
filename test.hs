import Data.Char

lexer:: String -> [Token]
lexDigit :: String -> [Token]
lexString :: String -> [Token]
lexParenBrace :: String -> [Token]
extract :: Maybe([Token], a) -> Maybe a

newtype Parser a = Parser {runParser :: [Token] -> Maybe ([Token],a) }

extract input = do 
  (a,b) <- input
  Just(b) 

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> do
    (input',x) <- p input
    Just(input',f x)

instance Applicative Parser where
  pure x = Parser $ \input -> Just(input,x)
  (Parser p1) <*> (Parser p2) = Parser $ \input -> do 
    (input',f) <- p1 input
    (input'',a) <- p2 input'
    Just (input'', f a)

tokenP :: Token -> Parser Token
tokenP t = Parser $ f
  where 
    f (x:xs)
      | (tokentype t == Identifier || tokentype t == Literal) && tokenvalue t == Nothing = Just(xs,x) 
      | x == t = Just(xs,t)
      |otherwise = Nothing
    f [] = Nothing

funcP :: [Token] -> Maybe ([Token],AST Token)
funcP input = do
  (list,(a:b:c)) <- runParser (sequenceA (map tokenP [Token Keyword (Just "int"),Token Identifier Nothing, Token OpenParen Nothing, Token CloseParen Nothing,Token OpenBrace Nothing])) input
  (list',ast) <-statP list
  (list'',d) <- runParser (tokenP (Token CloseBrace Nothing)) list'
  Just(list'',Function b (ast))

statP :: [Token] -> Maybe ([Token], AST Token)
statP input = do
  (list,a) <- runParser (tokenP (Token Keyword (Just "return"))) input
  (list', ast) <- expP list
  (list'', b) <- runParser (tokenP (Token Semicolon Nothing)) list'
  Just(list'', Statement ast)  

expP :: [Token] -> Maybe ([Token], AST Token)
expP input = do
  (list,ast) <- runParser (tokenP (Token Literal Nothing)) input
  Just(list, Expression ast)

data Token = Token {tokentype :: Tokentype , tokenvalue :: Maybe String}
   deriving(Show, Eq)

data Tokentype = Identifier | End | Keyword | Semicolon | OpenParen | CloseParen | OpenBrace | CloseBrace | Literal | Invalid
  deriving(Show, Eq)

data AST t = Program (AST t)| Function t (AST t) | Statement (AST t)| Expression t 
  deriving(Show)

parser = undefined

lexDigit cs = Token Literal (Just s1) : lexer s2 
  where (s1,s2) = (span isDigit cs)

lexString cs = case s1 of 
  "if" -> Token Keyword (Just s1) : lexer s2
  "else" -> Token Keyword (Just s1) : lexer s2
  "int" -> Token Keyword (Just s1) : lexer s2
  "return" -> Token Keyword (Just s1) : lexer s2
  _ -> Token Identifier (Just s1) : lexer s2
  where (s1,s2) = (span isAlphaNum cs)

lexParenBrace fs@(f:fs') = case f of
  '(' -> Token OpenParen Nothing : lexer fs'
  ')' -> Token CloseParen Nothing : lexer fs'
  '{' -> Token OpenBrace Nothing : lexer fs'
  '}' -> Token CloseBrace Nothing : lexer fs'
  ';' -> Token Semicolon Nothing : lexer fs'
  _ -> Token Invalid Nothing : lexer fs

lexer []  = Token End Nothing : []
lexer fs@(f:fs')
  | isSpace f = lexer fs'
  | isDigit f = lexDigit fs
  | isAlphaNum f = lexString fs
  | otherwise = lexParenBrace fs



main = do 
  s <- readFile "test.txt"
  print (lexer s)
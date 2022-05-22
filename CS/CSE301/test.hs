-- Basic parser with applicative and monadic combinators.
--
-- Based upon chapter 13 of Graham Hutton's "Programming in Haskell" (2nd ed)
--
-- See also Hutton's Computerphile video on "Functional Parsing"
-- [https://www.youtube.com/watch?v=dDtZLm7HIJs]
--
-- The code is commented by Paul-André Melliès [https://www.irif.fr/~mellies/],
-- inspired by a blog post by Eli Bendersky [http://eli.thegreenplace.net].


import Control.Applicative
import Data.Char

newtype Parser a = P (String -> [(a,String)])

-- applies the parser to the string
parse :: Parser a -> String -> [(a,String)]
parse (P p) inp = p inp

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  -- once the value v of type a has been obtained by the parser
  -- give that value v as argument to the function g :: a -> b
  fmap g p = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> [(g v,out)])

instance Applicative Parser where
  -- pure :: a -> Parser a
  -- the parser pure v returns the value v and does not touch at the string
  pure v = P (\inp -> [(v,inp)])

  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  -- the parser pg <*> px applies the parser pg to the input string inp
  -- when the parser pg applied on inp produces (g,out) where g :: a -> b
  -- then the parser pg <*> px applies the function g :: a -> b
  -- to the value of type a parsed by px on remaining string out.
  pg <*> px = P (\inp -> case parse pg inp of
                            []        -> []
                            [(g,out)] -> parse (fmap g px) out)

instance Monad Parser where
  -- return :: a -> Parser a
  return = pure

  -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= f = P (\inp -> case parse p inp of
                          []        -> []
                          [(v,out)] -> parse (f v) out)

instance Alternative Parser where
  -- empty :: Parser a
  empty = P (\inp -> [])

  -- (<|>) :: Parser a -> Parser a -> Parser a
  -- the parser p <|> q makes p parse the input string
  -- and then returns the result (v,out) if the parsing works
  -- otherwise it makes the parser q parse the input string
  p <|> q = P (\inp -> case parse p inp of
                          []        -> parse q inp
                          [(v,out)] -> [(v,out)])

-- Basic building block: item parses a single char from the input.
-- Note that the type of the parser item is Parser Char because 
-- it returns a char together with the tail of the input string
item :: Parser Char
item = P (\inp -> case inp of
                    []      -> []
                    (x:xs)  -> [(x,xs)])

-- sat takes a test function p and then returns empty
-- when the first character of the list is not accepted by p
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else empty

-- digit is the parser which returns the first character
-- (together with the tail of the input string)
-- of the input string when it is a digit
digit :: Parser Char
digit = sat isDigit

lower :: Parser Char
lower = sat isLower

upper :: Parser Char
upper = sat isUpper

letter :: Parser Char
letter = sat isAlpha

alphanum :: Parser Char
alphanum = sat isAlphaNum

-- char x is the parser which returns the first character
-- (together with the tail of the input string)
-- of the input string when this character is equal to x
char :: Char -> Parser Char
char x = sat (== x)

-- string xs is the parser obtained by monadic composition
string :: String -> Parser String
string [] = return []
string (x:xs) = do char x
                   string xs
                   return (x:xs)

nat :: Parser Int
nat = do xs <- some digit
         return (read xs)

space :: Parser ()
space = do many (sat isSpace)
           return ()

int :: Parser Int
int = do char '-'
         n <- nat
         return (-n)
      <|> nat

-- Token wraps a parser with space-ignoring capabilities.
token :: Parser a -> Parser a
token p = do space
             v <- p
             space
             return v

natural :: Parser Int
natural = token nat

integer :: Parser Int
integer = token int

symbol :: String -> Parser String
symbol xs = token (string xs)

character :: Char -> Parser Char
character c = token (char c)

-- BNF grammar for our language:
--
-- expr       ::= term '+' expr | term
-- term       ::= factor '*' term | factor
-- factor     ::= (expr) | int

expr :: Parser Int
expr = do x <- term
          character '+'
          y <- expr
          return (x+y)
       <|> term

term :: Parser Int
term = do x <- factor
          character '*'
          y <- term
          return (x*y)
       <|> factor

factor :: Parser Int
factor = do character '('
            x <- expr
            character ')'
            return (x)
         <|> integer

-- testing the parser expr for expressions
-- using the parseExpr function

test :: String
test = "(1*2)+3+(4*5)"

test_with_space :: String
test_with_space = "(1 * 2 ) +  3 + ( 4 * 5 )"

test_without_parens :: String
test_without_parens = "1 * 2 +  3 + 4 * 5"

parseExpr :: String -> Int
parseExpr string = (fst . head) (parse expr string)

value1 = parseExpr test
value2 = parseExpr test_with_space

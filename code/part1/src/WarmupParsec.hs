module WarmupParsec where

-- Original grammar (E is start symbol):
--   E ::= E "+" T | E "-" T | T | "-" T .
--   T ::= num | "(" E ")" .
-- Lexical specifications:
--   num is one or more decimal digits (0-9)
--   tokens may be separated by arbtrary whitespace (spaces, tabs, newlines).

-- Rewritten grammar, without left-recursion:
--   E ::=  T EOpt | "-" T EOpt
--   EOpt ::= "+" T EOpt | "-" T EOpt | epsilon
--   T ::= num | "(" E ")"

import Text.ParserCombinators.Parsec -- exports a suitable type ParseError
import Data.Char
  -- may use instead of +++ for easier portability to Parsec

--type Parser a = Parsec a   -- may use synomym for easier portability to Parsec

--type ParseError = String  -- not particularly informative with ReadP

data Exp = Num Int | Negate Exp | Add Exp Exp
  deriving (Eq, Show)

symbol :: String -> Parser()
symbol s = do string s; return ()

pNum :: Parser Int
pNum = do ds <- many1 (satisfy isDigit) 
          return $ read ds 

pExp :: Parser Exp
pExp = do t <- pTerm; pEOpt t
       <|> do symbol "-"; t <- pTerm; pEOpt (Negate t)

pTerm :: Parser Exp
pTerm = do n <- pNum; return $ Num n
        <|> do symbol "("; e <- pExp; symbol ")"; return e


pEOpt :: Exp -> Parser Exp
pEOpt e = do symbol "+"; t <- pTerm; pEOpt (Add e t)
          <|>  do symbol "-"; t <- pTerm; pEOpt (Add e (Negate t))
          <|> return e

 
parseString :: String -> Either ParseError Exp
parseString s =
  case parse (do a <- pExp; eof; return a) "" s of
    Right val -> Right val
    Left err -> Left err
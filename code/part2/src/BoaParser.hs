-- Skeleton file for Boa Parser.

-- Rewritten grammar, without left-recursion:
--   E ::=  T EOpt | "-" T EOpt
--   EOpt ::= "+" T EOpt | "-" T EOpt | epsilon
--   T ::= num | "(" E ")"


module BoaParser (ParseError, parseString) where

import BoaAST
-- add any other other imports you need
import Text.ParserCombinators.Parsec
import Data.Char

-- Converts comments to strings of length 0
convComments :: Parser String
convComments =
   try (do _ <- try $ string "#"
           _ <- try $ manyTill anyChar newline
           return " ")
     <|> try (do _ <- try $ string "#"
                 _ <- try $ manyTill anyChar eof
                 return " ") 

-- Remove spaces 
removeSpaceComments :: Parser ()
removeSpaceComments =
  do spaces
     optional $ convComments >> removeSpaceComments

-- Removes whitespaces
whitespaceRemover :: Parser a -> Parser a
whitespaceRemover p =
  do a <- p
     removeSpaceComments
     return a

keywords :: [String]
keywords = ["None", "False", "True", "for", "if", "in", "not", "not in"]

isUnderscore :: Char -> Bool 
isUnderscore c = case c of 
                '_' -> True 
                _ -> False 

           
-- Gets VNames from the parser
pIdentVName :: Parser VName
pIdentVName = whitespaceRemover $ do name <- many1 (satisfy isLetter <|> satisfy isUnderscore <|> satisfy isDigit); 
                                     if name `elem` keywords then fail "Keyword" else return name

-- Gets FNames from the parser
pIdentFName :: Parser FName
pIdentFName = whitespaceRemover $ do name <- many1 (satisfy isLetter <|> satisfy isUnderscore <|> satisfy isDigit); 
                                     if name `elem` keywords then fail "Keyword" else return name

escape :: Parser String
escape = do _ <- char '\\'
            try (do c <- oneOf ['\'', '\\']; 
                    return [c]) 
             <|> try (do _ <- oneOf ['\n']; 
                         return [])
             <|> try (do _ <- oneOf ['n']; 
                         return ['\n'])
    

nonEscape :: Parser Char
nonEscape = satisfy (\x -> isPrint x && x /= '\'' && x /= '\\')

character :: Parser String
character = fmap return nonEscape <|> escape


pString :: Parser String
pString = do
    char '\''
    strings <- many character
    char '\''
    return $ concat strings


-- Gets NumConst from the parser
pNumConst :: Parser Exp
pNumConst = whitespaceRemover $ do num <- oneOf "0"; 
                                   return $ Const $ IntVal $ read [num]
                                <|> do _ <-  oneOf "0"; many1 (satisfy isDigit);
                                       fail "Leading zero"
                                <|> do num <-  many1 (satisfy isDigit); 
                                       return $ Const $ IntVal $ read num
                                <|> do neg <- oneOf "-"
                                       num <- many1 (satisfy isDigit); 
                                       return $ Const $ IntVal $ read (neg:num)

-- Our Exp from the grammar. The top-level after Program and Statements level
pExp :: Parser Exp
pExp = try (do _ <- whitespaceRemover $ try $ string "not#" 
               _ <- try $ manyTill anyChar newline
               e <- try $ whitespaceRemover pExp; 
               return (Not e))
       <|> try (do _ <- whitespaceRemover $ string "not ";  
                   e <- whitespaceRemover pExp; 
                   return (Not e))
       <|> try (do whitespaceRemover pOperRel)

-- Relational Operations are handled here
pOperRel :: Parser Exp
pOperRel = try (do a <- whitespaceRemover pOperAdd; 
                        whitespaceRemover $ string "=="; 
                        a1 <- whitespaceRemover pOperAdd; 
                        return (Oper Eq a a1)) 
                <|> try (do a <- whitespaceRemover pOperAdd; 
                            whitespaceRemover $ string "!="; 
                            a1 <- whitespaceRemover pOperAdd; 
                            return (Not (Oper Eq a a1)))
                <|> try (do a <- whitespaceRemover pOperAdd; 
                                 whitespaceRemover $ string ">"; 
                                 a1 <- whitespaceRemover pOperAdd; 
                                 return (Oper Greater a a1))
                <|> try (do a <- whitespaceRemover pOperAdd; 
                                 whitespaceRemover $ string "<="; 
                                 a1 <- whitespaceRemover pOperAdd; 
                                 return (Not (Oper Greater a a1)))
                <|> try (do a <- whitespaceRemover pOperAdd; 
                                 whitespaceRemover $ string "<"; 
                                 a1 <- whitespaceRemover pOperAdd; 
                                 return (Oper Less a a1))
                <|> try (do a <- whitespaceRemover pOperAdd;
                                 whitespaceRemover $ string ">="; 
                                 a1 <- whitespaceRemover pOperAdd; 
                                 return (Not (Oper Less a a1)))
                <|> try (do a <- whitespaceRemover pOperAdd; 
                                 whitespaceRemover $ string "in "; 
                                 a1 <- pOperAdd; 
                                 return (Oper In a a1))
                <|> try (do a <- whitespaceRemover pOperAdd; 
                                 whitespaceRemover $ string "not in"; 
                                 a1 <- pOperAdd; 
                                 return (Not (Oper In a a1)))
                <|> do whitespaceRemover pOperAdd

-- Additional operations are handled here
pOperAdd :: Parser Exp
pOperAdd = do m <- whitespaceRemover pOperMul; 
                   whitespaceRemover $ pOperAdd' m

-- Handles left-recursion in our grammar for additional operations 
pOperAdd' :: Exp -> Parser Exp
pOperAdd' m = do whitespaceRemover $ string "+"; 
                 m' <- whitespaceRemover pOperMul; 
                 pOperAdd' (Oper Plus m m')
              <|> do whitespaceRemover $ string "-"; 
                     m' <- whitespaceRemover pOperMul; 
                     pOperAdd' (Oper Minus m m')
              <|> do return m

-- Multiplication operations are handled here
pOperMul :: Parser Exp
pOperMul = do t <- whitespaceRemover pTerm; 
                   whitespaceRemover $ pOperMul' t

-- Handles left-recursion in our grammar for multiplication operations 
pOperMul' :: Exp -> Parser Exp
pOperMul' t = do whitespaceRemover $ string "*"; 
                 t' <- whitespaceRemover pTerm; 
                 pOperMul' (Oper Times t t')
              <|> do whitespaceRemover $ string "//"; 
                     t' <- whitespaceRemover pTerm; 
                     pOperMul' (Oper Div t t')
              <|> do whitespaceRemover $ string "%"; 
                     t' <- whitespaceRemover pTerm; 
                     pOperMul' (Oper Mod t t')
              <|> do return t

-- Boolean operations are handled here
pBool :: Parser Value
pBool = do whitespaceRemover $ string "None"; return NoneVal 
        <|> do whitespaceRemover $ string "False"; return FalseVal
        <|> do whitespaceRemover $ string "True"; return TrueVal 

-- Handles multiple expressions in a list
pExpz :: Parser [Exp]
pExpz = do whitespaceRemover $ sepBy pExp (whitespaceRemover $ string ",")


pClausez :: Parser [CClause]
pClausez = do many pClause

pClause :: Parser CClause
pClause = do whitespaceRemover $ string "for"; 
             vname <- pIdentVName; 
             whitespaceRemover $ string "in"; 
             e <- pExp; 
             return (CCFor vname e)
             whitespaceRemover $ string "if"; 
                 e <- pExp; return (CCIf e)

-- Terminals in our grammar. 
pTerm :: Parser Exp
pTerm = do whitespaceRemover pNumConst
        <|> do s <- pString; return (Const (StringVal s))
        <|> try (do bool <- whitespaceRemover pBool; 
                    return (Const bool))
        <|> do whitespaceRemover $ string "("; 
               e <- whitespaceRemover pExp; 
               whitespaceRemover $ string ")"; 
               return e
        <|> try (do whitespaceRemover $ string "["; 
                    e <- whitespaceRemover pExp; 
                    f <- whitespaceRemover pClause; 
                    c <- whitespaceRemover pClausez; 
                    whitespaceRemover $ string "]"; 
                    return (Compr e ([f]<>c)))
        <|> try (do fname <- whitespaceRemover pIdentFName; 
                    whitespaceRemover $ string "("; 
                    ez <- whitespaceRemover pExpz; 
                    whitespaceRemover $ string ")"; 
                    return $ Call fname ez)
                 <|> do vname <- whitespaceRemover pIdentVName; 
                        return $ Var vname
                 <|> do whitespaceRemover $ string "["; 
                        ez <- whitespaceRemover pExpz; 
                        whitespaceRemover $ string "]"; 
                        return (List ez)

-- Statements are handled here
pStmt :: Parser Stmt
pStmt = try (do vname <- whitespaceRemover pIdentVName; 
                whitespaceRemover $ string "="; 
                e <- whitespaceRemover pExp; return (SDef vname e))
             <|> do e <- whitespaceRemover pExp; return (SExp e)

-- Program from the grammar is handled here
pProgram :: Parser Program
pProgram = do whitespaceRemover $ sepBy pStmt (string "; ")

-- Runs our program untill the end of line and removes beginning whitespaces
ourProgram :: Parser Program
ourProgram =
  do removeSpaceComments
     p <- do pProgram
     eof
     return p

-- This function has been modified slightly to include our implementations
parseString :: String -> Either ParseError Program
parseString s =
  case parse ourProgram "" s of
    Right val -> Right val
    Left err -> Left err

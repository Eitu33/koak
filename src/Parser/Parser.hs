module Parser.Parser where

import           Data.Dynamic
import           Parser.Functor
import           Parser.FunctorsUtils
import           Parser.ParserDataTypes
import           Parser.Splitter
import           Parser.Utils
import           Prelude                hiding (pure, (*>), (<$>), (<*), (<*>), (<|>))

type Datums = ([String], Infos)

getBinop :: Parser String
getBinop datum
  | first == second && first `elem` ["=", "|", "&"] =
    Right (first ++ second, list)
  | first `elem` ["+", "-", "/", "*", "<", ">", "%", "="] = Right (first, next)
  | first == "!" && second == "=" = Right ("!=", list)
  | otherwise = Left "No Binary"
  where
    (first, next) = getNextString datum
    (second, list) = getNextString next

getBinary :: Parser Binary
getBinary a =
  ((Binary NoType (snd a) <$> getBinop <*> getUnary <*> hasBinop <*>
    (getBinary <|> pure NoBinary)) <|>
   pure NoBinary)
    a

hasBinop :: Parser Bool
hasBinop datum
  | first == second && first `elem` ["=", "|", "&"] ||
      first `elem` ["+", "-", "/", "*", "<", ">", "%", "="] ||
      first == "!" && second == "=" = Right (True, datum)
  | otherwise = Right (False, datum)
  where
    (first, next) = getNextString datum
    (second, list) = getNextString next

getExpressionsList :: String -> Parser [Expressions]
getExpressionsList s =
  getStartingPar *> manySeparated parseExpressions s <* getEndingPar
  where
    getStartingPar = string "(" "No starting parenthesis at expressions list"
    getEndingPar = string ")" "No ending parenthesis at expressions list"

hasCall :: Parser Bool
hasCall a =
  case string "(" "" a of
    Right _ -> Right (True, a)
    Left _  -> Right (False, a)

createPrimaryS :: Infos -> String -> Primary
createPrimaryS d a = Primary NoType d True a NoExpressions

createPrimaryE :: Infos -> Expressions -> Primary
createPrimaryE d = Primary NoType d False ""

getPrimary :: Parser Primary
getPrimary a =
  ((createPrimaryE (snd a) <$> findExp) <|>
   (createPrimaryS (snd a) <$> parseIdentifier))
    a
  where
    getStartingPar = string "(" "No starting parenthesis at primary"
    getEndingPar = string ")" "No ending parenthesis at primary"
    findExp = getStartingPar *> parseExpressions <* getEndingPar

getPostfix :: Parser Postfix
getPostfix a =
  (Postfix NoType (snd a) <$> getPrimary <*> hasCall <*>
   (getExpressionsList "," <|> pure []))
    a

getUnop :: Parser String
getUnop a
  | first == "!" && second == "" = Left "Unary '!' not followed by anything"
  | first == "!" && second == "=" = Left "Binary '!=' given instead of unary"
  | first `elem` ["!", "-"] = Right (first, next)
  | otherwise = Left "No Unary given"
  where
    (first, next) = getNextString a
    (second, _) = getNextString next

createUnaryUnop :: Datums -> String -> Unary -> Unary
createUnaryUnop dat un una = Unary NoType (snd dat) True un una NoPostfix

createUnaryNoUn :: Datums -> Postfix -> Unary
createUnaryNoUn dat = Unary NoType (snd dat) False "" NoUnary

getUnary :: Parser Unary
getUnary a =
  ((createUnaryUnop a <$> getUnop <*> getUnary) <|>
   (createUnaryNoUn a <$> getPostfix))
    a

getExprExpr :: Parser ExprExpr
getExprExpr a =
  (ExprExpr NoType (snd a) <$> getUnary <*> hasBinop <*>
   (getBinary <|> (\x -> Right (NoBinary, x))))
    a

getExprExprList :: String -> Parser [ExprExpr]
getExprExprList = someSeparated getExprExpr

isElse :: Parser Bool
isElse dat = Right (first == "else", dat)
  where
    (first, _) = getNextString dat

getIf :: Parser IfExpr
getIf a =
  (IfExpr NoType (snd a) <$> (remI *> getExprExpr) <*>
   (remT *> parseExpressions) <*>
   isElse <*>
   ((remE *> parseExpressions) <|> pure NoExpressions))
    a
  where
    remI = string "if" "This is not a if. How have you gotten here ?"
    remT = string "then" "The if has no else"
    remE = string "else" "no else"

getWhile :: Parser WhileExpr
getWhile a =
  (WhileExpr NoType (snd a) <$> (isWhile *> getExprExpr) <*>
   (remDo *> parseExpressions))
    a
  where
    isWhile = string "while" "This is not a while. How have you gotten here ?"
    remDo = string "do" "The while has no do"

getVariable :: Parser Variable
getVariable a =
  (Variable NoType (snd a) <$> (getIdentifier <* remE) <*> (getExprExpr <* remC))
    a
  where
    remE = string "=" "For is not well formatted"
    remC = string "," "For is not well formatted"

getFor :: Parser ForExpr
getFor dat =
  (ForExpr NoType (snd dat) <$> (isFor *> getVariable) <*> getIdentifier <*>
   (remS *> getExprExpr <* remC) <*>
   getExprExpr <*>
   (remE *> parseExpressions))
    dat
  where
    remS = string "<" "For is not well formatted"
    remC = string "," "For is not well formatted"
    remE = string "in" "For is not well formatted"
    isFor = string "for" "This is not a for. How have you gotten here ?"

getExprType :: Parser ExpressionType
getExprType dat
  | first == "" = Left "No Expressions given"
  | first == "while" = Right (While, dat)
  | first == "if" = Right (If, dat)
  | first == "for" = Right (For, dat)
  | otherwise = Right (Expr, dat)
  where
    (first, _) = getNextString dat

parseExpressions :: Parser Expressions
parseExpressions dat =
  (Expressions NoType (snd dat) <$> getExprType <*>
   (toDyn <$> getFor <|> (toDyn <$> getWhile) <|> (toDyn <$> getIf) <|>
    (toDyn <$> getExprExprList ":")))
    dat

parseType :: Parser Type
parseType dat
  | first == "" = Left "No type given"
  | otherwise = Right (Type first, next)
  where
    (first, next) = getNextString dat

nextIs :: String -> Parser String
nextIs s dat
  | first == s = Right (first, dat)
  | otherwise = Left "The arguments does not end with a ending parenthesis"
  where
    (first, _) = getNextString dat

getArgument :: Parser Argument
getArgument dat =
  (Argument (snd dat) <$> getIdentifier <*> (getP *> parseType)) dat
  where
    getP = string ":" "No separator between identifier and type"

parseArguments :: Parser [Argument]
parseArguments = getStartingPar *> many getArgument <* getEndingPar
  where
    getStartingPar = string "(" "No starting parenthesis"
    getEndingPar = string ")" "No ending parenthesis"

parseIdentifier :: Parser String
parseIdentifier = getIdentifier <|> getStringNumber

parsePrototype :: Parser Prototype
parsePrototype dat =
  (Prototype NoType (snd dat) <$> parseIdentifier <*> parseArguments <*>
   (getP *> parseType))
    dat
  where
    getP = string ":" "No separator between arguments and type"

parseDefinition :: Parser Definition
parseDefinition s =
  (Definition (snd s) NoType <$> parsePrototype <*> parseExpressions) s

statementByDef :: Datums -> Definition -> Statement
statementByDef (_, d) def = Statement d True def NoExpressions

statementByExp :: Datums -> Expressions -> Statement
statementByExp (_, d) = Statement d False NoDefinition

createStatement :: Parser Statement
createStatement infos =
  (((getIsDefinition *> (statementByDef infos <$> parseDefinition)) <* remove) <|>
   ((statementByExp infos <$> parseExpressions) <* remove))
    infos
  where
    remove = string ";" "the statement does not end with a ';'"
    getIsDefinition = string "def" ""

runParser :: Datums -> Either String Parsed
runParser str =
  case many createStatement str of
    Right (r, ([], _)) -> Right r
    Left msg           -> Left msg
    _                  -> Left "string was not fully consumed"

parser :: String -> String -> Either String Parsed
parser datum filename = runParser (split, getVoidInfos filename)
  where
    split = splitter datum

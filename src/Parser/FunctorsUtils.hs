module Parser.FunctorsUtils where

import           Parser.Functor
import           Parser.ParserDataTypes
import           Parser.Utils
import           Prelude                hiding (pure, (*>), (<$>), (<*), (<*>), (<|>))

someSeparated :: Parser a -> String -> Parser [a]
someSeparated p s = (:) <$> p <*> many (string s "" *> p)

manySeparated :: Parser a -> String -> Parser [a]
manySeparated p s = ((:) <$> p <*> many (string s "" *> p)) <|> pure []

many :: Parser a -> Parser [a]
many p = ((:) <$> p <*> many p) <|> pure []

some :: Parser a -> Parser [a]
some p = (:) <$> p <*> many p

checkString :: String -> (String -> Bool) -> Parser String
checkString msg f datum =
  if f top
    then Right (top, rest)
    else Left (msg ++ " - failed at \"" ++ top ++ "\"")
  where
    (top, rest) = getNextString datum

string :: String -> String -> Parser String
string s e = clear (checkString e (== s))

pure :: a -> Parser a
pure x s = Right (x, s)

check :: (String -> Bool) -> Parser String
check f s =
  case s of
    (x:xs, a)
      | f x -> Right (x, (xs, a))
    _ -> Left "check failed"

char :: String -> Parser String
char c = check (== c)

oneOf :: [String] -> Parser String
oneOf cs = check (`elem` cs)

clear :: Parser a -> Parser a
clear p = snip *> p <* snip
  where
    snip = many (checkString "" (== " "))

isVarName :: String -> Bool
isVarName (x:xs) =
  x `elem` ['A' .. 'Z'] ++ ['a' .. 'z'] &&
  all (\s -> s `elem` ['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['0' .. '1']) xs

getIdentifier :: Parser String
getIdentifier dat
  | first == "" = Left "No identifier given"
  | isVarName first = Right (first, rest)
  | otherwise = Left (show first ++ " is not a var name")
  where
    (first, rest) = getNextString dat

isNumber :: String -> Bool
isNumber str =
  all (\s -> s `elem` ('.' : ['0' .. '9'])) str &&
  ((length . filter (== '.')) str `elem` [0, 1])

getStringNumber :: Parser String
getStringNumber dat
  | first == "" = Left "No number given"
  | isNumber first = Right (first, rest)
  | otherwise = Left (show first ++ " is not a number")
  where
    (first, rest) = getNextString dat

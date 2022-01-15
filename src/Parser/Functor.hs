module Parser.Functor where

import           Parser.ParserDataTypes

type Parser a = ([String], Infos) -> Either String (a, ([String], Infos))

(<$>) :: (a -> b) -> Parser a -> Parser b
f <$> p =
  \str ->
    case p str of
      Right (v, str') -> Right (f v, str')
      Left msg        -> Left msg

(<*) :: Parser a -> Parser b -> Parser a
p1 <* p2 =
  \str ->
    case p1 str of
      Right (v, str') ->
        case p2 str' of
          Right (_, str'') -> Right (v, str'')
          Left msg         -> Left msg
      Left msg -> Left msg

(*>) :: Parser a -> Parser b -> Parser b
p1 *> p2 =
  \str ->
    case p1 str of
      Right (_, str') -> p2 str'
      Left msg        -> Left msg

(<*>) :: Parser (a -> b) -> Parser a -> Parser b
p1 <*> p2 =
  \str ->
    case p1 str of
      Right (f, str') ->
        case p2 str' of
          Right (v, str'') -> Right (f v, str'')
          Left msg         -> Left msg
      Left msg -> Left msg

(<|>) :: Parser a -> Parser a -> Parser a
p1 <|> p2 =
  \str ->
    case p1 str of
      Right _ -> p1 str
      Left a  -> p2 str

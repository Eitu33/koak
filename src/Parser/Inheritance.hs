module Parser.Inheritance where

import           Data.Dynamic
import           Parser.ParserDataTypes

class GetType a where
  getType :: a -> Type

instance GetType Statement where
  getType (Statement _ True def _) = getType def
  getType (Statement _ False _ ex) = getType ex

instance GetType Definition where
  getType NoDefinition = NoType
  getType d            = typeDefinition d

instance GetType Primary where
  getType = typePrimary

instance GetType Postfix where
  getType = typePostfix

instance GetType Unary where
  getType = typeUnary

instance GetType Binary where
  getType = binaryType

instance GetType ExprExpr where
  getType = exprExprType

instance GetType Variable where
  getType = typeVariable

instance GetType WhileExpr where
  getType = typeWhile

instance GetType IfExpr where
  getType = typeIf

instance GetType ForExpr where
  getType = typeFor

instance GetType Expressions where
  getType NoExpressions = NoType
  getType e             = typeExpression e

instance GetType Argument where
  getType = argumentType

instance GetType Prototype where
  getType = prototypeType

class Position a where
  getPosition :: a -> Infos

instance Position Primary where
  getPosition = positionPrimary

instance Position Postfix where
  getPosition = positionPostfix

instance Position Unary where
  getPosition = positionUnary

instance Position Binary where
  getPosition = positionBinary

instance Position ExprExpr where
  getPosition = positionExpr

instance Position Variable where
  getPosition = positionVariable

instance Position WhileExpr where
  getPosition = positionWhile

instance Position IfExpr where
  getPosition = positionIf

instance Position ForExpr where
  getPosition = positionFor

instance Position Expressions where
  getPosition = positionExpressions

instance Position Argument where
  getPosition = positionArgument

instance Position Prototype where
  getPosition = positionPrototype

instance Position Definition where
  getPosition = positionDefinition

instance Position Statement where
  getPosition = positionStatement

showBool :: Bool -> String
showBool True  = "true"
showBool False = "false"

instance Show Primary where
  show (Primary t v c b d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" :" ++
    show v ++
    ", \"isIdentifier\": " ++
    showBool c ++
    ", \"identifierPrimary\": " ++
    show b ++ ", \"expressionPrimary\": " ++ show d ++ "}"

instance Show Postfix where
  show NoPostfix = "null"
  show (Postfix t v c b d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"primary\": " ++
    show c ++
    ", \"hasCallExpr\": " ++ showBool b ++ ", \"callExprs\": " ++ show d ++ "}"

instance Show Unary where
  show NoUnary = "null"
  show (Unary t v c b a d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"unop\": " ++
    showBool c ++
    ", \"unopChar\": " ++
    show b ++ ", \"unaryRec\": " ++ show a ++ ", \"postfix\": " ++ show d ++ "}"

instance Show Binary where
  show NoBinary = "null"
  show (Binary t v c b a d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"binop\" :" ++
    show c ++
    ", \"unaryB\": " ++
    show b ++
    ", \"isRec\": " ++ showBool a ++ ", \"recursive\": " ++ show d ++ "}"

instance Show ExprExpr where
  show NoExprExpr = "null"
  show (ExprExpr t v c b d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"unary\": " ++
    show c ++
    ", \"hasBinary\": " ++ showBool b ++ ", \"binary\": " ++ show d ++ "}"

instance Show Variable where
  show (Variable t v c d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++ ", \"name\": " ++ show c ++ ", \"value\": " ++ show d ++ "}"

instance Show WhileExpr where
  show NoWhileExpr = "null"
  show (WhileExpr t v c d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++ ", \"condition\": " ++ show c ++ ", \"exprW\": " ++ show d ++ "}"

instance Show IfExpr where
  show NoIfExpr = "null"
  show (IfExpr t v c b a d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"ifE\": " ++
    show c ++
    ", \"elseE\": " ++
    show b ++
    ", \"thenExist\": " ++ showBool a ++ ", \"thenE\": " ++ show d ++ "}"

instance Show ForExpr where
  show NoForExpr = "null"
  show (ForExpr t v c e b a d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show v ++
    ", \"variable\": " ++
    show c ++
    ", \"identifierFor\": " ++
    show e ++
    ", \"comparator\": " ++
    show b ++ ", \"beforeIn\": " ++ show a ++ "\"afterIn\": " ++ show d ++ "}"

instance Show Expressions where
  show NoExpressions = "null"
  show (Expressions t d For e) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show d ++
    ", \"expressionType\": \"For\", \"exprE\":" ++
    show (fromDyn e NoForExpr) ++ "}"
  show (Expressions t d While e) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show d ++
    ", \"expressionType\": \"While\", \"exprE\": " ++
    show (fromDyn e NoWhileExpr) ++ "}"
  show (Expressions t d If e) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show d ++
    ", \"expressionType\": \"If\", \"exprE\": " ++
    show (fromDyn e NoIfExpr) ++ "}"
  show (Expressions t d Expr e) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show d ++
    ", \"expressionType\": \"Expr\", \"exprE\": " ++
    show (fromDyn e [] :: [ExprExpr]) ++ "}"

instance Show Argument where
  show (Argument p e d) =
    "{\"position\": " ++
    show p ++
    ", \"identifierArgument\" : " ++ show e ++ ", \"type\": " ++ show d ++ "}"

instance Show Prototype where
  show (Prototype t i a r d) =
    "{\"type\": " ++
    show t ++
    ", \"position\" : " ++
    show i ++
    ", \"identifierPrototype\": " ++
    show a ++ ", \"args\": " ++ show r ++ ", \"returnType\": " ++ show d ++ "}"

instance Show Definition where
  show NoDefinition = "null"
  show (Definition a p e d) =
    "{\"position\": " ++
    show a ++
    ",\"type\":" ++
    show p ++
    ", \"prototype\": " ++
    show e ++ ", \"expressionDefinition\": " ++ show d ++ "}"

instance Show Statement where
  show (Statement isD def ex du) =
    "{ \"position\" : " ++
    show isD ++
    ", \"isDefinition\": " ++
    showBool def ++
    ", \"definition\": " ++
    show ex ++ ", \"expressionStatement\": " ++ show du ++ "}"

instance Show Infos where
  show (Infos fn l c) =
    "{\"filename\": " ++
    show fn ++ ", \"line\": " ++ show l ++ ", \"column\": " ++ show c ++ "}"

instance Show Variables where
  show (Variables name_ type_) =
    "{\"name\": " ++ show name_ ++ ", \"type\": " ++ show type_ ++ "}"

instance Show Type where
  show NoType = "null"
  show (Type n) = "{\"name\":" ++ show n ++ "}"
  show (CallingType a r n) =
    "{\"arguments\":" ++
    show a ++
    ", \"returnCall\":" ++ show r ++ ", \"nameCalling\":" ++ show n ++ " }"

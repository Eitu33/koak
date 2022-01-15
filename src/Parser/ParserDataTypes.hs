module Parser.ParserDataTypes where

import           Data.Dynamic

data ExpressionType
  = For
  | If
  | While
  | Expr
  deriving (Enum, Eq, Show)

data Variables =
  Variables
    { nameFunction  :: String
    , variablesType :: Type
    }
  deriving (Eq)

data Type
  = NoType
  | Type
      { nameType :: String
      }
  | CallingType
      { arguments   :: [Variables]
      , returnCall  :: Type
      , nameCalling :: String
      }
  deriving (Eq)

class TypeName a where
  typeName :: a -> String

instance TypeName Type where
  typeName (Type a)            = a
  typeName (CallingType a b c) = c

data Primary =
  Primary
    { typePrimary        :: Type
    , positionPrimary    :: Infos
    , isIdentifier       :: Bool
    , identifierPrimary  :: String
    , expressionsPrimary :: Expressions
    }
  deriving (Eq)

data Postfix
  = NoPostfix
  | Postfix
      { typePostfix     :: Type
      , positionPostfix :: Infos
      , primary         :: Primary
      , hasCallExpr     :: Bool
      , callExpr        :: [Expressions]
      }
  deriving (Eq)

data Unary
  = NoUnary
  | Unary
      { typeUnary     :: Type
      , positionUnary :: Infos
      , unop          :: Bool
      , unopChar      :: String
      , unaryRec      :: Unary
      , postfix       :: Postfix
      }
  deriving (Eq)

data Binary
  = NoBinary
  | Binary
      { binaryType     :: Type
      , positionBinary :: Infos
      , binop          :: String
      , unaryB         :: Unary -- Expression recursive is a binary
      , isRecursive    :: Bool
      , recursive      :: Binary
      }
  deriving (Eq)

data ExprExpr
  = NoExprExpr
  | ExprExpr
      { exprExprType :: Type
      , positionExpr :: Infos
      , unary        :: Unary
      , hasBinary    :: Bool
      , binary       :: Binary
      }
  deriving (Eq)

data Variable =
  Variable
    { typeVariable     :: Type
    , positionVariable :: Infos
    , variableName     :: String
    , variableValue    :: ExprExpr
    }
  deriving (Eq)

data WhileExpr
  = NoWhileExpr
  | WhileExpr
      { typeWhile     :: Type
      , positionWhile :: Infos
      , condition     :: ExprExpr
      , exprW         :: Expressions
      }
  deriving (Eq)

data IfExpr
  = NoIfExpr
  | IfExpr
      { typeIf     :: Type
      , positionIf :: Infos
      , ifE        :: ExprExpr
      , elseE      :: Expressions
      , thenExist  :: Bool
      , thenE      :: Expressions
      }
  deriving (Eq)

data ForExpr
  = NoForExpr
  | ForExpr
      { typeFor       :: Type
      , positionFor   :: Infos
      , variable      :: Variable
      , identifierFor :: String
      , comparator    :: ExprExpr
      , beforeIn      :: ExprExpr
      , afterIn       :: Expressions
      }
  deriving (Eq)

data Expressions
  = NoExpressions
  | Expressions
      { typeExpression      :: Type
      , positionExpressions :: Infos
      , expressionType      :: ExpressionType
      , exprE               :: Dynamic
  {-
  For -> ForExpr
  If -> IfExpr
  While -> WhileExpr
  Expr -> [ExprExpr]
  -}
      }

data Argument =
  Argument
    { positionArgument   :: Infos
    , identifierArgument :: String
    , argumentType       :: Type
    }
  deriving (Eq)

data Prototype =
  Prototype
    { prototypeType       :: Type
    , positionPrototype   :: Infos
    , identifierPrototype :: String -- Unary Binary + Number ?
    , args                :: [Argument]
    , returnType          :: Type
    }
  deriving (Eq)

data Definition
  = NoDefinition
  | Definition
      { positionDefinition   :: Infos
      , typeDefinition       :: Type
      , prototype            :: Prototype
      , expressionDefinition :: Expressions
      }
  deriving (Eq)

data Statement =
  Statement
    { positionStatement   :: Infos
    , isDefinition        :: Bool
    , definition          :: Definition
    , expressionStatement :: Expressions
    }
  deriving (Eq)

type Parsed = [Statement]

data Infos =
  Infos
    { filename :: String
    , line     :: Integer
    , column   :: Integer
    }
  deriving (Eq)

instance Eq Expressions where
  NoExpressions == NoExpressions = True
  (Expressions t1 dur1 type1 dyn1) == (Expressions t2 dur2 type2 dyn2)
    | type1 /= type2 || dur1 /= dur2 || t1 /= t2 = False
    | type1 == For = fromDyn dyn1 NoForExpr == fromDyn dyn2 NoForExpr
    | type1 == If = fromDyn dyn1 NoIfExpr == fromDyn dyn2 NoIfExpr
    | type1 == While = fromDyn dyn1 NoWhileExpr == fromDyn dyn2 NoWhileExpr
    | type1 == Expr =
      (fromDyn dyn1 [] :: [ExprExpr]) == (fromDyn dyn2 [] :: [ExprExpr])

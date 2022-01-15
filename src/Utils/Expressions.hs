module Utils.Expressions where

import Parser.ParserDataTypes
import Parser.Inheritance
import Data.Dynamic

forExpr :: Expressions -> ForExpr
forExpr (Expressions _ _ For dyn) = fromDyn dyn NoForExpr
forExpr _ = NoForExpr

whileExpr :: Expressions -> WhileExpr
whileExpr (Expressions _ _ While dyn) = fromDyn dyn NoWhileExpr
whileExpr _ = NoWhileExpr

ifExpr :: Expressions -> IfExpr
ifExpr (Expressions _ _ If dyn) = fromDyn dyn NoIfExpr
ifExpr _ = NoIfExpr

exprsExpr :: Expressions -> [ExprExpr]
exprsExpr (Expressions _ _ Expr dyn) = fromDyn dyn []
exprsExpr _ = []

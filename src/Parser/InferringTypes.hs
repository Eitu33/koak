{-# LANGUAGE ParallelListComp #-}

module Parser.InferringTypes where

import           Data.Dynamic           (Dynamic, fromDyn, toDyn)
import           Debug.Trace            (trace)
import           Parser.FunctorsUtils   (isVarName)
import           Parser.Inheritance
import           Parser.ParserDataTypes
import           Parser.Utils

isDefType :: Statement -> Type -- Implement if struct
isDefType s = NoType

removeDouble :: [Type] -> [Type]
removeDouble =
  foldl (\seen x -> ifCase (x `elem` seen || x == NoType) seen (seen ++ [x])) []

getTypes :: Parsed -> [Type]
getTypes l =
  removeDouble $ [Type "int", Type "double", Type "void"] ++ map isDefType l

checkType :: [Type] -> Type -> (Type, Bool)
checkType a b
  | b `elem` a = (b, False)
  | otherwise = trace (show (typeName b) ++ " type not in scope") (b, True)

getFun :: [Type] -> Statement -> ([Variables], Bool)
getFun _ (Statement _ _ NoDefinition _) = ([], False)
getFun ts (Statement info _ def _) =
  ([Variables funName (CallingType arguments returnT "def")], e1 || e2)
  where
    funName = (identifierPrototype . prototype) def
    (arguments, e1) =
      foldl
        tmp
        ([], False)
        (map (Variables "" . getType) (args $ prototype def))
    (returnT, e2) = (checkType ts . returnType . prototype) def
    tmp :: ([Variables], Bool) -> Variables -> ([Variables], Bool)
    tmp (done, e3) new =
      (done ++ [new], ((snd . checkType ts) (variablesType new)) || e3)

getFunctions :: [Type] -> Parsed -> ([Variables], Bool)
getFunctions ts = foldl tmp ([], False)
  where
    tmp :: ([Variables], Bool) -> Statement -> ([Variables], Bool)
    tmp (done, err) new = (done ++ end, er || err)
      where
        (end, er) = getFun ts new

refactoExprsList ::
     [Type]
  -> [Variables]
  -> ([Expressions], Bool)
  -> Expressions
  -> ([Expressions], Bool)
refactoExprsList ts vars (done, err) new = (done ++ [end], er || err)
  where
    (end, _, er) = refactoExpr ts vars new

makeTypeOfDef :: Bool -> Primary -> [Expressions] -> Type
makeTypeOfDef hCE pri cE =
  ifCase
    hCE
    (CallingType (map (Variables "" . getType) cE) (getType pri) "def")
    (getType pri)

refactoPostfixName ::
     [Type] -> [Variables] -> Postfix -> (Postfix, [Variables], Bool)
refactoPostfixName ts vars (Postfix _ infos pri hCE cE) = (newPos, vars, e1)
  where
    newPos = Postfix type_ infos pri hCE newCE
    (newCE, e1) = (foldl (refactoExprsList ts vars) ([], False)) cE
    type_ = makeTypeOfDef hCE pri newCE

refactoUnaryName ::
     [Type] -> [Variables] -> Unary -> (Unary, String, [Variables], Bool)
refactoUnaryName ts vars (Unary _ infos u uC uR p) = (newUn, name_, vars, e1)
  where
    newUn = Unary NoType infos u uC uR newP -- U should be false else -> DON'T call this function
    (newP, _, e1) = refactoPostfixName ts vars p
    name_ = (identifierPrimary . primary) newP

refactoPrimary ::
     [Type] -> [Variables] -> Primary -> (Primary, [Variables], Bool)
refactoPrimary ts vars (Primary _ infos isID iP eP) = (newPri, vars, e1 || e2)
  where
    newPri = Primary type_ infos isID iP newEP
    (newEp, _, e1) = refactoExpr ts vars eP
    type_ =
      if isID
        then getTypeOfId
        else getType newEP
    getTypeOfId
      | isVarName iP = typeOfVar
      | all (\s -> s `elem` ['0' .. '9']) iP = Type "int"
      | otherwise = Type "double"
    typeOfVar =
      case filter (\(Variables n t) -> n == iP) vars of
        [] -> trace ("Unknown variable: " ++ iP) NoType
        a  -> variablesType $ head a
    (newEP, _, e2) = refactoExpr ts vars eP

refactoPostfix ::
     [Type] -> [Variables] -> Postfix -> (Postfix, [Variables], Bool)
refactoPostfix ts vars (Postfix _ infos pri hCE cE) =
  (newPos, vars, e1 || e2 || e3)
  where
    newPos = Postfix type_ infos newPri hCE newCE
    (newPri, _, e1) = refactoPrimary ts vars pri
    (newCE, e2) = (foldl (refactoExprsList ts vars) ([], False)) cE
    type_ = getType newPri
    e3 =
      (isCallingType $ getType newPri) &&
      foldl
        (||)
        False
        [ snd $ matchType (variablesType x) (getType y)
        | x <- (arguments $ getType newPri)
        | y <- newCE
        ]

refactoUnary :: [Type] -> [Variables] -> Unary -> (Unary, [Variables], Bool)
refactoUnary ts vars NoUnary = (NoUnary, vars, False)
refactoUnary ts vars (Unary _ infos u uC uR pos) = (newUn, vars, e1 || e2 || e3)
  where
    newUn = Unary type_ infos u uC newUR newPos
    (newUR, _, e1) = refactoUnary ts vars uR
    (newPos, _, e2) = refactoPostfix ts vars pos
    (type_, e3) =
      matchType
        (ifCase (newUR == NoUnary) NoType (typeUnary newUR))
        (ifCase (newPos == NoPostfix) NoType ((typePrimary . primary) newPos))

matchType :: Type -> Type -> (Type, Bool)
matchType a NoType = (a, False)
matchType NoType b = (b, False)
matchType a b
  | a == b = (a, False)
  | a `elem` [Type "int", Type "double"] && b `elem` [Type "int", Type "double"] =
    trace "Int and Double concurring -> Double winning" (Type "double", False)
  | isCallingType a = matchType (returnCall a) b
  | isCallingType b = matchType a (returnCall b)
matchType a b =
  trace
    (show a ++ " concurring with " ++ show b ++ "Fatal error")
    (NoType, True)

refactoBinary ::
     [Type] -> [Variables] -> Binary -> (Binary, Type, [Variables], Bool)
refactoBinary _ vars NoBinary = (NoBinary, NoType, vars, False)
refactoBinary ts vars (Binary _ infos b un iR r) =
  (bin, type3, vars, e1 || e2 || e3)
  where
    bin = Binary type3 infos b newUn iR newR
    (newUn, _, e1) = refactoUnary ts vars un
    (newR, type2, _, e2) = refactoBinary ts vars r
    (type3, e3) = matchType (typeUnary newUn) type2

refactoExprExpr ::
     [Type] -> [Variables] -> ExprExpr -> (ExprExpr, [Variables], Bool)
refactoExprExpr _ vars NoExprExpr = (NoExprExpr, vars, False)
refactoExprExpr ts vars (ExprExpr _ infos un True b)
  | binop b == "=" &&
      not (unop un) &&
      not ((hasCallExpr . postfix) un) &&
      (isIdentifier . primary . postfix) un &&
      (isVarName . identifierPrimary . primary . postfix) un =
    ( exprExpr
    , vars ++
      [Variables ((identifierPrimary . primary . postfix) newUn) (getType newB)]
    , e1 || e2)
  where
    exprExpr = ExprExpr type_ infos newUn True newB
    (newUn, name, _, e1) = refactoUnaryName ts vars un
    (newB, type_, _, e2) = refactoBinary ts vars b
refactoExprExpr ts vars (ExprExpr _ infos un hB b) =
  (newExpr, vars, e1 || e2 || e3)
  where
    newExpr = ExprExpr newType infos newUn hB newB
    (newUn, _, e1) = refactoUnary ts vars un
    (newB, type_, _, e2) = refactoBinary ts vars b
    (newType, e3) = matchType type_ (getType newUn)

refactoExprs ::
     [Type] -> [Variables] -> Dynamic -> (Dynamic, Type, [Variables], Bool)
refactoExprs ts vars dyn = (toDyn d, newType, newVars, e)
  where
    tmp ::
         ([ExprExpr], [Variables], Bool)
      -> ExprExpr
      -> ([ExprExpr], [Variables], Bool)
    tmp (done, vars, err) new = (done ++ [end], newVars, er || err)
      where
        (end, newVars, er) = refactoExprExpr ts vars new
    (d, newVars, e) = foldl tmp ([], vars, False) (fromDyn dyn [] :: [ExprExpr])
    newType =
      case d of
        [] -> NoType
        a  -> getType $ last a

refactoWhile ::
     [Type] -> [Variables] -> Dynamic -> (Dynamic, Type, [Variables], Bool)
refactoWhile ts vars dyn = (toDyn whileE, newType, vars, e1 || e2)
  where
    whileE = WhileExpr newType infos newCond newExpr
    (WhileExpr _ infos cond expr) = fromDyn dyn NoWhileExpr
    (newCond, _, e1) = refactoExprExpr ts vars cond
    (newExpr, _, e2) = refactoExpr ts vars expr
    newType = getType newExpr

refactoVar :: [Type] -> [Variables] -> Variable -> (Variable, [Variables], Bool)
refactoVar ts vars (Variable _ infos name val) =
  (newVar, vars ++ [Variables name type_], e1)
  where
    newVar = Variable type_ infos name newVal
    (newVal, _, e1) = refactoExprExpr ts vars val
    type_ = exprExprType newVal

refactoFor ::
     [Type] -> [Variables] -> Dynamic -> (Dynamic, Type, [Variables], Bool)
refactoFor ts vars dyn = (toDyn forE, newType, vars, e1 || e2 || e3 || e4)
  where
    forE = ForExpr newType infos newVar idFor newComp newBIn newAIn
    (ForExpr _ infos var idFor comp bIn aIn) = fromDyn dyn NoForExpr
    (newVar, newVars, e1) = refactoVar ts vars var
    (newComp, _, e2) = (comp, vars, False)
    (newBIn, _, e3) = (bIn, vars, False)
    (newAIn, _, e4) = (aIn, vars, False)
    newType = getType newAIn

ifCase :: Bool -> a -> a -> a
ifCase True t _  = t
ifCase False _ e = e

refactoIf ::
     [Type] -> [Variables] -> Dynamic -> (Dynamic, Type, [Variables], Bool)
refactoIf ts vars dyn = (toDyn ifE, newType, vars, e1 || e2 || e3 || e4 || e5)
  where
    ifE = IfExpr newType infos newExprExpr expr1r bool expr2r
    (IfExpr _ infos exprExpr expr1 bool expr2) = fromDyn dyn NoIfExpr
    (newExprExpr, _, e1) = refactoExprExpr ts vars exprExpr
    (expr1r, _, e2) = refactoExpr ts vars expr1
    (expr2r, _, e3) = refactoExpr ts vars expr2
    (newType, e4) = matchType (getType expr1r) (getType expr2r)
    e5 = getType expr1r == NoType

refactoExpr ::
     [Type] -> [Variables] -> Expressions -> (Expressions, [Variables], Bool)
refactoExpr ts vars NoExpressions = (NoExpressions, vars, False)
refactoExpr ts vars (Expressions _ infos t d) = (newExpr, vars2, e)
  where
    newExpr = Expressions newType infos t newChild
    (newChild, newType, vars2, e) =
      case t of
        If    -> refactoIf ts vars d
        For   -> refactoFor ts vars d
        While -> refactoWhile ts vars d
        Expr  -> refactoExprs ts vars d

refactoPrototype ::
     [Type] -> [Variables] -> Prototype -> (Prototype, [Variables], Bool)
refactoPrototype ts vars (Prototype _ inf iP aS rT) =
  (Prototype t inf iP aS rT, vars, False)
  where
    t = CallingType (map (\(Argument _ n t) -> Variables n t) aS) rT "def"

refactoDefinition ::
     [Type] -> [Variables] -> Definition -> (Definition, [Variables], Bool)
refactoDefinition ts vars (Definition inf _ pro eD) =
  (Definition inf t newPro newED, newVars, e1 || e2)
  where
    (newPro, newVars, e1) = refactoPrototype ts vars pro
    (newED, _, e2) = refactoExpr ts (newVars ++ arguments t) eD
    t = getType newPro

checkStatement ::
     [Type] -> [Variables] -> Statement -> (Statement, [Variables], Bool)
checkStatement ts vars s@(Statement infos True def _) = (s2, newVars, e1)
  where
    s2 = Statement infos True newDef NoExpressions
    (newDef, newVars, e1) = refactoDefinition ts vars def
checkStatement ts vars s@(Statement infos False _ expr) = (s2, vars2, e)
  where
    s2 = Statement infos False NoDefinition newExpr
    (newExpr, vars2, e) = refactoExpr ts vars expr

refacto :: [Type] -> [Variables] -> Parsed -> (Parsed, [Variables], Bool)
refacto ts fs = foldl tmp ([], fs, False)
  where
    tmp ::
         ([Statement], [Variables], Bool)
      -> Statement
      -> ([Statement], [Variables], Bool)
    tmp (done, vars, er) new = (done ++ [las], vars2, err || er)
      where
        (las, vars2, err) = checkStatement ts vars new

inferringTypes ::
     Either String Parsed -> Either String ([Variables], [Type], Parsed)
inferringTypes (Left a) = Left a
inferringTypes (Right list)
  | res = Left "Use of undefined type -> fatal error"
  | res2 = Left "Inconsistent types -> fatal error"
  | otherwise = Right (functions, types, final)
  where
    (functions, res) = getFunctions types list
    types = getTypes list
    (final, _, res2) = refacto types functions list

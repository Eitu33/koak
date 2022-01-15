{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings        #-}
{-# LANGUAGE RecursiveDo              #-}

module IR.Builder where

import           Compiler.Run
import           Debug.Trace
import           Parser.Inheritance
import           Parser.Parser
import           Parser.ParserDataTypes          hiding (Type)
import qualified Parser.ParserDataTypes          as PDT
import           Utils.Expressions
import           Utils.FindType

import           LLVM.AST                        hiding (function)
import qualified LLVM.AST                        as AST
import           LLVM.AST.AddrSpace
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import           LLVM.AST.ParameterAttribute
import qualified LLVM.AST.Type                   as T

import qualified LLVM.IRBuilder.Constant         as IC
import           LLVM.IRBuilder.Instruction
import           LLVM.IRBuilder.Module
import           LLVM.IRBuilder.Monad

import           Control.Monad.Fix
import qualified Data.ByteString.Char8           as BS
import           Data.ByteString.Short           hiding (length, null)
import           Data.Char
import           Data.Dynamic
import           Data.List                       (elemIndex)

-- types
externf :: Type -> Name -> Operand
externf ty nm = ConstantOperand (C.GlobalReference ty nm)

mptr :: Type -> Type
mptr ref = PointerType {pointerReferent = ref, pointerAddrSpace = AddrSpace 0}

fn :: Type -> [Type] -> Type
fn res arg =
  FunctionType {resultType = res, argumentTypes = arg, isVarArg = False}

builder :: Parsed -> AST.Module
builder p = buildModule "main module" $ funcGen p

-- codegen
funcGen ::
     (MonadModuleBuilder m, Control.Monad.Fix.MonadFix m) => Parsed -> m Operand
funcGen p
  | (isDefinition $ head p) =
    mdo function
          fident
          (zip aty (map (ParameterName . toShort . BS.pack) aident))
          retty $ \a -> do
          r <- bodyGen expr a aident
          ret r
        funcGen $ tail p
  | otherwise =
    function "main" [] (findType $ expr) $ \[] -> do
      r <- bodyGen expr [] []
      ret r
  where
    proto = prototype . definition $ head p
    fident = mkName $ identifierPrototype proto
    aty = map findType adef
    aident = map identifierArgument adef
    adef = PDT.args proto
    retty = findType proto
    expr =
      case isDefinition $ head p of
        True  -> expressionDefinition . definition $ head p
        False -> expressionStatement $ head p

pident :: [ExprExpr] -> String
pident ee = identifierPrimary . primary . postfix . unary $ head ee

bpident :: [ExprExpr] -> String
bpident ee = identifierPrimary . primary . postfix . unaryB . binary $ head ee

controlGen ::
     (LLVM.IRBuilder.Monad.MonadIRBuilder m, Control.Monad.Fix.MonadFix m)
  => IfExpr
  -> [Operand]
  -> [String]
  -> m Operand
controlGen ie vs nm =
  mdo _entry <- block `named` "entry"
      cond <- bodyGen ifcond vs nm
      condBr cond ifThen ifElse
      ifThen <- block
      trVal <- bodyGen thenExpr vs nm
      br ifExit
      ifElse <- block `named` "if.else"
      flVal <- bodyGen elseExpr vs nm
      br ifExit
      ifExit <- block `named` "if.exit"
      phi [(trVal, ifThen), (flVal, ifElse)]
  where
    ifcond = wrapExpr $ ifE ie
    thenExpr = elseE ie
    elseExpr = thenE ie

wrapExpr :: ExprExpr -> Expressions
wrapExpr ee =
  Expressions
    { typeExpression = getType ee
    , positionExpressions = getPosition ee
    , expressionType = Expr
    , exprE = toDyn [ee]
    }

-- could recurse here
bodyGen ::
     (MonadIRBuilder m, Control.Monad.Fix.MonadFix m)
  => Expressions
  -> [Operand]
  -> [String]
  -> m Operand
bodyGen expr vs nm
  | expressionType expr == If = controlGen (ifExpr expr) vs nm
  | hasCallExpr fpost = do
    a <-
      call
        (externf
           (mptr $ fn (findType $ fpost) (map findType (callExpr fpost)))
           fname)
        (map (\x -> (x, [])) fcexpr)
    case hasBinary $ head body of
      True  -> cvOps (head body) a s
      False -> fadd a (IC.double 0)
  | hasBinary $ head body = cvOps (head body) f s
  where
    body = exprsExpr expr
    f = c $ pident body
    s = c $ bpident body
    fname = mkName . identifierPrimary $ primary fpost
    fcexpr = map (c . pident . exprsExpr) (callExpr fpost)
    fpost = postfix . unary . head $ exprsExpr expr
    c v = cvIdent expr vs nm v

cvOps :: MonadIRBuilder m => ExprExpr -> Operand -> Operand -> m Operand
cvOps ee = res op
  where
    b = binop $ binary ee
    m =
      [ ("+", [fadd, add])
      , ("-", [fsub, sub])
      , ("*", [fmul, mul])
      , ("/", [fdiv, sdiv])
      , ("==", [fcmp FP.OEQ, icmp IP.EQ])
      ]
    ops = filter comp m
    op
      | null ops = head m
      | otherwise = head ops
    res (_, o)
      | getType ee == PDT.Type "double" = head o
      | getType ee == PDT.Type "int" = last o
      | otherwise = head o
    comp (a, c) = a == b

cvIdent :: Expressions -> [Operand] -> [String] -> String -> Operand
cvIdent expr vs nm a =
  case elemIndex a nm of
    Just r  -> vs !! r
    Nothing -> cdigit
  where
    cdigit
      | getType expr == PDT.Type "double" = IC.double (read a :: Double)
      | getType expr == PDT.Type "int" = IC.int32 (read a :: Integer)
      | otherwise = IC.double 0.0

module Parser.ParserSpec (parserTests) where

import Parser.Utils
import qualified Parser.Parser as SUT
import Parser.ParserDataTypes
import Data.Dynamic (fromDyn, toDyn)

cIT :: Integer -> Infos
cIT = Infos "toto.koak" 0

cIT1 :: Integer -> Infos
cIT1 = Infos "toto.koak" 1

primaryTests :: IO()
primaryTests =
   putStrLn ("Primary: " ++ show (primary == Primary NoType (cIT 0) True "add" NoExpressions)) >>
   putStrLn ("Primary: rest: " ++ show (["(", ")"] == rest))
  where Right (primary, (rest, _)) = SUT.getPrimary (["add", "(", ")"], getVoidInfos "toto.koak")

callExprTests :: IO()
callExprTests =
    putStrLn ("CallExpr: exist: " ++ show bool) >>
    putStrLn ("CallExpr: rest: " ++ show (["(", ")", ")"] == rest))
  where Right (bool, (rest, _)) = SUT.hasCall (["(",")", ")"], getVoidInfos "toto.koak")

postfixTests :: IO ()
postfixTests =
    putStrLn ("Postfix: has callExpr: " ++ show (postfix == Postfix NoType (cIT 0) (
      Primary NoType (cIT 0) True "add" NoExpressions) True [])) >>
    putStrLn ("Postfix: rest: " ++ show (rest == [","]))
  where Right (postfix, (rest, _)) = SUT.getPostfix (["add", "(", ")", ","], getVoidInfos "toto.koak")

unaryTests :: IO()
unaryTests =
    putStrLn ("Unary: rest: " ++ show (rest == ["tata"])) >>
    putStrLn ("Unary: is unop: "++ show (unary == Unary NoType (cIT 0) False "" NoUnary (Postfix NoType (cIT 0)
        (Primary NoType (cIT 0) True "toto" NoExpressions) False []))) >>
    putStrLn ("Unary: 2: " ++ show (unary2 == Unary NoType (cIT 0) True "!" (Unary NoType (cIT 1) False "" NoUnary (Postfix NoType (cIT 1)
      (Primary NoType (cIT 1) True "0" NoExpressions) False [])) NoPostfix))
  where Right (unary, (rest, _)) = SUT.getUnary (["toto", "tata"], getVoidInfos "toto.koak")
        Right (unary2, (rest2, _)) = SUT.getUnary (["!", "0", ":"], getVoidInfos "toto.koak")

binaryTests :: IO()
binaryTests =
    putStrLn ("Binary: no binary: " ++ show (false == NoBinary)) >>
    putStrLn ("Binary: 2: " ++ show (binary2 == Binary NoType (cIT 0) "==" (Unary NoType (cIT 2) False "" NoUnary
     (Postfix NoType (cIT 2) (Primary NoType (cIT 2) True "1" NoExpressions) False [])) False NoBinary))
  where Right (false, _) = SUT.getBinary ([")"], getVoidInfos "toto.koak")
        Right (binary2, (rest, _)) = SUT.getBinary (["=", "=", "1", ")"], getVoidInfos "toto.koak")

expressionTests :: IO()
expressionTests =
    putStrLn ("Expression: has binop: " ++ show (expression == ExprExpr NoType (cIT 0) (Unary NoType (cIT 0) False "" NoUnary (
    Postfix NoType (cIT 0) (Primary NoType (cIT 0) True "toto" NoExpressions) False [])) True (Binary NoType (cIT 4) "||" (Unary NoType (cIT 6)
    False "" NoUnary (Postfix NoType (cIT 6) (Primary NoType (cIT 6) True "tata" NoExpressions) False [])) False NoBinary))) >>
    putStrLn ("Expression: 2: has binop: " ++ show (hasBinary expression2 == False))
  where Right (expression2, _) = SUT.getExprExpr (["toto"], getVoidInfos "toto.koak")
        Right (expression, _) = SUT.getExprExpr (["toto", "|", "|", "tata"], getVoidInfos "toto.koak")

statDefParsTests :: Statement -> IO()
statDefParsTests a = putStrLn ("Parser: Definition: " ++ show (a == (Statement (cIT 0) True (Definition (cIT 3) NoType (
    Prototype NoType (cIT 3) "test" [(Argument (cIT 10) "x" (Type "double"))] (Type "double")) (Expressions NoType (cIT 32) Expr (toDyn [(ExprExpr NoType
    (cIT 32) (Unary NoType (cIT 32) False "" NoUnary (Postfix NoType (cIT 32) (Primary NoType (cIT 32) True "x" NoExpressions) False []))
    True (Binary NoType (cIT 34) "+" (Unary NoType (cIT 36) False "" NoUnary (Postfix NoType (cIT 36) (Primary NoType (cIT 36) True "2.0"
    NoExpressions) False [])) False NoBinary))]))) NoExpressions)))

statExprParsTests :: Statement -> IO()
statExprParsTests a = putStrLn ("Parser: Expression: " ++ show (a == (Statement (cIT 41) False NoDefinition (Expressions NoType (cIT 41)
    Expr (toDyn [(ExprExpr NoType (cIT 41) (Unary NoType (cIT 41) False "" NoUnary (Postfix NoType (cIT 41) (Primary NoType (cIT 41) True "test"
    NoExpressions) True [(Expressions NoType (cIT1 6) Expr (toDyn [(ExprExpr NoType (cIT1 6) (Unary NoType (cIT1 6) False "" NoUnary (Postfix NoType
    (cIT1 6) (Primary NoType (cIT1 6) True "5.0" NoExpressions) False [])) False NoBinary)]))])) True (Binary NoType (cIT1 10) "-"
    (Unary NoType (cIT1 12) False "" NoUnary (Postfix NoType (cIT1 12) (Primary NoType (cIT1 12) True "2" NoExpressions) False [])) True
    (Binary NoType (cIT1 14) "*" (Unary NoType (cIT1 16) False "" NoUnary (Postfix NoType (cIT1 16) (Primary NoType (cIT1 16) True "3"
    NoExpressions) False [])) True (Binary NoType (cIT1 18) "+" (Unary NoType (cIT1 20) False "" NoUnary (Postfix NoType (cIT1 20) (
    Primary NoType (cIT1 20) True "1" NoExpressions) False [])) False NoBinary))))])))))

parserFullTests :: IO()
parserFullTests =
  putStrLn ("Parser: length: " ++ show (length result == 0)) >>
  putStrLn ("Parser: 2: length: " ++ show (length result1 == 2)) >>
  statDefParsTests defS >>
  statExprParsTests expressionsS
  where  Right result = SUT.parser "" "toto.koak"
         Right result1 = SUT.parser "def test ( x : double ) : double x + 2.0;\ntest (5.0) - 2 * 3 + 1;" "toto.koak"
         [defS, expressionsS] = result1


parserTests :: IO()
parserTests =
    primaryTests >>
    callExprTests >>
    postfixTests >>
    unaryTests >>
    binaryTests >>
    expressionTests >>
    parserFullTests

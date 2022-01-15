module Utils.FindType where
import Parser.Inheritance
import Parser.ParserDataTypes
import Parser.Utils
import qualified LLVM.AST.Type               as T

findType :: GetType a => a -> T.Type
findType = findTypeFromType . getType

findTypeFromType :: Type -> T.Type
findTypeFromType x
    | x == Type "double" = T.double
    | x == Type "int" = T.i32
    | x == Type "void" = T.void
    | isCallingType x = (findTypeFromType . returnCall) x
    | otherwise = T.double

findLastType :: [Statement] -> Int
findLastType [] = 0
findLastType [Statement _ True _ _] = 0
findLastType t | all isDefinition t = 0
findLastType t = case getType $ expressionStatement $ last $ filter (not . isDefinition) t of
    Type "double" -> 1
    Type "int" -> 2
    _ -> 0
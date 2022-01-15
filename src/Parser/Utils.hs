module Parser.Utils where

import           Parser.ParserDataTypes

addLineInfos :: ([String], Infos) -> ([String], Infos)
addLineInfos (list, infos) = (list, Infos (filename infos) (1 + line infos) 0)

addColumnInfos :: ([String], Infos) -> Integer -> ([String], Infos)
addColumnInfos (list, infos) inc =
  (list, Infos (filename infos) (line infos) (column infos + inc))

getNextString :: ([String], Infos) -> (String, ([String], Infos))
getNextString (" ":list, infos) = getNextString $ addColumnInfos (list, infos) 1
getNextString ("\t":list, infos) =
  getNextString $ addColumnInfos (list, infos) 4
getNextString ("\n":list, infos) = getNextString $ addLineInfos (list, infos)
getNextString (a:list, infos) =
  (a, addColumnInfos (list, infos) (toInteger $ length a))
getNextString ([], infos) = ("", ([], infos))

getVoidInfos :: String -> Infos
getVoidInfos filename = Infos filename 0 0

throw :: ([String], Infos) -> String -> a
throw inf err = error (showInfos inf ++ " " ++ err)

showInfos :: ([String], Infos) -> String
showInfos (_, infos) =
  "<" ++
  filename infos ++
  ":" ++ show (line infos) ++ ":" ++ show (column infos) ++ ">"

isCallingType :: Type -> Bool
isCallingType NoType              = False
isCallingType (Type a)            = False
isCallingType (CallingType _ _ _) = True

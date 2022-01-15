module Parser.Splitter
  ( splitter
  ) where

import           Data.Dynamic
import           Debug.Trace  (trace)

cutter :: String -> [Bool] -> [String]
cutter [a] [b] = [[a]]
cutter (a:r) (b:s)
  | n || b = [a] : cutter r s
  where
    n = head s
cutter (a:r) (b:s) = (a : head res) : tail res
  where
    res = cutter r s

mySplit :: (Char -> Bool) -> String -> [String]
mySplit ver ""     = []
mySplit ver string = cutter string (map ver string)

cutBySeveral :: (String -> (Bool, String, String)) -> String -> [String]
cutBySeveral ver "" = []
cutBySeveral ver string
  | bool || bool2 = new : cutBySeveral ver rest
  | otherwise = (new ++ head result) : tail result
  where
    (bool, new, rest) = ver string
    (bool2, _, _) = ver rest
    result = cutBySeveral ver rest

mySplitOn :: (String -> (Bool, String, String)) -> String -> [String]
mySplitOn ver ""     = []
mySplitOn ver string = cutBySeveral ver string

oneOf :: String -> Char -> Bool
oneOf [] char = False
oneOf [key] char = key == char
oneOf (a:list) char
  | a == char = True
  | otherwise = oneOf list char

onOneSublist :: [String] -> String -> (Bool, String, String)
onOneSublist [] "" = (False, "", "")
onOneSublist [] string = (False, [head string], tail string)
onOneSublist [token] string
  | null string = (False, "", "")
  | length token > length string = (False, [head string], tail string)
  | length token == length string && token == string = (True, string, "")
  | token == take (length token) string =
    (True, token, drop (length token) string)
  | otherwise = (False, [head string], tail string)
onOneSublist (token:rest) string
  | null string = (False, "", "")
  | length token > length string = onOneSublist rest string
  | length token == length string && token == string = (True, string, "")
  | token == take (length token) string =
    (True, token, drop (length token) string)
  | otherwise = onOneSublist rest string

replace :: String -> String -> String -> String
replace token replacing string
  | length token > length string = string
replace token replacing string
  | length token == length string && token == string = replacing
replace token replacing string
  | token == take (length token) string =
    replacing ++ replace token replacing (drop (length token) string)
replace token replacing (a:str) = a : replace token replacing str

splitRemoveOne :: String -> [String] -> [String]
splitRemoveOne token list =
  filter (/= token) $
  filter (/= "") $ append (map (mySplit (oneOf token)) list) []

splitReplaceOne :: String -> String -> [String] -> [String] -- only works with replacing of 1 character
splitReplaceOne token replacing list =
  append (map (mySplit (oneOf replacing) . replace token replacing) list) []

splitKeepSeveral :: String -> [String] -> [String]
splitKeepSeveral token list = append (map (mySplit (oneOf token)) list) []

splitKeepOne :: [String] -> [String] -> [String]
splitKeepOne tokens list =
  append (map (mySplitOn (onOneSublist tokens)) list) []

append :: [[String]] -> [String] -> [String]
append [] ret    = ret
append [a] ret   = ret ++ a
append (a:b) ret = append b (ret ++ a)

splitter :: String -> [String]
splitter line =
  (splitReplaceOne "\r\n" "\n" . splitKeepSeveral ":;,()=*/|%&+-<>! \t") [line]

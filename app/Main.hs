module Main where

import           Lib
import           System.Environment

main :: IO ()
main = getArgs >>= \args -> launcher args

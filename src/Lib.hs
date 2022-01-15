{-# LANGUAGE OverloadedStrings #-}

module Lib where

import           Compiler.Run
import           IR.Builder
import qualified LLVM.AST              as AST
import           Parser.InferringTypes
import           Parser.Parser

launcher :: [String] -> IO ()
launcher [] = error "No arguments given in parameter"
launcher [file] =
  readFile file >>= \contents ->
    case inferringTypes $ parser contents file of
      Right (_, _, r) -> do
        run r $ builder r
      Left msg -> putStrLn msg
launcher (file:args) =
  readFile file >>= \contents ->
    case inferringTypes $ parser contents file of
      Right (_, _, r) -> do
        run r $ builder r
      Left msg -> putStrLn msg >> launcher args

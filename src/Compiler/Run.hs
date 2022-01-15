{-# LANGUAGE OverloadedStrings #-}

module Compiler.Run where

import qualified Data.ByteString.Char8  as BS
import           Foreign.Ptr            (FunPtr, castFunPtr)
import qualified LLVM.AST               as AST
import           LLVM.Context
import qualified LLVM.ExecutionEngine   as E
import           LLVM.Module
import           LLVM.PassManager
import qualified Parser.ParserDataTypes as PDT
import           Utils.FindType

foreign import ccall "dynamic" doubleFunc :: FunPtr (IO Double) -> (IO Double)

foreign import ccall "dynamic" intFunc :: FunPtr (IO Int) -> (IO Int)

runDFunc :: FunPtr a -> IO Double
runDFunc fn = doubleFunc (castFunPtr fn :: FunPtr (IO Double))

runIFunc :: FunPtr a -> IO Int
runIFunc fn = intFunc (castFunPtr fn :: FunPtr (IO Int))

jitExec :: Context -> (E.MCJIT -> IO a) -> IO a
jitExec c = E.withMCJIT c (Just 0) Nothing Nothing Nothing

pass :: PassSetSpec
pass = defaultCuratedPassSetSpec {optLevel = Just 3}

run :: [PDT.Statement] -> AST.Module -> IO ()
run stats mod = do
  withContext $ \context ->
    jitExec context $ \executionEngine ->
      withModuleFromAST context mod $ \m ->
        withPassManager pass $ \pm -> do
          optmod <- moduleAST m
          s <- moduleLLVMAssembly m
          E.withModuleInEngine executionEngine m $ \e -> do
            mainfn <- E.getFunction e "main"
            case mainfn of
              Just fn -> result fn
              Nothing -> return ()
  where
    lastType = findLastType stats
    result fn
      | lastType == 0 = return ()
      | lastType == 1 = runDFunc fn >>= \res -> putStrLn $ show res
      | otherwise = runIFunc fn >>= \res -> putStrLn $ show res

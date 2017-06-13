module Genius.Repl where

import           Genius.IOPrimitives
import           Genius.Parsers
import           Genius.Primitives
import           Genius.Types
import           Genius.Core

-- import           Control.Monad
import           Control.Monad.Error

import           System.IO

-- |Start a REPL.
runRepl :: IO ()
runRepl = primitiveBindings >>= until_ (== "quit") (readPrompt "Lisp>>> ") . evalAndPrint

-- |Execute a list of expressions and return an IO.
runOne :: [String] -> IO ()
runOne args = do
    env <- primitiveBindings >>= flip bindVars [("args", List $ map String $ drop 1 args)]
    (runIOThrows $ liftM show $ eval env (List [Atom "load", String (args !! 0)]))
        >>= hPutStrLn stderr

-- |Execute a list of expressions and return a String.
runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

-- |Evaluate a string in an environment and return its result.
evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr =  evalString env expr >>= putStrLn

-- |Evaluate a string in an environment and return its result.
evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ liftM show $ (liftThrows $ parseExprOrThrow expr) >>= eval env

-- |Return an environment which includes primitives and IO primitives functions.
primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map (makeFn IOFunc) ioPrimitives
                                               ++ map (makeFn PrimitiveFunc) primitives)
     where makeFn constructor (var, func) = (var, constructor func)

-- HELPERS

-- |Display a string and flush stdout.
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- |Display a prompt and return a line from stdin.
readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine

-- |Apply an action until a condition is satisfied.
until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ predicate prompt action = do
   result <- prompt
   if predicate result
      then return ()
      else action result >> until_ predicate prompt action

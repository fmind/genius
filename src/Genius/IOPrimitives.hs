module Genius.IOPrimitives where

import           Genius.Core
import           Genius.Parsers
import           Genius.Types

import           Control.Monad
import           Control.Monad.Error

import           System.IO

-- MAIN FUNCTIONS

ioPrimitives :: [(String, [LispVal] -> IOThrowsError LispVal)]
ioPrimitives =
  [ ("open-input-file", makePort ReadMode)
  , ("open-output-file", makePort WriteMode)
  , ("close-input-port", closePort)
  , ("close-output-port", closePort)
  , ("read", readProc)
  , ("write", writeProc)
  , ("apply", applyProc)
  , ("read-all", readAll)
  , ("read-contents", readContents)
  ]

-- PRIMITIVES

-- |Open a file handle.
makePort :: IOMode -> [LispVal] -> IOThrowsError LispVal
makePort mode [String filename] = liftM Port $ liftIO $ openFile filename mode
makePort _ _ = throwError $ Default "Can only make a port from a filename"

-- |Close a file handle.
closePort :: [LispVal] -> IOThrowsError LispVal
closePort [Port port] = liftIO $ hClose port >> (return $ Bool True)
closePort _           = return $ Bool False

-- |Convert a string to a LISP value.
readProc :: [LispVal] -> IOThrowsError LispVal
readProc []          = readProc [Port stdin]
readProc [Port port] = (liftIO $ hGetLine port) >>= liftThrows . parseExprOrThrow
readProc _ = throwError $ Default "Can only read a proc from a port"

-- |Convert a LISP value to a string.
writeProc :: [LispVal] -> IOThrowsError LispVal
writeProc [obj]            = writeProc [obj, Port stdout]
writeProc [obj, Port port] = liftIO $ hPrint port obj >> (return $ Bool True)
writeProc _ = throwError $ Default "Can only write an object to a port"

-- |Apply a function to a list of arguments.
applyProc :: [LispVal] -> IOThrowsError LispVal
applyProc [func, List args] = apply func args
applyProc (func : args)     = apply func args
applyProc [] = throwError $ Default "Cannot apply on empty list"

-- |Convert the content of a file to a list.
readAll :: [LispVal] -> IOThrowsError LispVal
readAll [String filename] = liftM List $ load filename
readAll _ = throwError $ Default "Can only read the content of a file from a file name"

-- |Convert the content of a file to a string.
readContents :: [LispVal] -> IOThrowsError LispVal
readContents [String filename] = liftM String $ liftIO $ readFile filename
readContents _ = throwError $ Default "Can only read the content of a file from a file name"

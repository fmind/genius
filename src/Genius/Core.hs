module Genius.Core where

import Genius.Types
import Genius.Parsers

import Control.Monad.Error

-- |Load values from a string (read)
load :: String -> IOThrowsError [LispVal]
load filename = (liftIO $ readFile filename) >>= liftThrows . parseExprListOrThrow

-- |Apply a function to its arguments.
apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args
apply (Func values varargs forms close) args =
      if num values /= num args && varargs == Nothing
         then throwError $ NumArgs (num values) args
         else (liftIO $ bindVars close $ zip values args) >>= bindVarArgs varargs >>= evalBody
      where remainingArgs = drop (length values) args
            num = toInteger . length
            evalBody env = liftM last $ mapM (eval env) forms
            bindVarArgs arg env = case arg of
                Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
                Nothing -> return env
apply (IOFunc func) args = func args
apply badForm _ = throwError $ BadSpecialForm "Unrecognized function" badForm

-- |Evaluate the value of an expression.
eval :: Env -> LispVal -> IOThrowsError LispVal
eval _ val@(String _) = return val
eval _ val@(Number _) = return val
eval _ val@(Bool _) = return val
eval env (Atom sym) = getVar env sym
eval env (List [Atom "if", predicate, conseq, alt]) = do
  result <- eval env predicate
  case result of
    Bool False -> eval env alt
    _  -> eval env conseq
eval _ (List [Atom "quote", val]) = return val
eval env (List [Atom "set!", Atom var, form]) =
     eval env form >>= setVar env var
eval env (List [Atom "define", Atom var, form]) =
     eval env form >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : values) : forms)) =
     makeNormalFunc env values forms >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : values) varargs : forms)) =
     makeVarArgs varargs env values forms >>= defineVar env var
eval env (List (Atom "lambda" : List values : forms)) =
     makeNormalFunc env values forms
eval env (List (Atom "lambda" : DottedList values varargs : forms)) =
     makeVarArgs varargs env values forms
eval env (List (Atom "lambda" : varargs@(Atom _) : forms)) =
     makeVarArgs varargs env [] forms
eval env (List [Atom "load", String filename]) =
     load filename >>= liftM last . mapM (eval env)
eval env (List (function : args)) = do
     func <- eval env function
     argVals <- mapM (eval env) args
     apply func argVals
eval _ badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

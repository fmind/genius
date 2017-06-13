module Genius.Types where

import           Control.Monad.Error

import           Data.IORef
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec (ParseError)

-- ENV

-- |Represent a LISP environment (bindings).
type Env = IORef [(String, IORef LispVal)]

-- |Create an empty environment.
nullEnv :: IO Env
nullEnv = newIORef []

-- |Check if a variable is bound to an environment
isBound :: Env -> String -> IO Bool
isBound envRef var = do env <- liftIO $ readIORef envRef
                        return $ maybe False (const True) (lookup var env)
-- isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- |Return the value associated to a given variable in an environment.
getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

-- |Change the value associated to a given variable in an environment.
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

-- |Create the value associated to a given variable in an environment.
defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value

-- |Bind a list of variable/value pairs to an environment.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv binds env = liftM (++ env) (mapM addBinding binds)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- |Construct a regular function.
makeNormalFunc :: Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeNormalFunc = makeFunc Nothing

-- |Construct a function with variable arguments.
makeVarArgs :: LispVal -> Env -> [LispVal] -> [LispVal] -> IOThrowsError LispVal
makeVarArgs = makeFunc . Just . showVal

-- |Construct a function in the context of a monad.
makeFunc :: Monad m => Maybe String -> Env -> [LispVal] -> [LispVal] -> m LispVal
makeFunc varargs env values forms = return $ Func (map showVal values) varargs forms env

-- LISP VALUE

-- |Represent a LISP value.
data LispVal
  = Bool Bool
  | Port Handle
  | Atom String
  | String String
  | Number Integer
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Func { params  :: [String]
         , vararg  :: (Maybe String)
         , body    :: [LispVal]
         , closure :: Env }
  | IOFunc ([LispVal] -> IOThrowsError LispVal)
  | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)

instance Show LispVal where show = showVal

-- |Show a LISP value.
showVal :: LispVal -> String
showVal (Atom name) = name
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (Port _)   = "<IO port>"
showVal (IOFunc _) = "<IO primitive>"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Number contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList h t) = "(" ++ unwordsList h ++ " . " ++ showVal t ++ ")"
showVal (Func {params = args, vararg = varargs}) =
   "(lambda (" ++ unwords (map show args) ++
      (case varargs of
         Nothing  -> ""
         Just arg -> " . " ++ arg) ++ ") ...)"

-- |Concatenate a list of LISP value.
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |Unpack an Integer from a LISP value.
unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                             then throwError $ TypeMismatch "number" $ String n
                             else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum     = throwError $ TypeMismatch "number" notNum

-- |Unpack String from a LISP value.
unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s)   = return $ show s
unpackStr notString  = throwError $ TypeMismatch "string" notString

-- |Unpack Boolean from a LISP value.
unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool  = throwError $ TypeMismatch "boolean" notBool

-- |Unpack Equal type from a LISP value.
unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

-- LISP ERROR

-- |Represent a LISP error.
data LispError = Default String
               | Parser ParseError
               | UnboundVar String String
               | NotFunction String String
               | NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | BadSpecialForm String LispVal

instance Error LispError where
     strMsg = Default
     noMsg = Default "A LISP error has occurred"

instance Show LispError where show = showError

-- |Show a LISP error.
showError :: LispError -> String
showError (UnboundVar message varname)  = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func)    = message ++ ": " ++ show func
showError (NumArgs expected found)      = "Expected " ++ show expected
                                       ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr)             = "Parse error at " ++ show parseErr
showError _ = error "Error cannot be show (unsupported LispError)"

-- |Represent a function that can throw a LispError.
type ThrowsError = Either LispError

-- |Extract a value from a ThrowsError.
extractValue :: ThrowsError String -> String
extractValue (Left err) = show err
extractValue (Right val) = val

-- |Represent an IO Function that can throw a LispError.
type IOThrowsError = ErrorT LispError IO

-- |Lift a ThrowsError to a IOThrowsError.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

-- | Catch a potential error given an action.
trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

-- |Represent a generic unpacker for any type of LISP value with equal typeclass.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

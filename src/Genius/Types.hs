-- |Types related to the implementation.

module Genius.Types where

import           Control.Monad.Error

import           Data.IORef
import           System.IO                     (Handle)
import           Text.ParserCombinators.Parsec (ParseError)

-- |A LISP environment (list of bindinds).
type Env = IORef [(String, IORef LispVal)]

-- |Create an empty environment.
nullEnv :: IO Env
nullEnv = newIORef []

-- |Check if a variable is bound to an environment
isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var

-- |Bind a list of variable/value pairs to an environment.
bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv binds env = liftM (++ env) (mapM addBinding binds)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)

-- |A LISP value.
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

-- |Show a list of LISP value.
unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

-- |A LISP error.
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

-- |Function that can throw a LispError.
type ThrowsError = Either LispError

-- |Extract a value from a ThrowsError.
extractValue :: ThrowsError String -> String
extractValue (Left err) = show err
extractValue (Right val) = val

-- |IO Function that can throw a LispError.
type IOThrowsError = ErrorT LispError IO

-- |Lift a ThrowsError to a IOThrowsError.
liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err)  = throwError err
liftThrows (Right val) = return val

-- | Catch a potential error given an action.
trapError :: (Show a, MonadError a m) => m String -> m String
trapError action = catchError action (return . show)

-- |Generic unpacker for any type of LISP value.
data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

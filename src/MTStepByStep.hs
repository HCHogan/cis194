{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module MTStepByStep (runEval1, eval0, eval1, eval2b, eval2, runEval3, eval3, runEval4, eval4, runEval5, runEval6, eval6) where

import Control.Monad.Except (MonadError (throwError))
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Except
import Control.Monad.Writer
import qualified Data.Map as Map
import Data.Maybe (fromJust)

type Name = String -- variable names

data Exp -- expressions
  = Lit Integer
  | Var Name
  | Plus Exp Exp
  | Abs Name Exp
  | App Exp Exp
  deriving (Show)

data Value -- values
  = IntVal Integer
  | FunVal Env Name Exp
  deriving (Show)

type Env = Map.Map Name Value -- mappings from names to values

instance MonadFail Identity where
  fail = error

eval0 :: Env -> Exp -> Value
eval0 _ (Lit i) = IntVal i
eval0 env (Var n) = fromJust (Map.lookup n env)
eval0 env (Plus e1 e2) =
  let IntVal i1 = eval0 env e1
      IntVal i2 = eval0 env e2
   in IntVal (i1 + i2)
eval0 env (Abs n e) = FunVal env n e
eval0 env (App e1 e2) =
  let val1 = eval0 env e1
      val2 = eval0 env e2
   in case val1 of
        FunVal env' n body -> eval0 (Map.insert n val2 env') body
        _ -> error "type error in application"

newtype Eval1 a = Eval1 {runEval1 :: Identity a} deriving (Functor, Applicative, Monad)

instance MonadFail Eval1 where
  fail = error

eval1 :: Env -> Exp -> Eval1 Value
eval1 _ (Lit i) = return $ IntVal i
eval1 env (Var n) = Eval1 $ Identity $ fromJust $ Map.lookup n env
eval1 env (Plus e1 e2) = do
  IntVal i1 <- eval1 env e1
  IntVal i2 <- eval1 env e2
  return $ IntVal (i1 + i2)
eval1 env (Abs n e) = return $ FunVal env n e
eval1 env (App e1 e2) = do
  val1 <- eval1 env e1
  val2 <- eval1 env e2
  case val1 of
    FunVal env' n body -> eval1 (Map.insert n val2 env') body
    _ -> fail "type error"

-- Basically, we use a stack of monad transformers to morph our original type, with the innermost monad being Identity or IO.

type Eval2 a = ExceptT String Identity a

eval2b :: Env -> Exp -> Eval2 Value
eval2b _ (Lit i) = return $ IntVal i
eval2b env (Var n) = case Map.lookup n env of
  Just x -> return x
  Nothing -> throwE "unbound"
eval2b env (Plus e1 e2) = do
  -- the drawback is that the error message only talks about "pattern match failure"
  IntVal i1 <- eval2b env e1
  IntVal i2 <- eval2b env e2
  return $ IntVal (i1 + i2)
eval2b env (Abs n e) = return $ FunVal env n e
eval2b env (App e1 e2) = do
  val1 <- eval2b env e1
  val2 <- eval2b env e2
  case val1 of
    FunVal env' n body -> eval2b (Map.insert n val2 env') body
    _ -> throwE "type error in application"

eval2 :: Env -> Exp -> Eval2 Value
eval2 _ (Lit i) = return $ IntVal i
eval2 env (Var n) = case Map.lookup n env of
  Just x -> return x
  Nothing -> throwE $ "unbound variable: " ++ n
eval2 env (Plus e1 e2) = do
  e1' <- eval2 env e1
  e2' <- eval2 env e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwE "type error in addition"
eval2 env (Abs n e) = return $ FunVal env n e
eval2 env (App e1 e2) = do
  val1 <- eval2 env e1
  val2 <- eval2 env e2
  case val1 of
    FunVal env' n body -> eval2 (Map.insert n val2 env') body
    _ -> throwE "type errpr in application"

-- A Reader Monad passes a value into a computation and all its sub-computations. This value can be read by all enclosed computations and get modified for *nested* computations.
-- In contrast to the State Monad, an encapsulated computation cannot change the calue used by surrounding computations.

type Eval3 a = ReaderT Env (ExceptT String Identity) a
runEval3 :: Env -> Eval3 a -> Either String a
runEval3 env ev = runIdentity $ runExceptT $ runReaderT ev env

eval3 :: Exp -> Eval3 Value
eval3 (Lit i) = return $ IntVal i
eval3 (Var n) = do
  env <- ask
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "unbound variable: " ++ n
eval3 (Plus e1 e2) = do
  e1' <- eval3 e1
  e2' <- eval3 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
eval3 (Abs n e) = do
  env <- ask
  return $ FunVal env n e
eval3 (App e1 e2) = do
  val1 <- eval3 e1
  val2 <- eval3 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval3 body)
    _ -> throwError "type error in application"

-- let's ignore ReaderT as it does not affect the return value

type Eval4 a = ReaderT Env (ExceptT String (StateT Integer Identity)) a

runEval4 :: Env -> Integer -> Eval4 a -> (Either String a, Integer)
runEval4 env st ev = runIdentity $ runStateT (runExceptT $ runReaderT ev env) st

tick :: (Num s, MonadState s m) => m ()
tick = do
  st <- get
  put (st + 1)

eval4 :: Exp -> Eval4 Value
eval4 (Lit i) = do
  tick
  return $ IntVal i
eval4 (Var n) = do
  tick
  env <- ask
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "unbound variable: " ++ n
eval4 (Plus e1 e2) = do
  tick
  e1' <- eval4 e1
  e2' <- eval4 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
eval4 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval4 (App e1 e2) = do
  tick
  val1 <- eval4 e1
  val2 <- eval4 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval4 body)
    _ -> throwError "type error in application"

type Eval5 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer Identity))) a
runEval5 :: Env -> Integer -> Eval5 a -> ((Either String a, [String]), Integer)
runEval5 env st ev = runIdentity $ runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

-- eval implemented as the same, but with tell

type Eval6 a = ReaderT Env (ExceptT String (WriterT [String] (StateT Integer IO))) a
runEval6 :: Env -> Integer -> Eval6 a -> IO ((Either String a, [String]), Integer)
runEval6 env st ev = runStateT (runWriterT $ runExceptT $ runReaderT ev env) st

eval6 :: Exp -> Eval6 Value
eval6 (Lit i) = do
  tick
  liftIO $ print i
  return $ IntVal i
eval6 (Var n) = do
  tick
  tell [n]
  env <- ask
  case Map.lookup n env of
    Just x -> return x
    Nothing -> throwError $ "unbound variable: " ++ n
eval6 (Plus e1 e2) = do
  tick
  e1' <- eval6 e1
  e2' <- eval6 e2
  case (e1', e2') of
    (IntVal i1, IntVal i2) -> return $ IntVal (i1 + i2)
    _ -> throwError "type error in addition"
eval6 (Abs n e) = do
  tick
  env <- ask
  return $ FunVal env n e
eval6 (App e1 e2) = do
  tick
  val1 <- eval6 e1
  val2 <- eval6 e2
  case val1 of
    FunVal env' n body -> local (const (Map.insert n val2 env')) (eval6 body)
    _ -> throwError "type error in application"

-- eval0 :: Env -> Exp -> Value

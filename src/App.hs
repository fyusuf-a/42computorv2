{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App where

import System.Console.Haskeline (InputT, runInputT, Settings, defaultSettings, MonadException, controlIO, RunIO(..))
import Control.Monad.Trans.State.Strict (StateT, evalStateT, put, get)
import Control.Monad.State (lift)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import Complex (Complex)

type VarList = Map String Complex

--newtype Calculation a = Calculation {runCalc :: InputT (StateT VarList IO) a}
  --deriving (Functor, Applicative, Monad, MonadState VarList, MonadIO)
--type Calculation a = InputT (ExceptT CustomError (StateT VarList IO)) a
type Calculation a = InputT (StateT VarList IO) a

--instance (MonadState s m) => MonadState s (InputT m) where
  --p
  --t s = lift $ put s
  --get = lift get

--instance MonadException m => MonadException (ExceptT e m) where
  --controlIO f = ExceptT $ controlIO $ \(RunIO run) -> let
                  --run' = RunIO (fmap ExceptT . run . runExceptT)
                  --in fmap runExceptT $ f run'

runCalculation :: Calculation a -> VarList -> Settings _ -> IO a
runCalculation calc st settings = evalStateT (runInputT settings calc) st
  --case runExceptT calc of
    --Left e -> return $ Left e
    --Right calc' -> Right $ evalStateT (runInputT settings calc') st
  --where calc' = runCalc calc

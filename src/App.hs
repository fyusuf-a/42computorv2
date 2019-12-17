{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PartialTypeSignatures #-}

module App where

import System.Console.Haskeline (InputT, runInputT, Settings, defaultSettings)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Control.Monad.State (MonadState, lift, put, get)
import Control.Monad.IO.Class (MonadIO)
import Data.Map (Map)
import Complex (Complex)

type VarList = Map String Complex

--newtype Calculation a = Calculation {runCalc :: InputT (StateT VarList IO) a}
  --deriving (Functor, Applicative, Monad, MonadState VarList, MonadIO)
type Calculation a = StateT VarList (InputT IO) a

--instance (MonadState s m) => MonadState s (InputT m) where
  --put s = lift $ put s
  --get = lift get

--instance InputT Calculation where


runCalculation :: Calculation a -> VarList -> Settings IO -> IO a
runCalculation calc st settings = runInputT settings (evalStateT calc st)
  --where calc' = runCalc calc

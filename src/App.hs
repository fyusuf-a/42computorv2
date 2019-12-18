module App where

import System.Console.Haskeline (InputT, runInputT, Settings, defaultSettings)
import Control.Monad.Trans.State.Strict (StateT, evalStateT)
import Data.Map (Map)
import Complex (Complex)

type VarList = Map String Complex

type Calculation a = InputT (StateT VarList IO) a

runCalculation :: Calculation a -> VarList -> Settings (StateT VarList IO) -> IO a
runCalculation calc st settings = evalStateT (runInputT settings calc) st

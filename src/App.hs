module App where

import Control.Monad.State
import Data.Map

newtype App = App (StateT (Map String Exp) IO)

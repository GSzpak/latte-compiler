module Frontend where

import AbsLatte
import PrintLatte
import qualified Data.Map as Map
import Control.Monad.Error
import Control.Monad.Reader

type Environment = Map.Map String Type

module Util where

import qualified Data.Map as DataMap
import Control.Monad.State
import Control.Monad.Except

-- Get a value at key @k from map @m or raise an error @err
mustGet :: MonadError String m => Show k => Ord k => DataMap.Map k a -> k -> String -> m a
mustGet m k err = case m DataMap.!? k of
  Nothing -> throwError $ show k ++ err
  Just v -> return v

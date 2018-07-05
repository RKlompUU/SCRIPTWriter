{-# LANGUAGE DeriveDataTypeable #-}
module KlompStandard where

import qualified Data.Typeable as T
import qualified Data.Data as TD

import Control.Monad.State.Lazy

type CounterState a = State Int a

tickCounter :: CounterState Int
tickCounter = do
  i <- get
  put (i+1)
  return i

evalCounter :: CounterState a -> a
evalCounter f = evalState f 0


ccEq :: (TD.Data a) => a -> TD.Constr -> Bool
ccEq e c = TD.toConstr e == c


{-
class CC a where
  cc :: a -> TD.Constr

instance TD.Data a => CC a where
  cc = TD.toConstr

instance CC a => CC (b -> a) where
  cc f = cc (f undefined)
-}
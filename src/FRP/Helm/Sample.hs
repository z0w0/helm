module FRP.Helm.Sample (
  Sample(..),
  value,
  update
) where

import Control.Applicative

data Sample a = Changed a | Unchanged a
  deriving (Show, Eq)

instance Functor Sample where
  fmap = liftA

instance Applicative Sample where
  pure = Unchanged
  (Changed   f) <*> (Changed   x) = Changed (f x)
  (Changed   f) <*> (Unchanged x) = Changed (f x)
  (Unchanged f) <*> (Changed   x) = Changed (f x)
  (Unchanged f) <*> (Unchanged x) = Unchanged (f x)

value :: Sample a -> a
value (Changed   x) = x
value (Unchanged x) = x

update :: Eq a => p -> a -> Sample a -> Sample a
update _ new old = if new == value old
                   then Unchanged $ value old
                   else Changed new

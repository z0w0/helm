{-| Contains miscellaneous utility functions such as functions for working with signals and signal generators. -}
module FRP.Helm.Utilities where
import Control.Applicative
import Control.Monad ((>=>), ap)
import FRP.Elerea.Simple hiding (Signal)
import qualified FRP.Elerea.Simple as Elerea (Signal)
import System.Random (Random, randomIO, randomRIO)

data Event a = Changed a | Unchanged a

instance Functor Event where
    fmap f (Changed a)   = Changed (f a)
    fmap f (Unchanged a) = Unchanged (f a)

instance Applicative Event where
    pure = Unchanged
    (Changed   f) <*> (Changed   x) = Changed (f x)
    (Changed   f) <*> (Unchanged x) = Changed (f x)
    (Unchanged f) <*> (Changed   x) = Changed (f x)
    (Unchanged f) <*> (Unchanged x) = Unchanged (f x)

data Signal a = Signal (SignalGen (Elerea.Signal (Event a)))

instance Functor Signal where
    fmap = liftA

instance Applicative Signal where
    pure = Signal . pure . pure . pure
    (Signal f) <*> (Signal x) = Signal $ (liftA2 (liftA2 (<*>))) f x

{-| Creates a signal that never changes. -}
constant :: a -> Signal a
constant x = Signal $ stateful (Changed x) (\_ -> Unchanged x)

lift :: (a -> b) -> Signal a -> Signal b
lift = fmap

lift2 :: (a -> b -> c) -> Signal a -> Signal b -> Signal c
lift2 f a b = fmap f a <*> b

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> Signal a -> Signal b
(<~) = lift

infixl 4 <~

{-| Converts degrees into the standard angle measurement (radians). -}
degrees :: Double -> Double
degrees n = n * pi / 180

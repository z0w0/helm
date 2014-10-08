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
    fmap f (Signal x) = Signal ((fmap . fmap . fmap) f x)

{-| Creates a signal that never changes. -}
constant :: a -> Signal a
constant x = Signal $ stateful (Changed x) (\_ -> Unchanged x)

lift :: (a -> b) -> Signal a -> Signal b
lift = fmap

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> Signal a -> Signal b
(<~) = lift

infixl 4 <~

{-| Applies a function within a signal to a signal. This is a wrapper around the builtin '<*>' operator
    that automatically binds the input signal out of the signal generator.

    > render <~ Window.dimensions ~~ Window.position
 -}
--type Signal a = SignalGen (Elerea.Signal (Event a))
--(~~) :: Signal (a -> b) -> Signal a -> Signal b
--(~~) s1 s2 = (<*>)


--infixl 4 ~~

{-| Converts degrees into the standard angle measurement (radians). -}
degrees :: Double -> Double
degrees n = n * pi / 180

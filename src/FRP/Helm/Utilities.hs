{-| Contains miscellaneous utility functions such as functions for working with signals and signal generators. -}
module FRP.Helm.Utilities where
import Control.Applicative ((<*>))
import Control.Monad ((>=>))
import FRP.Elerea.Simple hiding (Signal)
import qualified FRP.Elerea.Simple as Elerea (Signal)
import System.Random (Random, randomIO, randomRIO)

data Event a = Changed a | Unchanged a

instance Functor Event where
    fmap f (Changed a)   = Changed (f a)
    fmap f (Unchanged a) = Unchanged (f a)

type Signal a = SignalGen (Elerea.Signal (Event a))

{-| Creates a signal that never changes. -}
constant :: a -> Signal a
constant x = stateful (Changed x) (\_ -> Unchanged x)

lift :: (a -> b) -> Signal a -> Signal b
lift = fmap . fmap . fmap

{-| An alias for 'lift'. -}
(<~) :: (a -> b) -> Signal a -> Signal b
(<~) = lift

infixl 4 <~

{-| Converts degrees into the standard angle measurement (radians). -}
degrees :: Double -> Double
degrees n = n * pi / 180

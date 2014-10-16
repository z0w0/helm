{-# LANGUAGE ScopedTypeVariables #-}
module FRP.Helm.Random (
  range,
  float,
  floatList
) where
import Control.Applicative (pure)
import Control.Monad (liftM, join, replicateM)
import FRP.Elerea.Simple hiding (Signal)
import qualified FRP.Elerea.Simple as Elerea (Signal)
import FRP.Helm.Signal
import FRP.Helm.Sample
import System.Random (Random, randomRIO)

{-| Given a range from low to high and a signal of values, this produces
a new signal that changes whenever the input signal changes. The new
values are random number between 'low' and 'high' inclusive.
-}
range :: Int -> Int -> Signal a -> Signal Int
range x y = lift2 const $ pureRandom (x,y)

{-| Produces a new signal that changes whenever the input signal changes.
The new values are random numbers in [0..1).
-}
float :: Signal a -> Signal Float
float = lift2 const $ pureRandom (0,1)

{-| Produces a new signal of lists that changes whenever the input signal
changes. The input signal specifies the length of the random list. Each value is
a random number in [0..1).
-}
floatList :: Signal Int -> Signal [Float]
floatList s = Signal $ do
  s' <- signalGen s
  fl :: Elerea.Signal (SignalGen (Elerea.Signal [Float]))
     <- floatListGens s'
  ss :: Elerea.Signal (Elerea.Signal [Float])
     <- generator fl
  transfer2 (pure []) update_ s' (join ss)
  where
    floatListGens :: Elerea.Signal (Sample Int)
                       -> SignalGen (Elerea.Signal
                            (SignalGen (Elerea.Signal [Float])))
    floatListGens = transfer (return (return [])) makeGens
    makeGens new _ = case new of
            Changed   n -> liftM sequence $ replicateM n
                                          $ effectful
                                          $ randomRIO (0,1)
            Unchanged _ -> return (return [])
    update_ int new old = case int of
            Changed   _ -> Changed new
            Unchanged _ -> Unchanged $ value old

{-| A utility signal that creates a pure (samples are marked as unchanged and
will not drive rendering) random number signal based on the given range. -}
pureRandom :: Random a => (a, a) -> Signal a
pureRandom = Signal . effectful . liftM pure . randomRIO

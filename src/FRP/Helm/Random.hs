{-# LANGUAGE ScopedTypeVariables #-}
module FRP.Helm.Random (
  range,
  float,
  floatList
) where
import Control.Applicative (pure)
import Control.Monad (liftM, join, replicateM)
import FRP.Elerea.Param hiding (Signal)
import qualified FRP.Elerea.Param as Elerea (Signal)
import FRP.Helm.Signal
import FRP.Helm.Sample
import FRP.Helm.Engine
import System.Random (Random, randomRIO)

{-| Given a range from low to high and a signal of values, this produces
a new signal that changes whenever the input signal changes. The new
values are random number between 'low' and 'high' inclusive.
-}
range :: Int -> Int -> Signal a -> Signal Int
range x y = rand (x,y)

{-| Produces a new signal that changes whenever the input signal changes.
The new values are random numbers in [0..1).
-}
float :: Signal a -> Signal Float
float = rand (0,1)

{-| A utility signal that does the work for 'float' and 'range'. -}
rand :: (Random a, Num a) =>
          (a, a) -> Signal b -> Signal a
rand limits s = Signal $ do
  s' <- signalGen s
  rs :: Elerea.Signal (SignalGen Engine (Elerea.Signal a))
     <- randomGens limits s'
  r  :: Elerea.Signal (Elerea.Signal a)
     <- generator rs
  transfer2 (pure 0) update_ s' (join r)
  where
    update_ :: (Random a, Num a) => p ->
                  Sample b -> a -> Sample a -> Sample a
    update_ _ new random old = case new of
      Changed   _ -> Changed random
      Unchanged _ -> Unchanged $ value old
    randomGens :: (Random a, Num a) =>
                    (a,a) -> Elerea.Signal (Sample b)
                          -> SignalGen p (Elerea.Signal
                               (SignalGen p (Elerea.Signal a)))
    randomGens l = transfer (return (return 0)) (makeGen l)
    makeGen ::(Random a, Num a) => (a,a) -> p -> Sample b
                -> SignalGen p (Elerea.Signal a)
                -> SignalGen p (Elerea.Signal a)
    makeGen l _ new _ = case new of
      Changed   _ -> effectful $ randomRIO l
      Unchanged _ -> return $ return 0

{-| Produces a new signal of lists that changes whenever the input signal
changes. The input signal specifies the length of the random list. Each value is
a random number in [0..1).
-}
floatList :: Signal Int -> Signal [Float]
floatList s = Signal $ do
  s' <- signalGen s
  fl :: Elerea.Signal (SignalGen Engine (Elerea.Signal [Float]))
     <- floatListGens s'
  ss :: Elerea.Signal (Elerea.Signal [Float])
     <- generator fl
  transfer2 (pure []) update_ s' (join ss)
  where
    floatListGens :: Elerea.Signal (Sample Int)
                       -> SignalGen p (Elerea.Signal
                            (SignalGen p (Elerea.Signal [Float])))
    floatListGens = transfer (return (return [])) makeGen
    makeGen _ new _ = case new of
            Changed   n -> liftM sequence $ replicateM n
                                          $ effectful
                                          $ randomRIO (0,1)
            Unchanged _ -> return (return [])
    update_ _ int new old = case int of
            Changed   _ -> Changed new
            Unchanged _ -> Unchanged $ value old

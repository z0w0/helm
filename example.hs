--license: CC-0
--see: https://creativecommons.org/publicdomain/zero/1.0/

--Elm version: http://share-elm.com/sprout/53ee4e81e4b07afa6f9844c9

import FRP.Helm
import qualified FRP.Helm.Window as Window
import qualified FRP.Helm.Mouse as Mouse
import FRP.Elerea.Simple

data Branch = Branch { branch_x1   :: Double
                     , branch_y1   :: Double
                     , branch_x2   :: Double
                     , branch_y2   :: Double
                     , branch_w    :: Double
                     , branch_a    :: Double
                     , branch_l    :: Int
                     , branch_len  :: Double
                     }

branch :: Branch
       -> (Double -> Double -> Double)
       -> Double
       -> Double
       -> Branch
branch (Branch x1 y1 x2 y2 w a l len) fn delta_a sc = Branch
    { branch_x1 = x2
    , branch_y1 = y2
    , branch_x2 = x2 - len * sc * cos (degrees (fn a delta_a))
    , branch_y2 = y2 - len * sc * sin (degrees (fn a delta_a))
    , branch_w  = w * 0.75
    , branch_a  = fn a delta_a
    , branch_l  = l + 1
    , branch_len = len * sc
    }

drawBranch :: Branch -> Form
drawBranch (Branch x1 y1 x2 y2 w _ _ _) =
    traced  (defaultLine {lineWidth = w, lineColor=red})
    $ path [(x1,y1),(x2,y2)]

branches :: Branch
         -> (Double -> Double -> Double)
         -> Double
         -> Double
         -> [Branch]
branches p@(Branch _ _ _ _ _ _ l _) fn delta_a sc =
    if l < 10
    then let relTrunk = branch p fn delta_a sc
         in relTrunk : (branches relTrunk (+) delta_a sc)
         ++ (branches relTrunk (-) delta_a sc)
    else []

trunk :: Branch
trunk = Branch
    { branch_x1 = 0
    , branch_y1 = 0
    , branch_x2 = 0
    , branch_y2 = -50
    , branch_w = 10
    , branch_a = 90
    , branch_l = 0
    , branch_len = 50
    }

tree :: Double -> Double -> [Branch]
tree delta_a sc =
    (branches trunk (+) delta_a sc)
    ++ [trunk]
    ++ (branches trunk (-) delta_a sc)

fraction1 w eng =
    (\x -> fromIntegral x / fromIntegral w)
    <~ Mouse.x
fraction2 h eng =
    (\y -> 1.0 - fromIntegral y / fromIntegral h)
    <~ Mouse.y

scene frac1 frac2=
    let w = 800
        h = 600
    in centeredCollage w h $ map drawBranch $ tree (90 * frac1) frac2

main :: IO ()
main = do
    engine <- startup defaultConfig
    run engine $ scene
        <~ fraction1 800 engine
        ~~ fraction2 600 engine

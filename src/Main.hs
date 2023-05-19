{-# LANGUAGE ViewPatterns #-}

import System.IO  
import Control.Monad


import Graphics.Gloss
    ( green,
      white,
      black,
      color,
      pictures,
      rectangleSolid,
      scale,
      translate,
      play,
      Display(InWindow),
      Picture (Blank, Text) )

import Graphics.Gloss.Interface.IO.Game
    ( Key(Char, SpecialKey),
      KeyState(Down),
      SpecialKey(KeyEnter, KeyLeft, KeyRight, KeyUp, KeyDown),
      Event(EventKey), playIO)

import qualified Diagrams.TwoD as V
import Linear
    (
        norm,
        normalize
    )

import Diagrams
    (
        unr2
    )

data Bird = Bird{
    pos :: Vec,
    spd :: Vec
}

newtype World = World{
    flock :: Flock
}

newtype Flock = Flock{
    birds :: [Bird]
}


type Vec = V.V2 Float

getx :: Vec -> Float
getx (unr2 -> (x, _)) = x
gety (unr2 -> (_, y)) = y


{-

flocking

-}

fps :: Int
fps = 60

windowW :: Int
windowW = 400

windowH :: Int
windowH = 400

windowWf :: Float
windowWf = fromIntegral windowW

windowHf :: Float
windowHf = fromIntegral windowH

windowTitle :: String
windowTitle = "flocking"

windowPosition :: (Int, Int)
windowPosition = (200, 200)

sort :: (a -> a -> Bool) -> [a] -> [a]
sort _ [] = []
sort f (x:xs) =
    let smaller = sort f (filter (f x) xs)
        bigger  = sort f (filter (not . f x) xs)
    in smaller ++ [x] ++ bigger


--var dx = Math.max(rect.min.x - p.x, 0, p.x - rect.max.x);
--var dy = Math.max(rect.min.y - p.y, 0, p.y - rect.max.y);
--return Math.sqrt(dx*dx + dy*dy);
pointToRect :: Vec -> Float -> Float -> Vec
pointToRect p w h = head $ sort (\a b -> norm a > norm b) [vl, vr, vd, vu]
    where 
        px = getx p
        py = gety p
        vl = p - V.V2 0 py
        vr = V.V2 w py - p
        vd = p - V.V2 px 0
        vu = V.V2 px h - p

main :: IO ()
main = playSim

-- Het Gloss venster
window :: Display
window = InWindow windowTitle (windowW, windowH) windowPosition 

startWorld :: World
startWorld = World{
    flock = Flock{
        birds = [let ifl = fromIntegral i :: Float in Bird{
                pos = V.V2 (ifl * windowWf / 10.0) (ifl * windowHf / 10.0),
                spd = V.V2 (cos (ifl / 10.0 * 3.14)) (sin (ifl / 10.0 * 3.14))
            }
             | i <- [0..9]]
    }
}

render :: World -> IO Picture
render w = return $ translate (-windowWf / 2) (-windowHf / 2) $ pictures [
        renderBird b | b <- birds $ flock w
    ]

renderBird :: Bird -> Picture
renderBird b@Bird{pos = p} = pictures [
        color white $ translate (getx p) (gety p) $ rectangleSolid 10 10,
        color green $ translate (getx towall) (gety towall) $ rectangleSolid 10 10
    ]
    where
        towall = pointToRect p windowWf windowHf

handleInputIO :: Event -> World -> IO World
handleInputIO e w = return w

tick :: Float -> World -> IO World
tick s w@World{flock = f@Flock{birds = birdlist}} = return w{flock = f{birds = [updateBird b | b <- birdlist]}}


updateBird :: Bird -> Bird
updateBird b@Bird{pos = p, spd = s} = b{pos = pos b + spd b, spd = nspd}
    where
        nspd = normalize (s + (steerto * 0.05))
        steerto = normalize $ V.rotateBy (3.14 / 3) $ pointToRect p windowWf windowHf

--start het Gloss-venster op met ingegeven assets en bestandsnamen
playSim :: IO()
playSim = playIO window black fps startWorld render handleInputIO tick
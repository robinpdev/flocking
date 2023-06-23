{-# LANGUAGE ViewPatterns #-}

import System.IO  
import Control.Monad


import Graphics.Gloss
    ( green,
      white,
      black,
      red,
      color,
      pictures,
      rectangleSolid,
      scale,
      translate,
      play,
      line,
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

import GHC.Float

data Bird = Bird{
    bid :: Int,
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

vectup :: Vec -> (Float, Float)
vectup (unr2 -> (x, y)) = (x, y)

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
pointToRect p w h = head (sort (\a b -> norm a > norm b) [vl, vr, vd, vu])
    where 
        px = getx p
        py = gety p
        vl = V.V2 (negate $ getx p) 0
        vr = V.V2 (w - getx p) 0
        vd = V.V2 0 (negate $ gety p)
        vu = V.V2 0 (h - gety p)

main :: IO ()
main = playSim

-- Het Gloss venster
window :: Display
window = InWindow windowTitle (windowW, windowH) windowPosition 

startWorld :: World
startWorld = World{
    flock = Flock{
        birds = [let ifl = fromIntegral i :: Float in Bird{
                bid = i,
                pos = V.V2 (ifl * windowWf / nbirdsf) (ifl * windowHf / nbirdsf),
                spd = V.V2 (cos (ifl / nbirdsf * 3.14)) (sin (ifl / nbirdsf * 3.14))
            }
             | i <- [1..nbirds-1]]
    }
}
    where
        nbirds = 60
        nbirdsf = fromIntegral nbirds

render :: World -> IO Picture
render w = return $ translate (-windowWf / 2) (-windowHf / 2) $ pictures [
        renderBird b allbirds | b <- allbirds
    ]
    where
        allbirds =  birds $ flock w

renderBird :: Bird -> [Bird] -> Picture
renderBird b@Bird{pos = p, spd = s} allbirds = pictures [
        color white $ translate (getx p) (gety p) $ rectangleSolid 10 10,
        color green $ line [ vectup (pos b), vectup $ pos (closestbirdto b allbirds)],
        color red $ line [ vectup p, vectup (p + 16 * s)]
    ]
    where
        towall = p + pointToRect p windowWf windowHf

handleInputIO :: Event -> World -> IO World
handleInputIO e w = return w

tick :: Float -> World -> IO World
tick s w@World{flock = f@Flock{birds = birdlist}} = return w{flock = f{birds = [updateBird b birdlist | b <- birdlist]}}

closestbirdto :: Bird -> [Bird] -> Bird
closestbirdto b birds = head $ sort (\a b -> dist a > dist b) $ filter (\x -> bid x /= bid b) birds
    where
        dist o = norm $ pos o - pos b


updateBird :: Bird -> [Bird] -> Bird
updateBird b@Bird{pos = p, spd = s} allbirds = b{pos = pos b + spd b, spd = nspd}
    where
        nspd = normalize ((normalize toclosestbird * cstrength) + s + strength * normalize steerto)
        strength = realToFrac $ 4 * (1 / max 0.01 (norm steerto))
        cstrength = realToFrac $ 2 * (1 / norm toclosestbird)
        steerto = V.rotateBy (3.14 / 2) $ pointToRect p windowWf windowHf
        toclosestbird = V.rotateBy(3.14 / 2.5) (pos b - pos (closestbirdto b allbirds))

--start het Gloss-venster op met ingegeven assets en bestandsnamen
playSim :: IO()
playSim = playIO window black fps startWorld render handleInputIO tick

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

import Linear
import Control.Lens

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

type Vec = V2 Float

getx = view _x
gety = view _y


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

main :: IO ()
main = playSim

-- Het Gloss venster
window :: Display
window = InWindow windowTitle (windowW, windowH) windowPosition 

startWorld :: World
startWorld = World{
    flock = Flock{
        birds = [let ifl = fromIntegral i :: Float in Bird{
                pos = V2 (ifl * windowWf / 10.0) (ifl * windowHf / 10.0),
                spd = V2 (cos (ifl / 10.0 * 3.14)) (sin (ifl / 10.0 * 3.14))
            }
             | i <- [0..9]]
    }
}

render :: World -> IO Picture
render w = return $ translate (-windowWf / 2) (-windowHf / 2) $ pictures [
        renderBird b | b <- birds $ flock w
    ]

renderBird :: Bird -> Picture
renderBird b@Bird{pos = p} = color white $ translate (getx p) (gety p) $ rectangleSolid 10 10

handleInputIO :: Event -> World -> IO World
handleInputIO e w = return w

tick :: Float -> World -> IO World
tick s w@World{flock = f@Flock{birds = birdlist}} = return w{flock = f{birds = [updateBird b | b <- birdlist]}}


updateBird :: Bird -> Bird
updateBird b = b{pos = pos b + spd b}

--start het Gloss-venster op met ingegeven assets en bestandsnamen
playSim :: IO()
playSim = playIO window black fps startWorld render handleInputIO tick
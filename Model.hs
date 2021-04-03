module Model where
import Data.Matrix ( Matrix, matrix )
import Graphics.Gloss (Color)
import Data.Bifunctor (bimap, first, second)
import Control.Monad  (join)
import Graphics.Gloss.Data.Color

data RenderOptions = RenderOptions
  { pixelSize :: Int
  , offset :: (Float,Float)
  , canvasSize :: (Int,Int)
  } deriving (Show)

data InputState = InputState
  { moveDirection :: (Int, Int)
  , isPainting :: Bool
  } deriving (Show)

data AppState = AppState
  { painting :: Matrix Color
  , renderOptions :: RenderOptions
  , cursor :: (Int,Int)
  , inputState :: InputState
  } deriving (Show)

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple = join bimap
mapI :: (a -> Int -> b) -> [a] -> [b]
mapI f l = zipWith f l [0..]

initialState :: AppState
initialState = AppState
  { painting = matrix m n (const yellow)
  , cursor = mapTuple (`div` 2)  (m,n)
  , inputState = InputState { moveDirection = (0,0), isPainting = False}
  , renderOptions = RenderOptions
    { pixelSize = pxSize
    , offset = baseOffset
    , canvasSize = mapTuple (+100) paintingSize
    }
  }
  where
    m = 50
    n = 50
    pxSize = 10
    paintingSize = (m*pxSize,n*pxSize)
    baseOffset = mapTuple ((* 0.5) . fromIntegral)  paintingSize

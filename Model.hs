module Model where
import Data.Matrix ( Matrix, matrix )
import Graphics.Gloss (Color)
import Data.Bifunctor (bimap)
import Control.Monad  (join)
import Graphics.Gloss.Data.Color

data RenderOptions = RenderOptions
  { pixelSize :: Int
  , offset :: (Float,Float)
  , canvasSize :: (Int,Int)
  } deriving (Show)


data InsertMode = InsertMode
  { moveDirection :: (Int, Int)
  , isPainting :: Bool
  } deriving (Show)
newtype ExMode = ExMode String
  deriving (Show)

data InputState =
   InputInsert InsertMode
  | InputEx ExMode
  deriving (Show)

data AppState = AppState
  { painting :: Matrix Color
  , renderOptions :: RenderOptions
  , cursor :: (Int,Int)
  , inputState :: InputState
  } deriving (Show)

-- blackbird operator, compose with 2 args = \f g x y -> f(g x y)
(...) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
(...) = (.) . (.)

mapTuple :: (a->b) -> (a,a) -> (b,b)
mapTuple = join bimap
applyTuple :: (t1 -> t2 -> b) -> (t1, t1) -> (t2, t2) -> (b, b)
applyTuple f (x1,x2) (y1,y2) = (f x1 y1, f x2 y2)

mapI :: (a -> Int -> b) -> [a] -> [b]
mapI f l = zipWith f l [0..]

defaultInput :: InputState
defaultInput = InputInsert InsertMode
  { moveDirection = (0,0)
  , isPainting = False
  }

paintingWidth = 50;
paintingHeight = 50;

initialState :: AppState
initialState = AppState
  { painting = matrix m n (const $ dim white)
  , cursor = mapTuple (`div` 2)  (m,n)
  , inputState = defaultInput
  , renderOptions = RenderOptions
    { pixelSize = pxSize
    , offset = baseOffset
    , canvasSize = mapTuple (+100) paintingSize
    }
  }
  where
    m = paintingWidth
    n = paintingHeight
    pxSize = 10
    paintingSize = (m*pxSize,n*pxSize)
    baseOffset = mapTuple ((* 0.5) . fromIntegral)  paintingSize

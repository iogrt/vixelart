module Render (renderFn, paintPixels) where
import Model
import Graphics.Gloss.Data.Color
import Data.Matrix
import Graphics.Gloss.Data.Point (Path)
import Graphics.Gloss.Data.Picture 

renderFn :: AppState -> Picture
renderFn AppState {renderOptions = renderOpts, painting=px, cursor=cursorPos}  =
  -- order gives z axis
  pictures (pixelsPicture ++ [cursorPicture])
  where
    w = ncols px
    pixelsPicture = mapI (\color listIndex ->
                    let (i,j) = quotRem listIndex w in
                      drawPixel renderOpts (i,j) color
                  )(toList px)
    cursorPicture = lineLoop $ getRectPath renderOpts cursorPos

pixelToPoint :: RenderOptions -> (Int,Int) -> (Float,Float)
pixelToPoint RenderOptions { pixelSize=size, offset=(offsetX,offsetY) } (i,j) =
  -- matrix is 1 based index, remove 1
  ( fromIntegral ((i-1) * size) - offsetX
  , fromIntegral ((j-1) * size) - offsetY
  )

pointToPixel :: RenderOptions -> (Float,Float) -> (Int,Int)
pointToPixel RenderOptions { pixelSize=size, offset=(offsetX,offsetY) } (x,y) =
  -- matrix is 1 based index, add 1
  (ceiling $ (x + offsetX + 1 ) / fromIntegral size
  ,ceiling $ (y + offsetY + 1)  / fromIntegral size
  )

getRectPath :: RenderOptions -> (Int,Int) -> Path
getRectPath renderOpts (i,j) =
  map (\(a,b) -> pixelToPoint renderOpts (a+i,b+j)
       )
    rectangle
  where
    rectangle = [(0,0),(0,1),(1,1),(1,0)]

drawPixel :: RenderOptions -> (Int,Int) -> Color -> Picture
drawPixel renderOptions (i,j)  color =
  Color color $ Polygon $ getRectPath renderOptions (i,j)

paintPixels :: Color -> [(Int,Int)] -> Matrix Color -> Matrix Color
paintPixels color posList matrix =
  foldr setPixel matrix posList
  where
    -- matrix is 1 based index
    setPixel pos = setElem color (mapTuple (+1) pos)

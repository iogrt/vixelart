{-# LANGUAGE NamedFieldPuns #-}
module Render (renderFn, paintPixels, pointToPixel) where
import Model
import Graphics.Gloss.Data.Color
import Data.Matrix
import Graphics.Gloss.Data.Point (Path)
import Graphics.Gloss.Data.Picture

renderFn :: AppState -> Picture
renderFn AppState
  { renderOptions = renderOpts,
    painting=px,
    cursor=cursorPos,
    inputState
  }  =
  -- order gives z axis
  pictures (pixelsPicture ++ [cursorPicture, exModePrompt])
  where
    pixelsPicture = mapI (\color listIndex ->
                    let (i,j) = quotRem listIndex (ncols px) in
                      drawPixel color renderOpts (i,j) 
                  )(toList px)

    cursorPicture = lineLoop $ getRectPath renderOpts $
      -- 1 because matrix is 1 based
      mapTuple (subtract 1) cursorPos
    exModePrompt =
      case inputState of
        InputEx (ExMode msg) ->
          uncurry Translate (mapTuple (* (-1)) $ offset renderOpts)
          $ Scale 0.1 0.1
          $ Text (":" ++ msg)
        _ -> Blank

pixelToPoint :: RenderOptions -> (Int,Int) -> (Float,Float)
pixelToPoint RenderOptions { pixelSize, offset } =
  -- matrix is 1 based index, remove 1
  applyTuple subtract offset . mapTuple (fromIntegral.(* pixelSize).subtract 1)

pointToPixel :: RenderOptions -> (Float,Float) -> (Int,Int)
pointToPixel RenderOptions { pixelSize=size, offset } =
  mapTuple (ceiling.(/ intSize).(+1)) . applyTuple (+) offset
  where
    intSize = fromIntegral size

getRectPath :: RenderOptions -> (Int,Int) -> Path
getRectPath renderOpts (i,j) =
  map (\(a,b) -> pixelToPoint renderOpts (a+i,b+j)
       )
    rectangle
  where
    rectangle = [(0,0),(0,1),(1,1),(1,0)]

drawPixel :: Color -> RenderOptions -> (Int,Int) ->  Picture
drawPixel c =
  Color c ... (Polygon ... getRectPath)

paintPixels :: Color -> Matrix Color -> [(Int,Int)] ->  Matrix Color
paintPixels =
  foldr . setElem

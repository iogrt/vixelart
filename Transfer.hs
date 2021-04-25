module Transfer where
import Data.Matrix ( Matrix, toList, fromList)
import Graphics.Gloss ( makeColor, rgbaOfColor, Color )
import Data.ByteString (pack, unpack, ByteString)

exportPainting :: Matrix Color -> ByteString
exportPainting m =
  pack $ map (ceiling.(*255))
  $ foldr ((\(r,g,b,_) l -> r : g : b : l) . rgbaOfColor) [] (toList m)

importPainting :: Int -> Int -> ByteString -> Matrix Color
importPainting n m  =
  fromList n m . groupColor . map fromIntegral . unpack
  where
    groupColor (r:g:b:l) = makeColor r g b 1 : groupColor l
    groupColor _ = []


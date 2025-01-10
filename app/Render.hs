module Render(Window,defaultWindow,samples,render, renderWithColor) where
import Codec.Picture
import Shape
import qualified Data.Map as Map
import Data.Word (Word8)




--  A window specifies what part of the world to render and at which
--  resolution.
--  Values are top left & bottom right corner to be rendered,
--             and the size of the output device to render into
data Window = Window Point Point (Int,Int)

-- Default window renders a small region around the origin into
-- a 100x100 pixel image
-- Note the comment below on the 'render' function. If you increase
-- the size of the output image to around 500 by 500 you'll see what
-- I mean by inefficient
defaultWindow :: Window
defaultWindow = Window (point (-10) (-10)) (point 10 10) (480,480)


-- Generate a list of evenly spaced samples between two values.
-- e.g. samples -1.5 +1.5 25 generates 25 samples evenly spaced
--      between the two bounds: [ -1.5, -1.375, -1.25 ... ]
samples :: Double -> Double -> Int -> [Double]
samples c0 c1 n = take n [ c0, c0 + (c1-c0) / fromIntegral (n-1) .. ]

-- Generate the matrix of points corresponding to the pixels of a window.
pixels :: Window -> [[Point]]
pixels (Window p0 p1 (w,h)) =
  [ [ point x y | x <- samples (getX p0) (getX p1) w ]
                | y <- reverse $ samples (getY p0) (getY p1) h
  ]

-- generate list of all screen coordinates in window
coords :: Window -> [[(Int,Int)]]
coords (Window _ _ (w,h)) = [ [(x,y) | x <- [0..w]] | y <- [0..h] ]

-- render a drawing into an image, then save into a file
-- NB: the lookup1 function is a VERY inefficient way to convert screen coordinates to drawing
--     coordinates! It should be possible to do this in O(1) time, not O(N) time!!
--     If you enlarge the viewport in defaultWindow from 50x50 to 500x500 then you will see the problem.
render :: String -> Window -> Drawing -> IO ()
render path win sh = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win

      pixRenderer x y = PixelRGB8 c c c where c = colorForImage $ mapPoint win (x,y)

      mapPoint :: Window -> (Int,Int) -> Point
      mapPoint _ p = lookup1 p locations

      -- Maps are more efficient for lookups than lists
      lookup1 :: (Int, Int) -> Map.Map (Int, Int) Point -> Point
      lookup1 a m = Map.findWithDefault (point 0 0) a m

      locations :: Map.Map (Int, Int) Point
      locations = Map.fromList $ concat $ zipWith zip (coords win) (pixels win)
      
      colorForImage p | p `inside` sh = 255
                      | otherwise     = 0

-- New render function with color 
renderWithColor :: String -> Window -> ColorDrawing -> IO ()
renderWithColor path win drawing = writePng path $ generateImage pixRenderer w h
    where
      Window _ _ (w,h) = win
      
      pixelMap :: Map.Map (Int, Int) Point
      pixelMap = Map.fromList $ concat $ zipWith zip (coords win) (pixels win)

      lookupPoint :: (Int, Int) -> Point
      lookupPoint p = Map.findWithDefault (point 0 0) p pixelMap

      toPixelRGBA8 :: Color -> PixelRGBA8
      toPixelRGBA8 col = PixelRGBA8 (r col) (g col) (b col) (a col)

      pixRenderer x y = toPixelRGBA8 $ colorPixel (lookupPoint (x, y)) drawing
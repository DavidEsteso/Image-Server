module Shape where

import Data.Word (Word8)

import Debug.Trace (trace)

import System.IO (appendFile)
import System.IO.Unsafe (unsafePerformIO)



-- Utilities

data Vector = Vector Double Double
    deriving (Show, Eq)
vector = Vector

cross :: Vector -> Vector -> Double
cross (Vector a b) (Vector a' b') = a * a' + b * b'

mult :: Matrix -> Vector -> Vector
mult (Matrix r0 r1) v = Vector (cross r0 v) (cross r1 v)

invert :: Matrix -> Matrix
invert (Matrix (Vector a b) (Vector c d)) = matrix (d / k) (-b / k) (-c / k) (a / k)
  where k = a * d - b * c

-- 2x2 square matrices are all we need.
data Matrix = Matrix Vector Vector
              deriving Show

matrix :: Double -> Double -> Double -> Double -> Matrix
matrix a b c d = Matrix (Vector a b) (Vector c d)

getX (Vector x y) = x
getY (Vector x y) = y



type Point  = Vector

point :: Double -> Double -> Point
point = vector

data Color = Color {
  r :: Word8,
  g :: Word8,
  b :: Word8,
  a :: Word8
} deriving Show


data ColorType
    = Solid Color       
    | Gradient Color Color 
    deriving Show

transparent = Color 255 255 255 0
black = Color 0 0 0 255
white = Color 255 255 255 255
red = Color 255 0 0 255
orange = Color 255 140 0 255
yellow = Color 255 255 0 255
green = Color 0 255 0 255
cyan = Color 0 255 255 255
blue = Color 0 0 255 255
purple = Color 128 0 128 255
magenta = Color 255 0 255 255
pink = Color 255 192 203 255
brown = Color 139 69 19 255
gray = Color 169 169 169 255
lightGray = Color 211 211 211 255
darkGray = Color 169 169 169 255
beige = Color 245 245 220 255
lightBlue = Color 173 216 230 255
lightGreen = Color 144 238 144 255
lightYellow = Color 255 255 224 255
lightPink = Color 255 182 193 255
peach = Color 255 218 185 255
lavender = Color 230 230 250 255
mint = Color 189 252 201 255
coral = Color 255 127 80 255
teal = Color 0 128 128 255
indigo = Color 75 0 130 255
ivory = Color 255 255 240 255
charcoal = Color 54 69 79 255
gold = Color 255 215 0 255
silver = Color 192 192 192 255
platinum = Color 229 228 226 255



interpolateColor :: Color -> Color -> Double -> Double -> Double -> Color
interpolateColor (Color r1 g1 b1 a1) (Color r2 g2 b2 a2) minY maxY y =
  let t = (y - minY) / (maxY - minY)  
      r = round $ fromIntegral r1 * (1 - t) + fromIntegral r2 * t
      g = round $ fromIntegral g1 * (1 - t) + fromIntegral g2 * t
      b = round $ fromIntegral b1 * (1 - t) + fromIntegral b2 * t
      a = round $ fromIntegral a1 * (1 - t) + fromIntegral a2 * t
  in Color r g b a

-- Shapes

data Shape 
    = Empty
    | Circle
    | Square
    | Rectangle Double Double
    | Ellipse Double Double
    | ConvexPolygon [Point]
    deriving (Show, Eq)

empty :: Shape
empty = Empty

circle :: Shape
circle = Circle

square :: Shape
square = Square

rectangle :: Double -> Double -> Shape
rectangle = Rectangle

ellipse :: Double -> Double -> Shape
ellipse = Ellipse

convexPolygon :: [Point] -> Shape
convexPolygon = ConvexPolygon


-- Transformations

data Transform = Identity
           | Translate Vector
           | Scale Vector
           | Compose Transform Transform
           | Rotate Matrix
           | Shear Vector
             deriving Show

identity = Identity
translate = Translate
scale = Scale
rotate angle = Rotate $ matrix (cos angle) (-sin angle) (sin angle) (cos angle)
shear = Shear
t0 <+> t1 = Compose t0 t1

transform :: Transform -> Point -> Point
transform Identity                   x = x
transform (Translate (Vector tx ty)) (Vector px py)  = Vector (px - tx) (py - ty)
transform (Scale (Vector tx ty))     (Vector px py)  = Vector (px / tx)  (py / ty)
transform (Rotate m)                 p = invert m `mult` p
transform (Compose t1 t2)            p = transform t2 $ transform t1 p
transform (Shear (Vector sx sy)) (Vector px py) = Vector (px + sy*py) (py + sx*px)

inverseTransform :: Transform -> Transform
inverseTransform Identity = Identity
inverseTransform (Translate (Vector tx ty)) = Translate (Vector (-tx) (-ty))
inverseTransform (Scale (Vector sx sy)) = Scale (Vector (1 / sx) (1 / sy))
inverseTransform (Rotate m) = Rotate (invert m)
inverseTransform (Shear (Vector sx sy)) = Shear (Vector (-sx) (-sy))
inverseTransform (Compose t1 t2) = Compose (inverseTransform t2) (inverseTransform t1)



--BoundingBox

-- Approximate shapes
approximateCircle :: Int  -> [Vector]
approximateCircle n = [Vector (1 * cos (2 * pi * i / fromIntegral n)) (1 * sin (2 * pi * i / fromIntegral n)) | i <- [0..fromIntegral n]]

approximateEllipse :: Double -> Double -> Int -> [Vector]
approximateEllipse semiMajor semiMinor n = [Vector (semiMajor * cos (2 * pi * i / fromIntegral n)) (semiMinor * sin (2 * pi * i / fromIntegral n)) | i <- [0..fromIntegral n]]

-- Get bounding box of a shape after applying a transformation
transformAndGetBoundingBox :: Transform -> Shape -> (Point, Point)
transformAndGetBoundingBox t Empty = (Vector 0 0, Vector 0 0)

transformAndGetBoundingBox t shape = 
    let points = case shape of
            Circle -> approximateCircle 15
            Square -> let halfSide = 1 in [Vector (-halfSide) (-halfSide), Vector halfSide (-halfSide), Vector halfSide halfSide, Vector (-halfSide) halfSide]
            Rectangle width height -> let halfWidth = width / 2; halfHeight = height / 2 in [Vector (-halfWidth) (-halfHeight), Vector halfWidth (-halfHeight), Vector halfWidth halfHeight, Vector (-halfWidth) halfHeight]
            Ellipse semiMajor semiMinor -> approximateEllipse semiMajor semiMinor 15
            ConvexPolygon pts -> pts
        transformedPoints = map (inverseTransform t `transform`) points
        (xs, ys) = (map getX transformedPoints, map getY transformedPoints)
        (minX, maxX, minY, maxY) = (minimum xs, maximum xs, minimum ys, maximum ys)
    in (Vector minX minY, Vector maxX maxY)


-- Get bounding box of a drawing
getBoundingBox :: ColorDrawing -> (Point, Point)
getBoundingBox (SingleDrawing (t, s, c)) = 
  let bbox = transformAndGetBoundingBox t s
  in bbox

getBoundingBox (Combine drawing1 op drawing2) = 
    let (p1, p2) = getBoundingBox drawing1
        (p1', p2') = getBoundingBox drawing2
        center1 = getCenter p1 p2
        center2 = getCenter p1' p2'
        width1 = getX p2 - getX p1
        width2 = getX p2' - getX p1'
        height1 = getY p2 - getY p1
        height2 = getY p2' - getY p1'

        -- Calculate new points for the bounding box of the combined drawing based on the operation 
        (newP1X, newP1Y, newP2X, newP2Y) = case op of
            LeftOf -> 
                (getX p1 - width2 / 2 - width1 / 2 + getX center2, getY p1 + getY center2,
                getX p2 - width2 / 2 - width1 / 2 + getX center2, getY p2 + getY center2)
            RightOf -> 
                (getX p1 + width2 / 2 + width1 / 2 + getX center2, getY p1 + getY center2,
                getX p2 + width2 / 2 + width1 / 2 + getX center2, getY p2 + getY center2)
            Over -> 
                (getX p1 + getX center2, getY p1 + getY center2,
                getX p2 + getX center2, getY p2 + getY center2)
            Below -> 
                (getX p1 + getX center2, getY p1 - height2 / 2 - height1 / 2 + getY center2,
                getX p2 + getX center2, getY p2 - height2 / 2 - height1 / 2 + getY center2)
            Above -> 
                (getX p1 + getX center2, getY p1 + height2 / 2 + height1 / 2 + getY center2,
                getX p2 + getX center2, getY p2 + height2 / 2 + height1 / 2 + getY center2)


        newP1 = Vector newP1X newP1Y
        newP2 = Vector newP2X newP2Y

        minX = min (getX newP1) (getX p1')
        minY = min (getY newP1) (getY p1')
        maxX = max (getX newP2) (getX p2')
        maxY = max (getY newP2) (getY p2')

    in (Vector minX minY, Vector maxX maxY)


-- Get center of a bounding box
getCenter :: Point -> Point -> Point  
getCenter (Vector x1 y1) (Vector x2 y2) = 
  let x = (x1 + x2) / 2
      y = (y1 + y2) / 2
  in Vector x y


-- Drawings

type Drawing = [(Transform,Shape)]

data Operation = Over | LeftOf | RightOf | Above | Below


data ColorDrawing 
    = SingleDrawing (Transform, Shape, ColorType)
    | Combine ColorDrawing Operation ColorDrawing


-- Check if a drawing is a single drawing
isSingleDrawing :: ColorDrawing -> Bool
isSingleDrawing (SingleDrawing _) = True
isSingleDrawing _ = False


-- Get pixel where the shape has to be drawn
getPixel :: Point -> ColorDrawing -> Double -> Operation -> Vector
getPixel position drawing radius fatherOp =
    let point = getPoint drawing radius fatherOp
    in transform (Translate point) position


-- Get point deppending on the operation
getPoint :: ColorDrawing -> Double -> Operation -> Vector
getPoint drawing radius fatherOp =
    let boundingBox = getBoundingBox drawing
        center = getCenter (fst boundingBox) (snd boundingBox)
        point = case fatherOp of
            LeftOf -> Vector (getX (fst boundingBox) - radius) (getY center)
            RightOf -> Vector (getX (snd boundingBox) + radius) (getY center)
            Above -> Vector (getX center) (getY (snd boundingBox) + radius)
            Below -> Vector (getX center) (getY (fst boundingBox) - radius)
            Over -> Vector (getX center) (getY center)
    in point



-- Computes the color of a pixel 
colorPixel :: Point -> ColorDrawing -> Color
colorPixel position (SingleDrawing (transform, shape, Solid color)) = 
  let isInsideShape = inside1 position (transform, shape)
  in if isInsideShape then color else transparent


colorPixel position (SingleDrawing (t, shape, Gradient color1 color2)) =
  let isInsideShape = inside1 position (t, shape)
      transformedPoint = transform t position
      boundingBox = transformAndGetBoundingBox identity shape
      minY = getY (fst boundingBox)
      maxY = getY (snd boundingBox)
      height = maxY - minY
      gradientColor = if isInsideShape 
                      then interpolateColor color1 color2 minY maxY (getY transformedPoint)
                      else transparent
  in if isInsideShape then gradientColor else transparent
  

colorPixel position drawing = 
    case drawing of
        Combine d1 op d2 -> 
            if isSingleDrawing d1
            then case op of
                Over -> colorPixelWithOp position drawing
                LeftOf -> colorPixelWithOp position drawing
                RightOf -> colorPixelWithOp position drawing
                Above -> colorPixelWithOp position drawing
                Below -> colorPixelWithOp position drawing
                _ -> error "Only use Over, LeftOf, RightOf, Above or Below"
            else error "You can only draw a SingleDrawing Over, LeftOf, RightOf, Above or Below a SingleDrawing or a Combine"


-- Computes the color of a pixel with an operation
colorPixelWithOp :: Point -> ColorDrawing -> Color
colorPixelWithOp position (Combine drawing1 op drawing2) =
    let boundingBox1 = getBoundingBox drawing1
        center = getCenter (fst boundingBox1) (snd boundingBox1)
        radius = case op of
            LeftOf -> (abs (getX (fst boundingBox1) - getX center))
            RightOf -> (abs (getX (fst boundingBox1) - getX center))
            Above -> (abs (getY (fst boundingBox1) - getY center))
            Below -> (abs (getY (fst boundingBox1) - getY center))
        pos2 = case op of
            LeftOf -> getPixel position drawing2 radius op
            RightOf -> getPixel position drawing2 radius op
            Above -> getPixel position drawing2 radius op
            Below -> getPixel position drawing2 radius op
            Over -> getPixel position drawing2 0.0 op
        color2 = colorPixel position drawing2
        color1 = colorPixel pos2 drawing1
    in if a color1 == 0
       then color2
       else color1


inside :: Point -> Drawing -> Bool
inside p d = any (inside1 p) d

inside1 :: Point -> (Transform, Shape) -> Bool
inside1 p (t,s) = insides (transform t p) s

insides :: Point -> Shape -> Bool
p `insides` Empty = False
p `insides` Circle = distance p <= 1
p `insides` Square = maxnorm  p <= 1
p `insides` Rectangle w h = abs (getX p) <= w / 2 && abs (getY p) <= h / 2
p `insides` Ellipse semiMajor semiMinor = (getX p / semiMajor)**2 + (getY p / semiMinor)**2 <= 1
insides _ (ConvexPolygon []) = False  
insides _ (ConvexPolygon [_]) = False 
insides _ (ConvexPolygon [_ , _]) = False  
insides point (ConvexPolygon (p1:p2:ps)) = checkEdges point p1 p2 ps

-- Check if a point is inside a convex polygon
checkEdges :: Vector -> Vector -> Vector -> [Vector] -> Bool
checkEdges point p1 p2 [] = halfLine point p1 p2  
checkEdges point p1 p2 (p3:ps) =
    if halfLine point p1 p2
    then checkEdges point p2 p3 ps  
    else False  

-- Check if a point is on the left side of a half line
halfLine :: Vector -> Vector -> Vector -> Bool
halfLine (Vector x y) (Vector x1 y1) (Vector x2 y2) =
    zcross (Vector (x1 - x) (y1 - y)) (Vector (x2 - x1) (y2 - y1)) < 0
    where zcross (Vector a b) (Vector c d) = a * d - b * c


distance :: Point -> Double
distance (Vector x y ) = sqrt ( x**2 + y**2 )

maxnorm :: Point -> Double
maxnorm (Vector x y ) = max (abs x) (abs y)


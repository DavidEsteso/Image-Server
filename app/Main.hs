{-# LANGUAGE OverloadedStrings #-}
import Data.Text.Lazy
import qualified Web.Scotty as Scotty
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Html.Renderer.Text

import Shape
import Render
import Control.Parallel (par, pseq)
import Control.Concurrent.Async (mapConcurrently)
import Control.Monad.IO.Class (liftIO)

main :: IO ()
main = do
    -- Render all illustrations concurrently
    _ <- mapConcurrently (\(path, window, illustration) -> 
            renderWithColor path window illustration)
        [ ("./images/greenRect.png", defaultWindow, greenRectangle)
        , ("./images/blueCircle.png", defaultWindow, blueCircle)
        , ("./images/redSquare.png", defaultWindow, redSquare)
        , ("./images/pinkSquare.png", defaultWindow, pinkSquare)
        , ("./images/yellowCircle.png", defaultWindow, yellowCircle)
        , ("./images/bluePentagon.png", defaultWindow, bluePentagon)
        , ("./images/orangeElipse.png", defaultWindow, orangeElipse)
        , ("./images/movedBlueCircle.png", defaultWindow, movedBlueCircle)
        , ("./images/movedRedSquare.png", defaultWindow, movedRedSquare)
        , ("./images/first.png", defaultWindow, firstDeliverable)
        , ("./images/shear.png", defaultWindow, shearIlustration)
        , ("./images/base.png", defaultWindow, baseDrawing)
        , ("./images/leftBase.png", defaultWindow, leftBaseDrawing)
        , ("./images/rightBase.png", defaultWindow, rightBaseDrawing)
        , ("./images/topBase.png", defaultWindow, topBaseDrawing)
        , ("./images/bottomBase.png", defaultWindow, bottomBaseDrawing)
        , ("./images/overBase.png", defaultWindow, overBaseDrawing) 
        , ("./images/movedComboLeft.png", defaultWindow, movedComboLeft)
        , ("./images/basicComboLeft.png", defaultWindow, basicComboLeft)
        , ("./images/basicComboRight.png", defaultWindow, basicComboRight)
        , ("./images/basicComboTop.png", defaultWindow, basicComboTop)
        , ("./images/basicComboBottom.png", defaultWindow, basicComboBottom)
        , ("./images/basicComboOver.png", defaultWindow, basicComboOver) 
        , ("./images/combo1.png", defaultWindow, combo1)
        ]
    Scotty.scotty 3000 $ do
        Scotty.get "/" $ do
            Scotty.html response
        Scotty.get "/images/:image" $ do 
            image <- Scotty.param "image"
            Scotty.file ("./images/" ++ image ++ ".png")

response :: Text
response = renderHtml $ do
    Text.Blaze.Html5.style $ mconcat
        [ ".content { max-width: 1200px; margin: auto; text-align: center; }"
        , ".section { margin: 40px 0; }"
        , ".shapes-container { display: flex; flex-wrap: wrap; justify-content: center; gap: 20px; margin: 20px 0; }"
        , ".shape-item { flex: 1; min-width: 250px; max-width: 400px; margin: 10px; }"
        , ".shape-item img { width: 100%; height: auto; }"
        , ".shape-item p { font-size: 0.9em; margin-top: 10px; text-align: left; }"
        , ".section-title { width: 100%; text-align: center; margin: 30px 0; padding: 10px; background: #f5f5f5; }"
        , ".section-intro { text-align: center; margin: 20px 0; font-style: italic; }"
        , ".code-block { text-align: left; background: #f8f8f8; padding: 15px; margin: 20px 0; border-radius: 5px; }"
        ]
    Text.Blaze.Html5.div ! class_ "content" $ do
        h1 "Shape Server"

        -- Basic Shapes Section
        Text.Blaze.Html5.div ! class_ "section" $ do
            h2 ! class_ "section-title" $ "Shapes that we are going to use"
            Text.Blaze.Html5.div ! class_ "code-block" $ do
                p "greenRectangle = SingleDrawing (scale (point 2 2) <+> shear (point 0.5 0) <+> shear (point 0 0.5), rectangle 1 2, Gradient black green)"
                p "blueCircle = SingleDrawing (scale (point 2 2) <+> shear (point 0.5 0), circle, Solid blue)"
                p "redSquare = SingleDrawing (scale (point 2 2) <+> shear (point 0 0.5), square, Solid red)"
                p "pinkSquare = SingleDrawing (scale (point 1 1), square, Solid pink)"
                p "yellowCircle = SingleDrawing (scale (point 1 1) <+> rotate (-10), circle, Gradient yellow blue)"
                p "bluePentagon = SingleDrawing (scale (point 1 1), convexPolygon [point 0 3, point 2 1, point 1 (-2), point (-1) (-2), point (-2) 1, point 0 3], Gradient red blue)"
                p "orangeElipse = SingleDrawing (scale (point 1 1), ellipse 3 7, Solid orange)"
                p "movedBlueCircle = SingleDrawing (translate (point 0 2), circle, Solid blue)"
                p "movedRedSquare = SingleDrawing (translate (point 2 0), square, Solid red)"
            Text.Blaze.Html5.div ! class_ "shapes-container" $ do
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/greenRect"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/blueCircle"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/redSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/pinkSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/yellowCircle"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/bluePentagon"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/orangeElipse"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/movedBlueCircle"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/movedRedSquare"

        -- First Deliverables Section
        Text.Blaze.Html5.div ! class_ "section" $ do
            h2 ! class_ "section-title" $ "First Deliverables"
            p ! class_ "section-intro" $ "New shapes, operations and colors"
            Text.Blaze.Html5.div ! class_ "shapes-container" $ do
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/first"
                    p "firstDeliverable = Combine greenRectangle LeftOf (Combine bluePentagon RightOf orangeElipse)"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/shear"
                    p "shearIlustration = Combine greenRectangle RightOf (Combine redSquare LeftOf blueCircle)"

        -- Shapes Combined with Drawing Section
        Text.Blaze.Html5.div ! class_ "section" $ do
            h2 ! class_ "section-title" $ "Shapes Combined with Drawing"
            Text.Blaze.Html5.div ! class_ "shapes-container" $ do
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/base"
                    p "baseDrawing = Combine yellowCircle Over bluePentagon"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/leftBase"
                    p "leftBaseDrawing = Combine pinkSquare LeftOf baseDrawing"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/rightBase"
                    p "rightBaseDrawing = Combine pinkSquare RightOf baseDrawing"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/topBase"
                    p "topBaseDrawing = Combine yellowCircle Above baseDrawing"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/bottomBase"
                    p "bottomBaseDrawing = Combine yellowCircle Below baseDrawing"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/overBase"
                    p "overBaseDrawing = Combine pinkSquare Over baseDrawing"

        -- Shapes Combined with Shape Section
        Text.Blaze.Html5.div ! class_ "section" $ do
            h2 ! class_ "section-title" $ "Shapes Combined with Shape"
            Text.Blaze.Html5.div ! class_ "shapes-container" $ do
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/movedComboLeft"
                    p "movedComboLeft = Combine movedBlueCircle LeftOf movedRedSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/basicComboLeft"
                    p "basicComboLeft = Combine blueCircle LeftOf redSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/basicComboRight"
                    p "basicComboRight = Combine blueCircle RightOf redSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/basicComboTop"
                    p "basicComboTop = Combine blueCircle Above redSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/basicComboBottom"
                    p "basicComboBottom = Combine blueCircle Below redSquare"
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/basicComboOver"
                    p "basicComboOver = Combine blueCircle Over redSquare"

        -- Combos Section
        Text.Blaze.Html5.div ! class_ "section" $ do
            h2 ! class_ "section-title" $ "Complex combos"
            Text.Blaze.Html5.div ! class_ "shapes-container" $ do
                Text.Blaze.Html5.div ! class_ "shape-item" $ do
                    img ! src "images/combo1"
                    p "combo1 = Combine redSquare Below (Combine yellowCircle Above (Combine pinkSquare LeftOf ( Combine greenRectangle RightOf ( Combine yellowCircle Over bluePentagon )) ) )"

-- Shape definitions remain the same
greenRectangle = SingleDrawing (scale (point 2 2) <+> shear (point 0.5 0) <+> shear (point 0 0.5), rectangle 1 2, Gradient black green)
blueCircle = SingleDrawing (scale (point 2 2) <+> shear (point 0.5 0), circle, Solid blue)
redSquare = SingleDrawing (scale (point 2 2) <+> shear (point 0 0.5), square, Solid red)
pinkSquare = SingleDrawing (scale (point 1 1), square, Solid pink) 
yellowCircle = SingleDrawing (scale (point 1 1) <+> rotate (-10), circle, Gradient yellow blue) 
bluePentagon = SingleDrawing (scale (point 1 1), convexPolygon [point 0 3, point 2 1, point 1 (-2), point (-1) (-2), point (-2) 1, point 0 3], Gradient red blue)
orangeElipse = SingleDrawing (scale (point 1 1), ellipse 3 7, Solid orange)
movedBlueCircle = SingleDrawing (translate (point 0 2), circle, Solid blue)
movedRedSquare = SingleDrawing (translate (point 2 0), square, Solid red)

-- First deliverables
firstDeliverable = Combine greenRectangle LeftOf (Combine bluePentagon RightOf orangeElipse)
shearIlustration = Combine greenRectangle RightOf (Combine redSquare LeftOf blueCircle)

-- Shapes combined with drawing
baseDrawing = Combine yellowCircle Over bluePentagon
leftBaseDrawing = Combine pinkSquare LeftOf baseDrawing
rightBaseDrawing = Combine pinkSquare RightOf baseDrawing
topBaseDrawing = Combine yellowCircle Above baseDrawing
bottomBaseDrawing = Combine yellowCircle Below baseDrawing
overBaseDrawing = Combine pinkSquare Over baseDrawing  

-- Shapes combined with shape
movedComboLeft = Combine movedBlueCircle LeftOf movedRedSquare
basicComboLeft = Combine blueCircle LeftOf redSquare
basicComboRight = Combine blueCircle RightOf redSquare
basicComboTop = Combine blueCircle Above redSquare
basicComboBottom = Combine blueCircle Below redSquare
basicComboOver = Combine blueCircle Over redSquare 

-- Complex combinations
combo1 = Combine redSquare Below (Combine yellowCircle Above (Combine pinkSquare LeftOf ( Combine greenRectangle RightOf ( Combine yellowCircle Over bluePentagon )) ) )
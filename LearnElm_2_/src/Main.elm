module Main exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Html exposing (Html, div, text)
import Html.Attributes exposing (..)
import Html.App
import Random
import Window
import Task
import Mouse

type alias Figure =
  { pos : Position
  , r : Int    -- radius for figure 
  , c : String -- color of figure
  }


-- MODEL 
type alias Model =
  { size : Window.Size
  , currentR : Int          -- current value of radius
  , currentC : String       -- current value of color
  , figures : List Figure   -- figures to draw 
  }


type alias Position =
  { x : Int, y : Int }


-- MESSAGES 
type Msg
  = Click Position  -- mouse click message
  | NewRadius Int   -- random radius message
  | WindowSize Window.Size


init : (Model, Cmd Msg)                                                         
init =                                                                          
  ( Model (Window.Size 1855 980) 20 "blue" []
  , Task.perform (\_ -> Debug.crash "task") WindowSize Window.size
  ) 


topMargin : Model -> String
topMargin model = 
  ((toString ((model.size.height - mainRectH)//2))++"px") 


leftMargin : Model -> String
leftMargin model = 
  ((toString ((model.size.width - mainRectW)//2))++"px")


-- VIEW 
view : Model -> Html Msg
view model =
  div
    [ 
      Html.Attributes.style                                                         
            [ ( "position", "absolute" )                                                
            , ( "top", (topMargin model) )                
            , ( "left", (leftMargin model) )                
            , ( "width", (toString mainRectW) )                                         
            , ( "height", (toString mainRectH) )                                        
            , ( "border", "1px solid #000000" )                                         
            , ( "background-color", "#87CEFA" )                                         
            ] 
    ]
    [ (addMainPanel model) ] 


mainRectW : Int
mainRectW = 800

mainRectH : Int
mainRectH = 600

colorSqW : Int
colorSqW = 30

colorSqH : Int
colorSqH = 30

calcColorSqLX : Int -> Int
calcColorSqLX ind = ind*colorSqW + ind*10

calcColorSqRX : Int -> Int                                                       
calcColorSqRX ind = ind*colorSqW + ind*10 + colorSqW


addColorPanel : List (Svg.Svg msg)
addColorPanel = 
      [ ( addColorSquare "#FF0000" (toString (calcColorSqLX 0)) )
      , ( addColorSquare "#008000" (toString (calcColorSqLX 1)) )
      , ( addColorSquare "#FFFF00" (toString (calcColorSqLX 2)) )
      , ( addColorSquare "#0000FF" (toString (calcColorSqLX 3)) )
      ]   


addColorSquare : String -> String -> Svg.Svg msg
addColorSquare c sx =
  (Svg.rect                                                                
      [ fill c
      , x sx
      , y "0"
      , stroke "black"
      , Svg.Attributes.width (toString colorSqW)
      , Svg.Attributes.height (toString colorSqH) 
      ] 
      [] 
  )


addMainPanel : Model -> Html Msg
addMainPanel model =
  Svg.svg
      [ version "1.1"
      , Svg.Attributes.width  (toString mainRectW)
      , Svg.Attributes.height (toString mainRectH)
      , x "0" 
      , y "0" 
      , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH))
      ]
      ( 
         [ Svg.rect
               [ fill "#87CEFA"                                                  
               , x "0"                  
               , y "0" 
               , Svg.Attributes.width (toString mainRectW)
               , Svg.Attributes.height (toString mainRectH)
               ]
               []
          ]             
          ++ 
          (drawAllFigures model)
          ++
          addColorPanel
      )


drawAllFigures : Model -> List (Svg.Svg msg)
drawAllFigures model =
  ( List.map (\f ->  drawFigure f) model.figures ) 


drawFigure : Figure -> Svg.Svg msg                                              
drawFigure f =                                                                  
  Svg.polygon                                                               
      [ fill f.c
      , x "0" 
      , y "0"
      , stroke "black" 
      , (getTriangPoints (f.pos.x) (f.pos.y) f.r) 
      ] 
      []  


getTriangPoints : Int -> Int -> Int -> Svg.Attribute msg                        
getTriangPoints x y r =                                                         
  points                                                         
    ( (getLUTriangPoint x y r) ++                                             
      (getMDTriangPoint x y r) ++                                             
      (getRUTriangPoint x y r)                                                
    )                     


getLUTriangPoint : Int -> Int-> Int -> String                                   
getLUTriangPoint x y r =                                                        
  (toString (x - r)) ++ " " ++ (toString (y - r)) ++ ","                        


getMDTriangPoint : Int -> Int-> Int -> String                                   
getMDTriangPoint x y r =                                                        
  (toString x) ++ " " ++ (toString (y + r)) ++ ","                              


getRUTriangPoint : Int -> Int -> Int -> String                                  
getRUTriangPoint x y r =                                                        
  (toString (x + r)) ++ " " ++ (toString (y - r))


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
  Click pos ->
    ( Position 
          ( pos.x - ((model.size.width - mainRectW)//2) ) 
          ( pos.y - ((model.size.height - mainRectH)//2) )
    )
    |> clickMsgProccessing model 
  NewRadius newR ->
    ( newRMsgProccessing model newR )
  WindowSize { width, height } ->
    ( { model | size = (Window.Size (width) (height)) }, Cmd.none )


-- process mouse message
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )
clickMsgProccessing model pos =
  if pos.y >= 0 && pos.y <= colorSqH 
          && pos.x >= (calcColorSqLX 0) && pos.x <= (calcColorSqRX 0)
            then ( curCUpd model "#FF0000" pos )                                    
  else if pos.y >= 0 && pos.y <= colorSqH 
          && pos.x >= (calcColorSqLX 1) && pos.x <= (calcColorSqRX 1)         
            then ( curCUpd model "#008000" pos )                                    
  else if pos.y >= 0 && pos.y <= colorSqH 
          && pos.x >= (calcColorSqLX 2) && pos.x <= (calcColorSqRX 2)          
            then ( curCUpd model "#FFFF00" pos )                                    
  else if pos.y >= 0 && pos.y <= colorSqH 
          && pos.x >= (calcColorSqLX 3) && pos.x <= (calcColorSqRX 3)          
            then ( curCUpd model "#0000FF" pos )
  else if pos.y < 0 || pos.y > mainRectH || pos.x < 0 || pos.x > mainRectW
          then ( model, Cmd.none )
  else ( addNewFigure model pos )  


-- process message for generate new random radius
newRMsgProccessing : Model -> Int -> ( Model, Cmd Msg ) 
newRMsgProccessing model r =
  ( { model | currentR = r }, Cmd.none )


-- update currentColor after mouse click on color square
curCUpd : Model -> String -> Position -> ( Model, Cmd Msg )
curCUpd m c p =
  ( { m | currentC = c }, Cmd.none)   


-- put new figure in list of figures for drawing (with random radius)
addNewFigure : Model -> Position -> ( Model, Cmd Msg )
addNewFigure m p =
  ( { m | figures = m.figures ++ [Figure p m.currentR m.currentC]},
    Random.generate NewRadius (Random.int 20 100) 
  )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = 
  Sub.batch
    [ Mouse.clicks Click 
    , Window.resizes WindowSize
    ]


-- MAIN
main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

module Main exposing (..)

import Svg exposing (Svg)
import Svg.Attributes exposing (..)
import Json.Decode as Json exposing ((:=))
import VirtualDom
import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Random


type alias Figure =
  { pos : Position
  , n : Int    -- number of poligon`s sides
  , r : Int    -- radius for figure (from 20 to 100)
  , c : String -- color of figure
  }


-- MODEL 
type alias Model =
  { currentN : Int          -- current value of n - number of sides 
  , currentR : Int          -- current value of radius
  , currentC : String       -- current value of color
  , figures : List Figure   -- figures to draw 
  }


type alias Position =
  { x : Int, y : Int }


-- MESSAGES 
type Msg
  = Decrement       -- button click messages 
  | Increment
  | Click Position  -- mouse click message
  | NewRadius Int   -- random radius message


init : (Model, Cmd Msg)                                                         
init =                                                                          
    ( Model 3 20 "#FFFFFF" [], Cmd.none ) 


-- VIEW 
view : Model -> Html Msg
view model =
  div [] 
    [ button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
    , div [] [ Html.text (toString model.currentN) ]
    , button [ Html.Events.onClick Increment ] [ Html.text "+" ]
    , drawColorSquares
    , (drawMainRect model)
    ] 


drawColorSquares : Html Msg
drawColorSquares = 
  Svg.svg -- collage with squares of color                                  
      [ version "1.1", x "0", y "0", viewBox "0 0 100 4"]                     
      [ ( addColorSquare "#FF0000" "0" )
      , ( addColorSquare "#7FD13B" "3" )
      , ( addColorSquare "#FFFF00" "6" )
      , ( addColorSquare "#0000FF" "9" )
      ]   


addColorSquare : String -> String -> Svg.Svg msg
addColorSquare c sx =
  (Svg.rect                                                                
      [ fill c, x sx, y "1", width "2", height "2" ] [] )


-- Main rectangular for drawing 
drawMainRect : Model -> Html Msg
drawMainRect model =
  Svg.svg                                    
      [version "1.1", x "0", y "0", viewBox "0 0 100.0 40.0", clickGenerate]  
      ( drawBackground ++ (drawAllFigures model) ) 


drawBackground : List (Svg.Svg msg)
drawBackground =
  [ Svg.rect                                                              
        [ fill "#87CEFA"                                                  
        ,  x "0"                                                          
        ,  y "0"                                                          
        , width "1000"                                                    
        , height "1000"                                                   
        ]                                                                 
        []
  ]


drawAllFigures : Model -> List (Svg.Svg msg)
drawAllFigures model =
  ( List.map (\f ->  drawFigure f) model.figures ) 


drawFigure : Figure -> Svg.Svg msg                                              
drawFigure f =                                                                  
  case f.n of                                                                   
    3 ->                                                                        
      Svg.polygon                                                               
          [ fill f.c, x "0", y "0", (getTriangPoints f.pos.x f.pos.y f.r) ] []  
    _ ->                                                                        
      Svg.rect                                                                  
          [ fill f.c, (getRectX f.pos.x f.r), (getRectY f.pos.y f.r),           
            (getRectW f.r), (getRectH f.r)                                      
          ] []                                                                  


getRectX : Int -> Int -> Svg.Attribute msg                                      
getRectX x r =                                                                  
  Svg.Attributes.x (toString (x - r))                                           
                                                                                    
getRectY : Int -> Int -> Svg.Attribute msg                                      
getRectY y r =                                                                  
  Svg.Attributes.y (toString (y - r))                                           
                                                                                    
getRectW : Int -> Svg.Attribute msg                                             
getRectW r =                                                                    
  Svg.Attributes.width (toString (2*r))                                         
                                                                                    
getRectH : Int -> Svg.Attribute msg                                             
getRectH r =                                                                    
  Svg.Attributes.height (toString(2*r))                                         
  
getTriangPoints : Int -> Int -> Int -> Svg.Attribute msg                        
getTriangPoints x y r =                                                         
  Svg.Attributes.points                                                         
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


clickGenerate : VirtualDom.Property Msg 
clickGenerate =
  VirtualDom.onWithOptions "click" options (Json.map Click offsetPosition)


options =
  { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decoder Position
offsetPosition =
  Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
  Decrement ->
    ( decMsgProccessing model )
  Increment ->
    ( incMsgProccessing model )
  Click pos ->
    ( clickMsgProccessing model pos )
  NewRadius newR ->
    ( newRMsgProccessing model newR )


-- process button messages 
decMsgProccessing : Model -> ( Model, Cmd Msg ) 
decMsgProccessing model =
  if model.currentN > 3                                                       
    then (curNI model)                                                       
  else ( model, Cmd.none ) 


incMsgProccessing : Model -> ( Model, Cmd Msg )
incMsgProccessing model =
  if model.currentN < 4                                                       
    then (curND model)                                                       
  else ( model, Cmd.none ) 


-- process mouse message
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )
clickMsgProccessing model pos =
  -- click color squares                                                      
  if pos.y >= 98 && pos.y <= 132 && pos.x >= 0 && pos.x < 37                  
      then ( curCUpd model "#FF0000" )                                    
  else if pos.y >= 98 && pos.y <= 132 && pos.x >= 56 && pos.x < 93            
      then ( curCUpd model "#008000" )                                    
  else if pos.y >= 98 && pos.y <= 132 && pos.x >= 112 && pos.x < 149          
      then ( curCUpd model "#FFFF00" )                                    
  else if pos.y >= 98 && pos.y <= 132 && pos.x >= 168 && pos.x < 205          
      then ( curCUpd model "#0000FF" )                                    
  -- click main rect
  else ( addNewFigure model pos )  


-- process message for generate new random radius
newRMsgProccessing : Model -> Int -> ( Model, Cmd Msg ) 
newRMsgProccessing model r =
  ( { model | currentR = r }, Cmd.none )


-- update currentN after Increment message
curNI : Model -> ( Model, Cmd Msg )
curNI m =
  ( { m | currentN = m.currentN - 1 }, Cmd.none )


-- update currentN after Decrement message
curND : Model -> ( Model, Cmd Msg )
curND m =
  ( { m | currentN = m.currentN + 1 }, Cmd.none )


-- update currentColor after mouse click on color square
curCUpd : Model -> String -> ( Model, Cmd Msg )
curCUpd m c =
  ( { m | currentC = c }, Cmd.none)   


-- put new figure in list of figures for drawing (with random radius)
addNewFigure : Model -> Position -> ( Model, Cmd Msg )
addNewFigure m p =
  ( { m | figures = m.figures ++ [Figure p m.currentN m.currentR m.currentC]},
    Random.generate NewRadius (Random.int 1 3) 
  )


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- MAIN
main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

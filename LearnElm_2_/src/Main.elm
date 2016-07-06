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
  , n : Int   -- number of poligon`s sides
  , r : Int   -- radius for figure (from 20 to 100)
  , c : String -- color of figure
  }


-- MODEL 
type alias Model =
  { currentN : Int          -- current value of n - number of sides 
  , currentR : Int          -- current value of radius
  , currentC : String       -- current value of color
  , figures : List Figure   -- figures to draw 
  , pos : Position
  }


type alias Position =
  { x : Int, y : Int }


init : (Model, Cmd Msg)
init =
  (Model 3 20 "#FFFFFF" [] (Position 0 0), Cmd.none)


-- MESSAGES 
type Msg
  = Decrement               -- button click messages 
  |Increment
  |Click Position  -- mouse click message
  |NewRadius Int            -- random radius message


drawFigure : Figure -> Svg.Svg msg
drawFigure f =
  case f.n of
    3 ->
      Svg.polygon [ fill f.c, x "5", y "5", points 
      ((toString (f.pos.x - 20*f.r)) ++ " " ++ (toString (f.pos.y - 20*f.r)) ++ "," ++ (toString f.pos.x) ++ " " ++ (toString (f.pos.y + 20*f.r)) ++ "," ++ (toString (f.pos.x + 20*f.r)) ++ " " ++ (toString (f.pos.y- 20*f.r)) ++ ",") ] []
    _ ->
      Svg.rect 
        [ fill f.c, x (toString (f.pos.x - 2*f.r)), y (toString (f.pos.y - 2*f.r)), 
          width (toString (4*f.r)), height (toString(4*f.r)) ] [] 


-- VIEW 
view : Model -> Html Msg
view model =
  div [] 
    [ button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
    , div [] [ Html.text (toString model.currentN) ]
    , button [ Html.Events.onClick Increment ] [ Html.text "+" ]
    , Svg.svg -- collage with squares of color
        [ version "1.1", x "0", y "0", viewBox "0 0 100 4"] --NO 100% from window width, 10% from window heigth
        [ Svg.rect                                                                       
              [ fill "#FF0000", x "0", y "1", width "2", height "2" ] --? x y = % from viewBox width and heigth
              []
        , Svg.rect                                                          
              [ fill "#7FD13B", x "3", y "1", width "2", height "2" ]           
              [] 
        , Svg.rect                                                          
              [ fill "#FFFF00", x "6", y "1", width "2", height "2" ]
              [] 
        , Svg.rect
              [ fill "#0000FF", x "9", y "1", width "2", height "2" ]
              []
        ]
    , Svg.svg -- main collage for drawing
        [ version "1.1", x "0", y "0", viewBox "0 0 100.0 40.0", 
          VirtualDom.onWithOptions "click" options (Json.map Click offsetPosition)]
        (
          [ Svg.rect                                                                      
              [ fill "#87CEFA"                                                          
              ,  x "0"                                                                  
              ,  y "0"                                                                  
              , width "1000"                                                            
              , height "1000"                                                           
              --, Svg.Events.onClick MouseMsg
              --, VirtualDom.onWithOptions "click" options (Json.map Click offsetPosition)
              ]                                                                         
              [] 
          ]
          ++ 
          {-  (List.map (\f ->   -- draw all of figures 
                          drawFigure f 
                        ) 
                        model.figures
            )-}
          [ Svg.line                                                                      
                [ x1 "0"                                                                    
                , y1 "0"                                                                    
                , x2 (toString model.pos.x)                                                 
                , y2 (toString model.pos.y)                                                 
                , style "stroke:rgb(255,0,0);stroke-width:2"                                
                ]                                                                           
                [] 
          ]
        )
    ] 


options =
  { preventDefault = True, stopPropagation = True }


offsetPosition : Json.Decoder Position
offsetPosition =
  Json.object2 Position ("offsetX" := Json.int) ("offsetY" := Json.int)


-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
  -- process button messages
  Decrement ->
    if model.currentN > 3 then ( { model | currentN = model.currentN - 1 }, Cmd.none )
    else ( model, Cmd.none )
  Increment ->
    if model.currentN < 4 then ( { model | currentN = model.currentN + 1 }, Cmd.none ) 
    else ( model, Cmd.none )
  -- process mouse message
  Click pos ->
    -- click color squares
    if pos.y >= 98 && pos.y <= 132 && pos.x >= 0 && pos.x < 37 then ( { model | currentC = "#FF0000" }, Cmd.none) 
    else if pos.y >= 98 && pos.y <= 132 && pos.x >= 56 && pos.x < 93 then ( { model | currentC = "#008000" }, Cmd.none)
    else if pos.y >= 98 && pos.y <= 132 && pos.x >= 112 && pos.x < 149 then ( { model | currentC = "#FFFF00" }, Cmd.none)
    else if pos.y >= 98 && pos.y <= 132 && pos.x >= 168 && pos.x < 205 then ( { model | currentC = "#0000FF" }, Cmd.none)
    -- white area not for drawing
    --else if pos.y <= 154 || pos.x > 1000 then ( model, Cmd.none ) 
    -- put new figure in list of figures for drawing (with random radius)
    -- else ( { model | figures = model.figures ++ [Figure pos model.currentN model.currentR model.currentC]}, Random.generate NewRadius (Random.int 20 100) )
    else ({ model | pos = pos}, Random.generate NewRadius (Random.int 20 100))
  -- process message for generate new random radius
  NewRadius newR ->
    ( { model | currentR = newR }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none
--Mouse.clicks MouseMsg


-- MAIN
main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

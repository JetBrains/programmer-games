-- Make radius random in between 20 and 40.

module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Mouse
import Collage exposing (..)
import Element
import Color exposing (..)
import Random

type alias Figure =
        { x : Int -- coordinates of center (point of mouse click)
        , y : Int
        , n : Int -- number of poligon`s sides
        , r : Int -- radius for figure (from 20 to 40)
        }

-- MODEL 
type alias Model =
        { currentN : Int          -- current value of n - number of sides 
        , currentR : Int
        , figures : List Figure   -- figures to draw 
        }


init : (Model, Cmd Msg)
init =
        (Model 3 20 [] , Cmd.none)


-- MESSAGES 
type Msg
    = Decrement
    |Increment
    |MouseMsg Mouse.Position
    |NewRadius Int


clearGrey : Color.Color
clearGrey =
           Color.rgba 111 111 111 0.6

width : Float 
width = 1000 

height : Float
height = 1000

-- VIEW 
view : Model -> Html Msg
view model =
      div [] 
          [ button [ onClick Decrement ] [ Html.text "-" ]
            , div [] [ Html.text (toString model.currentN) ]
            , button [ onClick Increment ] [ Html.text "+" ]
          , collage 
              (floor width) (floor height) 
              ( [rect width height |> filled clearGrey] ++
                (List.map (\f -> 
                             ngon f.n (toFloat f.r) 
                               |> filled blue 
                               |> move ( ((toFloat f.x) - width/2), (height/2 - (toFloat f.y) + 70) )
                          ) 
                          model.figures
                )
              )
              |> Element.toHtml
          ] 

-- UPDATE
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of
           Decrement ->
               if model.currentN > 3 then ( { model | currentN = model.currentN - 1 }, Cmd.none )
               else ( model, Cmd.none )
           Increment ->
                if model.currentN < 12 then ( { model | currentN = model.currentN + 1 }, Cmd.none ) 
                else ( model, Cmd.none )
           MouseMsg pos ->
                if pos.y <= 79 then ( model, Cmd.none ) 
                else ( { model | figures = model.figures ++ [Figure pos.x pos.y model.currentN model.currentR]}, Random.generate NewRadius (Random.int 20 100) )
           NewRadius newR ->
                ( { model | currentR = newR }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
     Mouse.clicks MouseMsg

-- MAIN

main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

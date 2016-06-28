module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Mouse

import Collage exposing (..)
import Element
import Color exposing (..)

type alias Figure =
        { x : Int -- coordinates of center (point of mouse click)
        , y : Int 
        , n : Int   -- number of poligon`s sides
        }

-- MODEL 
type alias Model =
        { currentN : Int          -- current value of n - number of sides 
        , figures : List Figure   -- figures to draw 
        }

initialModel: Model
initialModel =
        { currentN = 3
        , figures = []
        }

init : (Model, Cmd Msg)
init =
        (initialModel, Cmd.none)


-- MESSAGES 
type Msg
    = Decrement
    |Increment
    |MouseMsg Mouse.Position


clearGrey : Color.Color
clearGrey =
           Color.rgba 111 111 111 0.6


-- VIEW 
view : Model -> Html Msg
view model =
--   let x = (List.head model.figures).x   List.head return Maybe!!!
--   in
      div []
          [ button [ onClick Decrement ] [ Html.text "-" ]
          , div [] [ Html.text (toString model.currentN) ]
          , button [ onClick Increment ] [ Html.text "+" ]
--          , List.map (\f -> collage f.x f.y [filled clearGrey (ngon model.currentN 30)] |> Element.toHtml ) model.figures
          , collage 300 300 [filled clearGrey (ngon model.currentN 60)] |> Element.toHtml
          ]


newFigure : Int -> Int -> Int -> Figure 
newFigure x y n =
        { x = x
        , y = y
        , n = n
        }


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
                ( { model | figures = model.figures ++ [newFigure pos.x pos.y model.currentN]}, Cmd.none )


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

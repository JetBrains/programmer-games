module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Mouse

-- MODEL

type alias Model =
      Int

init : ( Model, Cmd Msg )
init =
     ( 3, Cmd.none )

-- MESSAGES

type Msg
    = Decrement
    |Increment
    |MouseMsg Mouse.Position

-- VIEW

view : Model -> Html Msg
view model =
      div []
          [ button [ onClick Decrement ] [ text "-" ]
          , div [] [ text (toString model) ]
          , button [ onClick Increment ] [ text "+" ]
          ]

-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
        case msg of
           Decrement ->
               if model > 3 then ( model - 1, Cmd.none )
               else ( model, Cmd.none )
           Increment ->
                if model < 12 then ( model + 1, Cmd.none ) 
                else ( model, Cmd.none )
           MouseMsg position ->
                ( model, Cmd.none )

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

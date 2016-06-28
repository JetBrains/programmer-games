--Project 1: 2 buttons with + and -, changing the number N in between 3 and 12.
                                                    
module Main exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Mouse


-- MODEL - state of counter
type alias Model =
      Int


-- Init function: counter initial state is 3; 
-- do nothing, so Cmd.none
init : ( Model, Cmd Msg )
init =
     ( 3, Cmd.none )


-- MESSAGES - set of messages that we will get from the UI
-- Messages will trigger when one of the buttons or mouse are pressed.
type Msg
    = Decrement
    |Increment
    |MouseMsg Mouse.Position


-- VIEW - a way to view state as HTML 
view : Model -> Html Msg
view model =
      div []
          [ button [ onClick Decrement ] [ text "-" ]
          , div [] [ text (toString model) ]
          , button [ onClick Increment ] [ text "+" ]
          ]


-- UPDATE describes what to do when receive one of messages.
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

-- SUBSCRIPTIONS declares the things we want to listen to
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

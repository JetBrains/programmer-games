import GameBase.Data.LevelsData exposing (machine1, transTable1, input1, 
                                          expectedResult1, expectedPos1)
import GameBase.Data.GameTypes exposing (Msg(..), Model, Position)       
import GameBase.Data.Init exposing (init)
import GameBase.UI.Div exposing (menuDiv, gameDiv, rulesDiv, authorsDiv, 
                                 finalDiv)
import GameBase.UI.DivSvgStyles exposing (mainRectW, mainRectH)
import GameBase.Proccessing.MsgProccessing exposing (clickMsgProccessing, 
                                                     moveMsgProccessing, 
                                                     tickMsgProccessing)

import Html exposing (Html, div)
import Html.Attributes exposing (style) 
import Html.App exposing (program)
import Window exposing (Size, resizes)
import Mouse exposing (clicks, moves)
import Time exposing (every)


-- VIEW                                                                         
view : Model -> Html Msg                                                        
view model =                                                                    
  div 
    [
      style
        [ ( "width",  (toString model.windSize.width ++ "px") )                                 
        , ( "height", (toString model.windSize.height ++ "px") )   
        , ( "background-color", "#F4A460" )
        ]
    ]
    (
      if model.ifStart == True
              then (menuDiv model)
      else if model.ifRules == True
              then (rulesDiv model)
      else if model.ifAuthors == True
              then (authorsDiv model)
      else if model.ifEnd == True
              then (finalDiv model)
      else (gameDiv model) 
    )


getPosition : Model -> Position -> Position
getPosition model pos =
  { x = pos.x - ( (model.windSize.width - mainRectW) // 2)
  , y = pos.y - ( (model.windSize.height - mainRectH) // 2)             
  }


-- UPDATE                                                                       
update : Msg -> Model -> ( Model, Cmd Msg )                                     
update msg model =
  case msg of                                                                   
    Click pos ->  
      (getPosition model pos)
      |> clickMsgProccessing model
    Move pos ->
      (getPosition model pos)                                                   
      |> moveMsgProccessing model   
    WindowSize { width, height } ->                                               
      ( { model | windSize = (Size (width) (height)) }, Cmd.none )   
    Tick time ->
      tickMsgProccessing model time


-- SUBSCRIPTIONS                                                                
subscriptions : Model -> Sub Msg                                                
subscriptions model =                                                           
  Sub.batch                                                                     
    [ clicks Click  
    , moves Move
    , resizes WindowSize                                                 
    , every model.timeUnit Tick
    ] 


-- MAIN                                                                         
main : Program Never
main =                                                                          
  program                                                              
    { init = (init machine1 transTable1 input1 expectedResult1 1 expectedPos1) 
    , view = view                                                               
    , update = update                                                           
    , subscriptions = subscriptions                      
    }  

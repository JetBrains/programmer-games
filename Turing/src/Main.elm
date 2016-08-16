import GameBase.Data.LevelsData.DemoLevelData exposing  
            (machineDemo, inputDemo, transTableDemo, expectedResultDemo, 
             expectedPosDemo, usedCatsDemo, usedBallsDemo, basketsNumbDemo)
import GameBase.Data.GameTypes exposing (Msg(..), Model, Position)          
import GameBase.Data.Init exposing (init)                                   
import GameBase.UI.MainObjects.Div exposing                                 
                             (menuDiv,gameDiv,rulesDiv,authorsDiv,finalDiv)
import GameBase.UI.MainObjects.DivSvgStyles exposing (mainRectW, mainRectH)
import GameBase.Proccessing.MsgProccessing.MsgProccessing exposing 
              (clickMsgProccessing, moveMsgProccessing, tickMsgProccessing)

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
    [ style
        [ ( "width",  (toString model.options.winSize.width ++ "px") )                                 
        , ( "height", (toString model.options.winSize.height ++ "px") )   
        , ( "background-color", "#F4A460" )
        ]
    ]
    ( if model.flags.ifStart == True
              then (menuDiv model)
      else if model.flags.ifRules == True
              then (rulesDiv model)
      else if model.flags.ifAuthors == True
              then (authorsDiv model)
      else if model.flags.ifEnd == True
              then (finalDiv model)
      else (gameDiv model) 
    )


getPosition : Model -> Position -> Position
getPosition model pos =
  { x = pos.x - ( (model.options.winSize.width - mainRectW) // 2)
  , y = pos.y - ( (model.options.winSize.height - mainRectH) // 2)             
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
      ( { model 
            | options = 
                { winSize = (Size (width) (height))
                , timeUnit = model.options.timeUnit
                , whenGameStarts = model.options.whenGameStarts
                , currTime = model.options.currTime
                , tapeCellsNumb = model.options.tapeCellsNumb
                }
        }
      , Cmd.none 
      )
    Tick time ->
      tickMsgProccessing model time


-- SUBSCRIPTIONS                                                                
subscriptions : Model -> Sub Msg                                                
subscriptions model =                                                           
  Sub.batch                                                                     
    [ clicks Click  
    , moves Move
    , resizes WindowSize                                                 
    , every model.options.timeUnit Tick
    ] 


fstLevel : Int
fstLevel = 1


-- MAIN                                                                         
main : Program Never
main =                                                                          
  program
    { init = (init basketsNumbDemo inputDemo machineDemo transTableDemo 
       fstLevel expectedPosDemo expectedResultDemo usedCatsDemo usedBallsDemo)
    , view = view                                                               
    , update = update                                                           
    , subscriptions = subscriptions                      
    } 

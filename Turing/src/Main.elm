import Html exposing (Html, div, text)
import Html.Attributes exposing (..)                                            
import Html.App  

import TuringTypes exposing ( Direction(..), Machine, MachineCfg, TapeCfg, 
                              TransTable )
import RunTuring exposing (runMachine, transFunc)                          
import InitUpdate exposing (initMachineCfg)

import Svg exposing (Svg)                                                       
import Svg.Attributes exposing (..)                                             

import Window                                                                   
import Task                                                                     
import Mouse  

import List exposing (head, drop, length, take)
import Array exposing (empty, toList)


------------------------------------------------------------------------------
type BallOfWool = Red | Yellow | Green | Blue -- a
--white is start, orange is natural, violet is reject
type Kitten = White | LightGrey | Grey | Orange | Violet -- b 


machine : Machine BallOfWool Kitten
machine =
  { transition = (transFunc transTable (Violet, Nothing, MoveLeft))
  , initHeadPosForDraw = 3
  , initHeadPosForMach = 0
  , startState = White
  , acceptState = Orange 
  , rejectState = Violet
  }


-- Change item (0 _ _ -> 1 _ _)
transTable : TransTable BallOfWool Kitten                                       
transTable =                                                                    
  [ { key = (White, Just Yellow),                                               
      value = (Orange, Just Red, MoveRight)
    } 
  ]  


input : List (Maybe BallOfWool)
input = 
  [Just Yellow]

------------------------------------------------------------------------------

-- MODEL                                                                        
type alias Model =                                                              
  { windSize     : Window.Size                                                          
  , inpWord      : List (Maybe BallOfWool) 
  , machine      : Machine BallOfWool Kitten
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)
  , transTable   : TransTable BallOfWool Kitten
  , catLeft      : Int
  , catImg       : String
  , helpImg      : String 
  }                                                                             
                                                                                
                                                                                
type alias Position =                                                           
  { x : Int, y : Int }                                                          
                                                                                
                                                                                
-- MESSAGES                                                                     
type Msg                                                                        
  = Click Position                     
  | WindowSize Window.Size                                                      
                                                                                
                                                                                
init : Machine BallOfWool Kitten -> TransTable BallOfWool Kitten -> 
       List (Maybe BallOfWool) -> (Model, Cmd Msg)                                                         
init machine table inp =                                                                          
  ( Model 
      (Window.Size 1855 980) 
      inp
      machine
      [(initMachineCfg machine inp machine.initHeadPosForMach)]
      table
      45
      "../img/saimonThink/SaimonThinkW.png"
      " "
  , Task.perform 
        (\_ -> Debug.crash "task") WindowSize Window.size              
  )                                                                             
                                                                                
                                                                                
topMargin : Model -> String                                                     
topMargin model =                                                               
  ((toString ((model.windSize.height - mainRectH)//2))++"px")                       
                                                                                
                                                                                
leftMargin : Model -> String                                                    
leftMargin model =                                                              
  ((toString ((model.windSize.width - mainRectW)//2))++"px")  


mainRectW : Int                                                                 
mainRectW = 800                                                                 


mainRectH : Int                                                                 
mainRectH = 600   


-- VIEW                                                                         
view : Model -> Html Msg                                                        
view model =                                                                    
  div 
    [
      Html.Attributes.style
          [ ( "width",  (toString model.windSize.width ++ "px") )                                 
          , ( "height", (toString model.windSize.height ++ "px") )   
          , ( "background-color", "#F4A460" )
          --, ( "background-image", "../img/klubok.png" )
          ]
    ]
    [
      div                                                                           
        [                                                                           
          Html.Attributes.style                                                     
              [ ( "position", "absolute" )                                        
              , ( "top", (topMargin model) )                                      
              , ( "left", (leftMargin model) )                                    
              , ( "width", (toString mainRectW) )                                 
              , ( "height", (toString mainRectH) )                                
              , ( "border", "1px solid #000000" )                                 
              , ( "background-color", "grey" )                                 
              ]                                                                   
        ]                                                                           
        [ (addMainPanel model) ]
    ]


basketLeftMarginI : Int -> Int                                               
basketLeftMarginI ind = ind * 70 + (ind+1)*40


basketTopMarginI : Int                                                       
basketTopMarginI = 410


basketLeftMarginS : Int -> String
basketLeftMarginS ind =
  toString (basketLeftMarginI ind) ++ "px"


basketTopMarginS : String
basketTopMarginS =
  (toString basketTopMarginI) ++ "px"


getNewBasket : Int -> Svg msg
getNewBasket ind =
  Svg.image                                                            
    [ x (basketLeftMarginS ind)                                                       
    , y basketTopMarginS                                            
    , Svg.Attributes.width "100px"                                   
    , Svg.Attributes.height "100px"                                  
    , xlinkHref ("../img/basket/basket.png")                                
    ]                                                                
    []    


allBasketsDraw : Int -> List (Svg msg) -> List (Svg msg)
allBasketsDraw n res =
  if n > 0 then ( allBasketsDraw (n-1) (res ++ [getNewBasket (n-1)]) )
  else res


tableW : String                                                                 
tableW = (toString mainRectW)                                                   


tableH : String                                                                 
tableH = (toString mainRectW) 


tableDraw : List (Svg msg)
tableDraw =
  [ Svg.image                                                            
      [ x "0"                                                          
      , y "0"                                                          
      , Svg.Attributes.width tableW                                    
      , Svg.Attributes.height tableH                                   
      , xlinkHref ("../img/table.jpg")                                 
      ]                                                                
      [] 
  ]


mirrorDraw : List (Svg msg) 
mirrorDraw =
  [ Svg.image                                                            
      [ x "30"                                                         
      , y "55"                                                         
      , Svg.Attributes.width "335px"                                   
      , Svg.Attributes.height "270px"                                  
      , xlinkHref ("../img/mirror/mirrorForOneChange.png")                               
      ]                                                                
      []                                                               
  ]  


ballLeftMarginI : Int -> Int
ballLeftMarginI ind =
  (basketLeftMarginI ind) + 27


ballTopMarginI : Int
ballTopMarginI =
  (basketTopMarginI) + 15


ballLeftMarginS : Int -> String                                                           
ballLeftMarginS ind =                                                               
  (toString (ballLeftMarginI ind) ++ "px")


ballTopMarginS : String                                                            
ballTopMarginS =                                                                
   (toString (ballTopMarginI) ++ "px")


getBallColor : Maybe (Maybe BallOfWool) -> String                               
getBallColor inpVal =                                                           
  case inpVal of                                                                
    Just (Just Red) -> "Red"                                                    
    Just (Just Yellow) -> "Yellow"                                              
    Just (Just Green) -> "Green"                                                
    Just (Just Blue) -> "Blue"                                                  
    _ -> "Black"   


getNewBall : Int -> Maybe (Maybe BallOfWool) -> Svg msg
getNewBall ind inpVal =
  let 
    color = (getBallColor inpVal)
  in
    Svg.image                                                            
        [ x (ballLeftMarginS ind)
        , y (ballTopMarginS)                                                        
        , Svg.Attributes.width "50px"                                    
        , Svg.Attributes.height "50px"                                   
        , xlinkHref ("../img/balls/" ++ color ++ "Ball.png")                                
        ]                                                                
        []


getTapeFromCfg : Maybe (MachineCfg BallOfWool Kitten)                           
                 -> List (Maybe BallOfWool)                                     
getTapeFromCfg maybeCfg =                                                       
  case maybeCfg of                                                              
    Just cfg ->                                                                 
      (                                                                         
        (toList cfg.tapeCfg.leftSyms) ++                                        
        [cfg.tapeCfg.currSym] ++                                                
        (toList cfg.tapeCfg.rightSyms)                                          
      )                                                                         
    Nothing -> []  


ballsOfOneTapeDraw : Int -> List (Svg msg) -> List (Maybe BallOfWool) -> Int 
                     -> List (Svg msg)                          
ballsOfOneTapeDraw n res tape hpos =              
  let 
    inpVal = head (drop (n-1) tape)
  in
    if n > 0 
       then if inpVal == Just (Nothing) 
               then (ballsOfOneTapeDraw (n-1) res tape hpos)
               else 
                 let
                   updRes = (res ++ [getNewBall (n-1+hpos) inpVal]) 
                 in 
                   (ballsOfOneTapeDraw (n-1) updRes tape hpos)               
    else res  
                                                                                
                                                                                
ballsOfAllTapesDraw : Model -> Int -> List (Svg msg) -> List (Svg msg)          
ballsOfAllTapesDraw model hpos res =                                            
  let
      curTape = (getTapeFromCfg (head model.machineCfgs))
      updModel = {model | machineCfgs = (drop 1 model.machineCfgs)}
      updRes = (res ++ (ballsOfOneTapeDraw 7 [] curTape hpos))
  in
    if (length model.machineCfgs) > 0 
       then (ballsOfAllTapesDraw updModel hpos updRes)
    else res                                                                      


catLeftMarginI : Int -> Int -> Int                                                   
catLeftMarginI hpos left =                                                           
  (basketLeftMarginI hpos) - left


catTopMarginI : Int                                                            
catTopMarginI = 
  basketTopMarginI + 35


catLeftMarginS : Int -> Int -> String                                                 
catLeftMarginS ind left =                                                           
    (toString (catLeftMarginI ind left) ++ "px")                                      


catTopMarginS : String                                                         
catTopMarginS =                                                                
     (toString (catTopMarginI) ++ "px") 


catDraw : Int -> Model -> List (Svg msg)
catDraw hpos model =
  let 
    href = model.catImg 
    left = model.catLeft
  in
    [ Svg.image                                                             
          [ x (catLeftMarginS hpos left)                                                         
          , y catTopMarginS                                                        
          , Svg.Attributes.width "155px"                                   
          , Svg.Attributes.height "155px"                                  
          , xlinkHref (href)
          ]                                                                
          []
    ]


transTableDraw : List (Svg msg)
transTableDraw =
  [ Svg.image                                                                   
        [ x "380px"                                             
        , y "30px"                                                    
        , Svg.Attributes.width "460px"                                          
        , Svg.Attributes.height "250px"                                         
        , xlinkHref ("../img/transTableDemo.png")                        
        ]                                                                       
        []                                                                      
  ]  


runButtonDraw : List (Svg msg)
runButtonDraw =
  [ Svg.image                                                                   
        [ x "523px"                                                             
        , y "285px"                                                              
        , Svg.Attributes.width "70px"                                          
        , Svg.Attributes.height "70px"                                         
        , xlinkHref ("../img/elements/run.png")                                  
        ]                                                                       
        []                                                                      
  ]


quesButtonDraw : List (Svg msg)                                                  
quesButtonDraw =                                                                 
  [ Svg.image                                                                   
        [ x "624px"                                                             
        , y "292px"                                                             
        , Svg.Attributes.width "30px"                                           
        , Svg.Attributes.height "55px"                                          
        , xlinkHref ("../img/elements/ques.png")                                 
        ]                                                                       
        []                                                                      
  ]


helpMsgDraw : String -> List (Svg msg)
helpMsgDraw hmsg =
   [ Svg.image                                                                   
        [ x "10px"                                                             
        , y "10px"                                                             
        , Svg.Attributes.width "537px"                                           
        , Svg.Attributes.height "40px"                                          
        , xlinkHref (hmsg)                                
        ]                                                                       
        []                                                                      
  ]    


addMainPanel : Model -> Html Msg                                                
addMainPanel model =     
  let
    hpos = model.machine.initHeadPosForDraw
  in
    Svg.svg                                                                       
        [ version "1.1"                                                           
        , Svg.Attributes.width  (toString mainRectW)                              
        , Svg.Attributes.height (toString mainRectH)                              
        , x "0"                                                                   
        , y "0"                                                                   
        , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH)) 
        ]                                                                         
        (  
          tableDraw
          ++
          mirrorDraw
          ++
          (allBasketsDraw 7 [])
          ++
          (ballsOfAllTapesDraw model hpos []) 
          ++
          (catDraw hpos model)
          ++
          transTableDraw
          ++
          runButtonDraw
          ++
          quesButtonDraw
          ++
          (helpMsgDraw model.helpImg)
        )    


-- UPDATE                                                                       
update : Msg -> Model -> ( Model, Cmd Msg )                                     
update msg model =
  case msg of                                                                   
    Click pos ->  
      let 
        p = ( Position                                                                  
              ( pos.x - ((model.windSize.width - mainRectW)//2) )                       
              ( pos.y - ((model.windSize.height - mainRectH)//2) )                      
            )                                                                           
      in 
        (clickMsgProccessing model p) 
    WindowSize { width, height } ->                                               
        ( { model | windSize = (Window.Size (width) (height)) }, Cmd.none )   


-- process mouse message                                                        
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )                   
clickMsgProccessing model pos =      
  -- process Run button click
  if pos.y >= 285 && pos.y <= 355 && pos.x >= 523 && pos.x <= 593      
     then 
         let                                                                           
           m = model.machine                                                           
           inp = model.inpWord                                                         
           hpos = m.initHeadPosForMach
           cfg = (initMachineCfg m inp hpos)
         in   
           ( { model 
                 | machineCfgs = (runMachine m cfg []) 
                 , catImg = "../img/saimonPush/SaimonPushW.png"
                 , catLeft = 55
             },
             Cmd.none
           )
  else if pos.y >= 292 && pos.y <= 347 && pos.x >= 624 && pos.x <= 654
          then 
            if model.helpImg == " " 
               then ({model | helpImg = "../img/help.png"}, Cmd.none)
            else ({model | helpImg = " "}, Cmd.none)
  else if pos.y < 0 || pos.y > mainRectH || pos.x < 0 || pos.x > mainRectW      
          then (model, Cmd.none)
  else (model, Cmd.none)   


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
    { init = (init machine transTable input) 
    , view = view                                                               
    , update = update                                                           
    , subscriptions = subscriptions                                             
    }  

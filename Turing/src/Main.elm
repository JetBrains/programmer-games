import Html exposing (Html, div, text)
import Html.Attributes exposing (style)                    
import Html.App  

import TuringTypes exposing ( Direction(..), Machine, MachineCfg, TapeCfg, 
                              TransTable )
import RunTuring exposing (runMachine, transFunc)                          
import InitUpdate exposing (initMachineCfg)

import Svg exposing (Svg, text)                                                       
import Svg.Attributes exposing ( fontSize, fontStyle, width, height, x, y, 
                                 xlinkHref, version, viewBox)          

import Window exposing (size)                                
import Task exposing (perform)
import Mouse  

import List exposing (head, drop, length, take)
import Array exposing (empty, toList)
import Time exposing (every, second, Time)

import Cmd.Extra exposing (message)

------------------------------------------------------------------------------
--white is start, orange is natural, violet is reject       
type BallOfWool = Red | Yellow | Green | Blue -- a
type Kitten = White | LightGrey | Grey | Orange | Violet -- b 


machine : Machine BallOfWool Kitten
machine =
  { transition = (transFunc transTable (Violet, Nothing, MoveLeft))
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 0
  , startState = White
  , acceptState = Orange 
  , rejectState = Violet
  }

{-
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
-}

transTable : TransTable BallOfWool Kitten                                       
transTable =                                                                    
  [ { key = (White, Just Red),                                              
      value = (White, Just Red, MoveRight)}                                 
  , { key = (White, Just Yellow),                                           
      value = (White, Just Yellow, MoveRight)}                              
  , { key = (White, Just Green),                                            
      value = (White, Just Green, MoveRight)}                               
  , { key = (White, Just Blue),                                             
      value = (White, Just Blue, MoveRight)}                                
  , { key = (White, Nothing),                                               
      value = (LightGrey, Just Red, MoveLeft)}                                       
  , { key = (LightGrey, Just Red),                                                   
      value = (LightGrey, Just Red, MoveLeft)}                                       
  , { key = (LightGrey, Just Yellow),                                                
      value = (LightGrey, Just Yellow, MoveLeft)}                                    
  , { key = (LightGrey, Just Green),                                                 
      value = (LightGrey, Just Green, MoveLeft)}                                     
  , { key = (LightGrey, Just Blue),                                                  
      value = (LightGrey, Just Blue, MoveLeft)}                                      
  , { key = (LightGrey, Nothing),                                                    
      value = (Orange, Just Blue, MoveRight)}                                    
  ]                                                                             
                                                                                
                                                                                
input : List (Maybe BallOfWool)                                                 
input =                                                                         
  [Just Red, Just Yellow, Just Green, Just Blue]

expectedResult : List (Maybe BallOfWool)
expectedResult =
  [Just Blue, Just Red, Just Yellow, Just Green, Just Blue, Just Red]

------------------------------------------------------------------------------

-- MODEL                                                                        
type alias Model =                                                              
  { windSize     : Window.Size -- for WindowsSize message                                                         
  , machine      : Machine BallOfWool Kitten
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)
  , trTableInit  : TransTable BallOfWool Kitten
  , trTableUser  : TransTable BallOfWool Kitten 
  , catLeft      : Int    -- different for catImg
  , catImg       : String -- catPush, catThink
  , helpImg      : String -- help text
  , currLevel    : Int
  , maxLevel     : Int
  , catPos       : Int 
  , ifPushRun    : Bool
  , expRes       : List (Maybe BallOfWool)
  , finalImg     : String 
  , ifEnd        : Bool
  --, catsImg      : List String -- cats that used on current level
  --, ballsImg     : List String -- balls that used on current level
  }                                                                             
                                                                                
                                                                                
type alias Position =                                                           
  { x : Int, y : Int }                                                          
                                                                                
                                                                                
-- MESSAGES                                                                     
type Msg
  = Click Position
  | WindowSize Window.Size
  | Tick Time
                                                                                
     
initModel : Machine BallOfWool Kitten -> TransTable BallOfWool Kitten ->
            List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> Model
initModel machine table inp expRes =
  ( Model                                                                       
      (Window.Size 1855 980)
      machine                                                                   
      [(initMachineCfg machine inp machine.initHeadPosForMach)]                 
      table                                                                     
      table                                                                     
      45                                                                        
      "../img/saimonThink/SaimonThinkW.png"                                     
      " "                                                                       
      1
      10
      machine.initHeadPosForDraw 
      False
      expRes
      " "
      False
  )


init : Machine BallOfWool Kitten -> TransTable BallOfWool Kitten -> 
       List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> (Model, Cmd Msg) 
init machine table inp expRes =                                                                          
  ( (initModel machine table inp expRes )
  , perform (\_ -> Debug.crash "task") WindowSize size              
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


gameDiv : Model -> List (Html Msg)
gameDiv model =
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


finalDiv : Model -> List (Html Msg)
finalDiv model =
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
          , ( "background-color", "#ff1f15" )                                  
          ]                                                                 
      ]                                                                       
      [
        Svg.svg 
          [ version "1.1" 
          , Svg.Attributes.width  (toString mainRectW)  
          , Svg.Attributes.height (toString mainRectH)
          , x "0" 
          , y "0"
          , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH))
          ]
          [ 
            Svg.image                                                                     
              [ x "0px"                                           
              , y "0px"                                                        
              , Svg.Attributes.width "800px"                                              
              , Svg.Attributes.height "600px"                                             
              , xlinkHref model.finalImg
              ]                                                                           
              [] 
          ]
      ]
  ] 


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
    (
      if model.ifEnd == False 
         then (gameDiv model)
      else (finalDiv model)
    )

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
      , xlinkHref ("../img/mirror/mirrorForBlueRedAtEnds2.png")                               
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
    _ -> "Transp"   


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


getInpV : List (Maybe BallOfWool) -> Int -> Maybe (Maybe BallOfWool)
getInpV tape n = 
  head (drop (n-1) tape)


ballsOfOneTapeDraw : Int -> List (Svg msg) -> List (Maybe BallOfWool) -> Int 
                     -> List (Svg msg)                          
ballsOfOneTapeDraw n res tape hpos =              
    if n > 0 then
      let
        updRes = (res ++ [getNewBall (n-1+hpos) (getInpV tape n)]) 
      in 
        (ballsOfOneTapeDraw (n-1) updRes tape hpos)               
    else res  
  

{-
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
-}


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


catDraw : Model -> List (Svg msg)
catDraw model =
  let 
    href = model.catImg
    left = model.catLeft
    hpos = model.catPos
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
        , xlinkHref ("../img/transTable1.png")                        
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


levelDraw : Int -> Int -> List (Svg msg)
levelDraw level max =
  [ Svg.text'                                                                     
        [ x "695px"
        , y "330px"
        , fontStyle "italic"
        , fontSize "30px"
        ]  
        [ Svg.text ((toString level) ++ "/" ++ (toString max)) ]
  ]


getCurPosForCat : MachineCfg BallOfWool Kitten -> Int
getCurPosForCat cfg =
  case cfg.currDir of
    MoveRight -> 1
    MoveLeft -> -1
    Stay -> 0


addMainPanel : Model -> Html Msg                                                
addMainPanel model =     
  let
    hpos = model.machine.initHeadPosForDraw 
    curTape = (getTapeFromCfg (head model.machineCfgs))
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
          (ballsOfOneTapeDraw 7 [] curTape hpos) 
          ++
          (catDraw model)
          ++
          transTableDraw
          ++
          runButtonDraw
          ++
          quesButtonDraw
          ++
          (helpMsgDraw model.helpImg)
          ++
          (levelDraw model.currLevel model.maxLevel)
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
    WindowSize { width, height } ->                                               
      ( { model | windSize = (Window.Size (width) (height)) }, Cmd.none )   
    Tick time ->
      tickMsgProccessing model


-- process mouse message                                                        
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )
clickMsgProccessing model pos =
  if pos.y >= 285 && pos.y <= 355 && pos.x >= 523 && pos.x <= 593
     then (clickRunProccessing model)
  else if pos.y >= 292 && pos.y <= 347 && pos.x >= 624 && pos.x <= 654
          then (clickHelpProccessing model)
  else if pos.y < 0 || pos.y > mainRectH || pos.x < 0 || pos.x > mainRectW      
          then (model, Cmd.none)
  else (model, Cmd.none)   


clickHelpProccessing : Model -> ( Model, Cmd Msg )                                 
clickHelpProccessing model =                                                       
  if model.helpImg == " "                                                       
    then ({model | helpImg = "../img/help.png"}, Cmd.none)                      
  else ({model | helpImg = " "}, Cmd.none) 


clickRunProccessing : Model -> ( Model, Cmd Msg )                                                    
clickRunProccessing model =                                                        
  ( (getAllCfgs model)                                                          
    |> setPushFlag                                                              
    |> updCatParam  
  , Cmd.none                                                                    
  )                                                                             
                                                                                
                                                                                
tickMsgProccessing : Model -> ( Model, Cmd Msg )                                 
tickMsgProccessing model =                                                       
  if model.ifPushRun == True then
    if (length model.machineCfgs) == 1 then 
       (checkResult model) 
    else if (length model.machineCfgs) > 0 then  
            (
              (getNextCfg model)
              |> updCatParam
            , Cmd.none
            )
    else                                                                        
      ( {model | ifPushRun = False}, Cmd.none )                                 
  else                                                                          
    (model, Cmd.none)  

-------------------------------------------------------------------------------

getResTape : Model -> List (Maybe BallOfWool)   
getResTape model =
  (head model.machineCfgs)
  |> getTapeFromCfg


{-
getResState : Model -> Kitten
getResState m =
  let
    {currState, currDir, tapeCfg} = (getHeadCfg m.machineCfgs)
  in
    currState
-}


ifCorrect : Model -> Bool
ifCorrect m =
  if (getResTape m) == m.expRes --&& (getResState m) == m.machine.acceptState
     then True
  else False


checkResult : Model -> ( Model, Cmd Msg )  
checkResult m =
  if (ifCorrect m) && (m.currLevel == m.maxLevel) 
     then ( { m
                | finalImg = "../img/finalImg/final.png"
                , ifEnd = True
            }
          , Cmd.none
          )
  else if (ifCorrect m) 
     then ( { m                                                                 
                | finalImg = "../img/finalImg/pos.jpg"                          
                , ifEnd = True                                                  
            }                                                                   
          , Cmd.none                                                            
          ) 
  else ( { m 
             | finalImg = "../img/finalImg/neg.png"
             , ifEnd = True 
         }
       , Cmd.none
       )  


getCatColour : Kitten -> String
getCatColour state =
  case state of
    White -> "W"
    LightGrey -> "LG"
    Grey -> "G"
    Orange -> "O"
    Violet -> "V"


emptyTape : TapeCfg BallOfWool                                                  
emptyTape =                                                                     
  { leftSyms = empty                                                            
  , currSym = Nothing                                                           
  , rightSyms = empty                                                           
  }                                                                             
                                                                                
                                                                                
emptyMCfg : MachineCfg BallOfWool Kitten                                        
emptyMCfg =                                                                     
  { currState = White                                                           
  , currDir = Stay                                                              
  , tapeCfg  = emptyTape                                                        
  }                                                                             
                                                                                
                                                                                
getHeadCfg : Model -> MachineCfg BallOfWool Kitten                              
getHeadCfg model =                                                              
    case (head (model.machineCfgs)) of                                          
      Just c -> c                                                               
      Nothing -> emptyMCfg   


getAllCfgs : Model -> Model
getAllCfgs model =
  let
  initCfg = (getHeadCfg model)
  in
    { model | machineCfgs = (runMachine model.machine initCfg [initCfg]) 
    }


getNextCfg : Model -> Model                                        
getNextCfg model =                                                              
  { model | machineCfgs = (drop 1 model.machineCfgs) }


updCatParam : Model -> Model 
updCatParam model =
  let
    cfg = (getHeadCfg model)
  in
    { model 
        | catImg = "../img/saimonPush/SaimonPush" ++
                   (getCatColour cfg.currState) ++ ".png"
        , catLeft = 55
        , catPos = model.catPos + (getCurPosForCat cfg)
    }


setPushFlag : Model -> Model
setPushFlag model = 
  { model | ifPushRun = True }                                                  

-------------------------------------------------------------------------------
                                                                                
-- SUBSCRIPTIONS                                                                
subscriptions : Model -> Sub Msg                                                
subscriptions model =                                                           
  Sub.batch                                                                     
    [ Mouse.clicks Click                                                        
    , Window.resizes WindowSize                                                 
    , every second Tick
    ] 


-- MAIN                                                                         
main : Program Never
main =                                                                          
  Html.App.program                                                              
    { init = (init machine transTable input expectedResult) 
    , view = view                                                               
    , update = update                                                           
    , subscriptions = subscriptions                                             
    }  

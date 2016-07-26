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
import Mouse exposing (clicks, moves) 

import List exposing (head, drop, length, take)
import Array exposing (empty, toList)
import Time exposing (every, second, millisecond, Time)

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
  -- machine
  , input        : List (Maybe BallOfWool) 
  , machine      : Machine BallOfWool Kitten
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)
  --tables
  , trTableInit  : TransTable BallOfWool Kitten
  , trTableUser  : TransTable BallOfWool Kitten
  --pictures
  , catLeft      : Int    -- different for catImg
  , menuCatTop  : Int
  , catPos       : Int
  , catImg       : String -- catPush, catThink
  , helpImg      : String -- help text
  , finalImg     : String     
  --levels and result
  , currLevel    : Int
  , maxLevel     : Int
  -- expected results
  , expPos       : Int 
  , expRes       : List (Maybe BallOfWool)
  -- flags
  , ifPushRun    : Bool                                                     
  , ifStart      : Bool                                                       
  , ifPlay       : Bool                                                      
  , ifRules      : Bool                                                      
  , ifAuthors    : Bool
  , ifEnd        : Bool
  -- options
  , timeUnit     : Time 
  --, catsImg      : List String -- cats that used on current level
  --, ballsImg     : List String -- balls that used on current level
  }                                                                             
                                                                                
                                                                                
type alias Position =                                                           
  { x : Int, y : Int }                                                          
                                                                                
                                                                                
-- MESSAGES                                                                     
type Msg
  = Click Position
  | Move Position
  | WindowSize Window.Size
  | Tick Time
                                                                                
     
initModel : Machine BallOfWool Kitten -> TransTable BallOfWool Kitten -> 
            List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> Int -> Model
initModel machine table inp expRes level =
  { windSize = (Window.Size 1855 980)
    , input = inp
    , machine = machine                                                                   
    , machineCfgs = [(initMachineCfg machine inp machine.initHeadPosForMach)]                 
    , trTableInit = table                                                                     
    , trTableUser = table                                                                     
    , catLeft = 45      
    , menuCatTop = 180
    , catPos = machine.initHeadPosForDraw
    , catImg = "../img/saimonThink/SaimonThinkW.png"                                     
    , helpImg = " "                       
    , finalImg = " "
    , currLevel = level
    , maxLevel = 5
    , expPos = 1 -- we can get it as parameter
    , expRes = expRes
    , ifPushRun = False
    , ifStart = True
    , ifPlay = False
    , ifRules = False
    , ifAuthors = False
    , ifEnd = False
    , timeUnit = second
  }


init : Machine BallOfWool Kitten -> TransTable BallOfWool Kitten -> 
       List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> Int -> 
       (Model, Cmd Msg) 
init machine table inp expRes level =                                                                          
  ( (initModel machine table inp expRes level)
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


divStyle : Model -> String -> Html.Attribute Msg
divStyle model color =
  Html.Attributes.style                                                   
    [ ( "position", "absolute" )                                          
    , ( "top", (topMargin model) )                                        
    , ( "left", (leftMargin model) )                                      
    , ( "width", (toString mainRectW) )                                   
    , ( "height", (toString mainRectH) )                                  
    , ( "border", "1px solid #000000" )                                   
    , ( "background-color", color )                                      
    ]  


svgStyle : Model -> List (Svg.Attribute msg)                                    
svgStyle model =                                                                
  [ version "1.1"                                                               
  , Svg.Attributes.width  (toString mainRectW)                                  
  , Svg.Attributes.height (toString mainRectH)                                  
  , x "0"                                                                       
  , y "0"                                                                       
  , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH))     
  ]                                                                             


fullScreenImg : String -> List (Svg msg)
fullScreenImg href =
  [ Svg.image                                                           
      [ x "0px"                                                         
      , y "0px"                                                         
      , Svg.Attributes.width  "800px"
      , Svg.Attributes.height "800px"
      , xlinkHref href                                  
      ]                                                                 
      []                                                                
  ] 


menuCatImg : Model -> List (Svg msg)
menuCatImg m =
  [ Svg.image                                                                   
      [ x "60px"                                                                 
      , y (toString m.menuCatTop ++ "px")                                                               
      , Svg.Attributes.width  "300px"                                           
      , Svg.Attributes.height "250px"                                           
      , xlinkHref "../img/menuCat.png"                                                          
      ]                                                                         
      []                                                                        
  ] 


menuDiv : Model -> List (Html Msg)
menuDiv model =
  [ div
      [ (divStyle model "#ff1f15") ]
      [ Svg.svg                                                                 
          (svgStyle model)                                                      
          ( (fullScreenImg "../img/windows/menu2.jpg") 
            ++
            (menuCatImg model)
          )
      ] 
  ]


gameDiv : Model -> List (Html Msg)
gameDiv model =
  [ div                                                                       
      [ (divStyle model "grey") ]                                                                       
      [ (addMainPanel model) ]                                                
  ] 


rulesDiv : Model -> List (Html Msg)                                             
rulesDiv model =                                                                
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ Svg.svg                                                                 
          (svgStyle model)                                                      
          (fullScreenImg "../img/windows/rules.jpg")                                     
      ]                                                                         
  ] 


authorsDiv : Model -> List (Html Msg)
authorsDiv model =
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ Svg.svg                                                                 
          (svgStyle model)                                                      
          (fullScreenImg "../img/windows/authors.jpg")                                     
      ]                                                                         
  ] 


finalDiv : Model -> List (Html Msg)
finalDiv model =
  [ div                                                                       
      [ (divStyle model "#ff1f15") ]                                                                       
      [ Svg.svg 
          (svgStyle model)
          (fullScreenImg model.finalImg)
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


tableDraw : List (Svg msg)
tableDraw =
  (fullScreenImg "../img/table.jpg")


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
        , xlinkHref ("../img/transTables/transTable1.png")                        
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


runFastDraw : List (Svg msg)                                                  
runFastDraw =                                                                 
  [ Svg.image                                                                   
        [ x "449px"                                                             
        , y "285px"                                                             
        , Svg.Attributes.width "70px"                                           
        , Svg.Attributes.height "70px"                                          
        , xlinkHref ("../img/elements/runFast.png")                                 
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
        (svgStyle model)                                                                          
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
          runFastDraw
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
    Move pos ->
      (getPosition model pos)                                                   
      |> moveMsgProccessing model   
    WindowSize { width, height } ->                                               
      ( { model | windSize = (Window.Size (width) (height)) }, Cmd.none )   
    Tick time ->
      tickMsgProccessing model


-- process mouse message                                                        
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )
clickMsgProccessing m pos =
  if m.ifStart == True 
     then if pos.y >= 190 && pos.y <= 217 && pos.x >= 360 && pos.x <= 447
                  then ( { m 
                            | ifStart = False
                            , ifPlay = True
                          }
                       , Cmd.none
                       ) 
          else if pos.y >= 247 && pos.y <= 274 && pos.x >= 360 && pos.x <= 465
                  then ( { m                                                
                            | ifStart = False
                            , ifRules = True
                          }                                                     
                       , Cmd.none                                               
                       )   
          else if pos.y >= 300 && pos.y <= 327 && pos.x >= 360 && pos.x <= 713
                  then ( { m    
                            | ifStart = False                                      
                            , ifAuthors = True 
                          }                                                     
                       , Cmd.none                                               
                       ) 
          else (m, Cmd.none)
  else if m.ifAuthors == True || m.ifRules == True
     then if pos.y >= 520 && pos.y <= 570 && pos.x >= 515 && pos.x <= 765 
                  then ( { m                                                    
                            | ifStart = True
                            , ifAuthors = False
                            , ifRules = False                                    
                          }                                                     
                       , Cmd.none                                               
                       )   
          else (m, Cmd.none)
  else if m.ifEnd == True && m.currLevel == m.maxLevel
     then if pos.y >= 350 && pos.y <= 380 && pos.x >= 155 && pos.x <= 680
                  then ( (initModel m.machine m.trTableInit m.input m.expRes 1)
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none)  
  else if m.ifEnd == True && m.finalImg == "../img/finalImg/pos.jpg"
     then if pos.y >= 170 && pos.y <= 200 && pos.x >= 190 && pos.x <= 670       
                  then ( (initModel m.machine m.trTableInit m.input m.expRes m.currLevel)   
                         |> contPlayModel 
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none)   
  else if m.ifEnd == True && m.finalImg == "../img/finalImg/neg.png"
     then if pos.y >= 520 && pos.y <= 560 && pos.x >= 315 && pos.x <= 668
                  then ( (initModel m.machine m.trTableInit m.input m.expRes m.currLevel)
                         |> contPlayModel                                      
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none) 
  else if m.ifPlay == True 
     then if pos.y >= 285 && pos.y <= 355 && pos.x >= 523 && pos.x <= 593
                  then (clickRunProccessing m second)
          else if pos.y >= 285 && pos.y <= 355 && pos.x >= 449 && pos.x <= 519
                  then (clickRunProccessing m millisecond)
          else if pos.y >= 292 && pos.y <= 347 && pos.x >= 624 && pos.x <= 654
                  then (clickHelpProccessing m)
          else (m, Cmd.none)
  else (m, Cmd.none)   


contPlayModel : Model -> Model
contPlayModel model =
  { model
      | ifStart = False
      , ifPlay = True
      , ifEnd = False
  }


moveMsgProccessing : Model -> Position -> ( Model, Cmd Msg ) 
moveMsgProccessing m pos =
  if m.ifStart == True                                                             
     then if pos.y >= 190 && pos.y <= 217 && pos.x >= 360 && pos.x <= 447           
                  then ( { m                        
                            | menuCatTop = 180
                          }                                                        
                       , Cmd.none                                                  
                       )         
          else if pos.y >= 247 && pos.y <= 274 && pos.x >= 360 && pos.x <= 465
                  then ( { m          
                            | menuCatTop = 240 
                          }                                                        
                       , Cmd.none       
                       )                
          else if pos.y >= 300 && pos.y <= 327 && pos.x >= 360 && pos.x <= 713
                  then ( { m                                                    
                            | menuCatTop = 300                                  
                          }                                                     
                       , Cmd.none                                               
                       ) 
          else (m, Cmd.none) 
  else (m, Cmd.none)


clickHelpProccessing : Model -> ( Model, Cmd Msg )                                 
clickHelpProccessing model =                                                       
  if model.helpImg == " "                                                       
    then ({model | helpImg = "../img/help.png"}, Cmd.none)                      
  else ({model | helpImg = " "}, Cmd.none) 


clickRunProccessing : Model -> Time -> ( Model, Cmd Msg )                                                    
clickRunProccessing model time =                                                        
  ( (getAllCfgs model)                                                          
    |> setPushFlag                                                              
    |> updCatParam  
    |> setTime time
  , Cmd.none                                                                    
  )                                                                             
                                                                                
                                                                                
tickMsgProccessing : Model -> ( Model, Cmd Msg )                                 
tickMsgProccessing model =                                                       
  if model.ifPushRun == True 
     then if (length model.machineCfgs) > 1 
             then ( (getNextCfg model)
                    |> updCatParam
                  , Cmd.none
                  )
          else (checkResult model)   
  else                                                                          
    (model, Cmd.none)  

-------------------------------------------------------------------------------

getResTape : Model -> List (Maybe BallOfWool)   
getResTape model =
  (head model.machineCfgs)
  |> getTapeFromCfg


getResState : Model -> Kitten
getResState m =
  let
    {currState, currDir, tapeCfg} = (getHeadCfg m)
  in
    currState


ifCorrect : Model -> Bool
ifCorrect m =
  if (getResTape m) == m.expRes && 
     (getResState m) == m.machine.acceptState &&
     m.catPos == m.expPos
     then True
  else False


resultModel : Model -> String -> Int -> (Model, Cmd Msg)
resultModel model href level =
  ( { model     
        | finalImg = href
        , currLevel = level
        , ifEnd = True
        , ifPlay = False
        , ifPushRun = False
    }  
  , Cmd.none
  )


checkResult : Model -> (Model, Cmd Msg)  
checkResult m =
  let 
    newLev = m.currLevel + 1
  in
    if (ifCorrect m) && newLev == m.maxLevel 
       then (resultModel m "../img/finalImg/final.png" newLev)
    else if (ifCorrect m) && newLev < m.maxLevel 
       then (resultModel m "../img/finalImg/pos.jpg" newLev)                                               
    else (resultModel m "../img/finalImg/neg.png" m.currLevel)


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


setTime : Time -> Model -> Model
setTime time model =
  { model
      | timeUnit = time
  }

-------------------------------------------------------------------------------
                                                                                
-- SUBSCRIPTIONS                                                                
subscriptions : Model -> Sub Msg                                                
subscriptions model =                                                           
  Sub.batch                                                                     
    [ Mouse.clicks Click  
    , Mouse.moves Move
    , Window.resizes WindowSize                                                 
    , every model.timeUnit Tick
    ] 


-- MAIN                                                                         
main : Program Never
main =                                                                          
  Html.App.program                                                              
    { init = (init machine transTable input expectedResult 1) 
    , view = view                                                               
    , update = update                                                           
    , subscriptions = subscriptions                      
    }  

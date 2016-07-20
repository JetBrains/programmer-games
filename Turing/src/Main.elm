import Html exposing (Html, div, text)
import Html.Attributes exposing (..)                                            
import Html.App  

import TuringTypes exposing ( Direction(..), Machine, MachineCfg, TapeCfg, 
                              TransTable )
import RunTuring exposing (debugRun, transFunc)                          
import InitUpdate exposing (initMachineCfg)

import Svg exposing (Svg)                                                       
import Svg.Attributes exposing (..)                                             

import Window                                                                   
import Task                                                                     
import Mouse  

import List exposing (head, drop)
import Array exposing (empty)


------------------------------------------------------------------------------
type BallOfWool = Red | Yellow | Green | Blue -- a
--white is start, orange is natural, violet is reject
type Kitten = White | LightGrey | Grey | Orange | Violet -- b 


machine : Machine BallOfWool Kitten
machine =
  { transition = (transFunc transTable (Violet, Nothing, MoveLeft))
  , initHeadPos = 1   
  , startState = White
  , acceptState = Orange 
  , rejectState = Violet
  }


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
      value = (Grey, Just Red, MoveLeft)}
  , { key = (Grey, Just Red), 
      value = (Grey, Just Red, MoveLeft)}
  , { key = (Grey, Just Yellow), 
      value = (Grey, Just Yellow, MoveLeft)}
  , { key = (Grey, Just Green), 
      value = (Grey, Just Green, MoveLeft)}
  , { key = (Grey, Just Blue), 
      value = (Grey, Just Blue, MoveLeft)}
  , { key = (Grey, Nothing), 
      value = (Orange, Just Blue, MoveRight)}
  ] 


input : List (Maybe BallOfWool)
input = 
  [Just Red, Nothing, Just Yellow, Just Green, Just Blue]

------------------------------------------------------------------------------

-- MODEL                                                                        
type alias Model =                                                              
  { windSize     : Window.Size                                                          
  , clickPos     : Position
  , machine      : Machine BallOfWool Kitten
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)
  , transTable   : TransTable BallOfWool Kitten
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
      (Position 0 0) 
      machine
      [(initMachineCfg machine inp machine.initHeadPos)]
      table
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
basketLeftMarginI ind = ind * 100 + (ind+1)*13


basketTopMarginI : Int                                                       
basketTopMarginI = 410


basketLeftMarginS : Int -> String
basketLeftMarginS ind =
  toString (basketLeftMarginI ind) ++ "px"


basketTopMarginS : String
basketTopMarginS =
  (toString basketTopMarginI) ++ "px"


newBasket : Int -> Svg msg
newBasket ind =
  Svg.image                                                            
    [ x (basketLeftMarginS ind)                                                       
    , y basketTopMarginS                                            
    , Svg.Attributes.width "100px"                                   
    , Svg.Attributes.height "100px"                                  
    , xlinkHref ("../img/basket/basket.png")                                
    ]                                                                
    []    


basketCreate : Int -> List (Svg msg) -> List (Svg msg)
basketCreate n res =
  if n > 0 then ( basketCreate (n-1) (res ++ [newBasket (n-1)]) )
  else res


tableW : String                                                                 
tableW = (toString mainRectW)                                                   


tableH : String                                                                 
tableH = (toString mainRectW) 


tableCreate : List (Svg msg)
tableCreate =
  [ Svg.image                                                            
      [ x "0"                                                          
      , y "0"                                                          
      , Svg.Attributes.width tableW                                    
      , Svg.Attributes.height tableH                                   
      , xlinkHref ("../img/table.jpg")                                 
      ]                                                                
      [] 
  ]


mirrorCreate : List (Svg msg) 
mirrorCreate =
  [ Svg.image                                                            
      [ x "30"                                                         
      , y "50"                                                         
      , Svg.Attributes.width "335px"                                   
      , Svg.Attributes.height "270px"                                  
      , xlinkHref ("../img/mirror/mirror2Empty.png")                               
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


newBall : Int -> Maybe (Maybe BallOfWool) -> Svg msg
newBall ind inpVal =
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


getBallColor : Maybe (Maybe BallOfWool) -> String
getBallColor inpVal =
  case inpVal of
    Just (Just Red) -> "Red"
    Just (Just Yellow) -> "Yellow"
    Just (Just Green) -> "Green"
    Just (Just Blue) -> "Blue"
    _ -> "Black"


ballCreate : Int -> List (Svg msg) -> List (Maybe BallOfWool) -> Int 
             -> List (Svg msg)                          
ballCreate n res tape hpos =              
  let 
    inpVal = head (drop (n-1) tape)
  in
    if n > 0 
       then if inpVal == Just (Nothing) 
               then (ballCreate (n-1) res tape hpos)
               else 
                 let
                   updRes = (res ++ [newBall (n-1+hpos) inpVal]) 
                 in 
                   (ballCreate (n-1) updRes tape hpos)               
    else res  


catLeftMarginI : Int -> Int                                                    
catLeftMarginI hpos =                                                           
    (basketLeftMarginI hpos) - 55 


catTopMarginI : Int                                                            
catTopMarginI = 
  basketTopMarginI + 35


catLeftMarginS : Int -> String                                                 
catLeftMarginS ind =                                                           
    (toString (catLeftMarginI ind) ++ "px")                                      


catTopMarginS : String                                                         
catTopMarginS =                                                                
     (toString (catTopMarginI) ++ "px") 


catCreate : Int -> List (Svg msg)
catCreate hpos =
  [ Svg.image                                                             
        [ x (catLeftMarginS hpos)                                                         
        , y catTopMarginS                                                        
        , Svg.Attributes.width "155px"                                   
        , Svg.Attributes.height "155px"                                  
        , xlinkHref ("../img/saimonPush/SaimonPushW.png")
        --, xlinkHref ("../img/saimonThink/GreySaimonThink.png")
        ]                                                                
        []
  ]


emptyTape : TapeCfg BallOfWool
emptyTape =
  { leftSyms = empty
  , currSym = Nothing
  , rightSyms = empty
  }                                                                             
                                                                                          
                                                                                
emptyMCfg : MachineCfg BallOfWool Kitten 
emptyMCfg =                                                     
  { currState = White
  , tapeCfg = emptyTape                                                       
  } 


transTableDraw : List (Svg msg)
transTableDraw =
  [ Svg.image                                                                   
        [ x "410px"                                             
        , y "20px"                                                    
        , Svg.Attributes.width "460px"                                          
        , Svg.Attributes.height "250px"                                         
        , xlinkHref ("../img/transTable1.png")                        
        ]                                                                       
        []                                                                      
  ]  


runButtonCreate : List (Svg msg)
runButtonCreate =
  [ Svg.image                                                                   
        [ x "603px"                                                             
        , y "285px"                                                              
        , Svg.Attributes.width "70px"                                          
        , Svg.Attributes.height "70px"                                         
        , xlinkHref ("../img/elements/run2.png")                                  
        ]                                                                       
        []                                                                      
  ]


addMainPanel : Model -> Html Msg                                                
addMainPanel model =     
  let
    {-
    tape = case (head (model.machineCfgs)) of
              Just ini -> ini
              Nothing -> emptyMCfg 
    -}
    tape = [Just Red, Nothing, Just Yellow, Just Green, Just Blue]
    hpos = model.machine.initHeadPos
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
          tableCreate
          ++
          mirrorCreate
          ++
          (basketCreate 7 [])
          ++
          (ballCreate 7 [] tape hpos)
          ++
          (catCreate hpos)
          ++
          (transTableDraw)
          ++
          (runButtonCreate)
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
        ( { model | clickPos = p }, Cmd.none)                                                
    WindowSize { width, height } ->                                               
        ( { model | windSize = (Window.Size (width) (height)) }, Cmd.none )   


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

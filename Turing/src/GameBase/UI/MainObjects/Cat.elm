module GameBase.UI.MainObjects.Cat exposing 
  (gameCatDraw, lookingCatDraw, menuCatDraw, updCatParam, menuCatY, catThinkX)

import GameBase.UI.MainObjects.Basket exposing (basketX, basketY)
import GameBase.UI.ControlObjects.ControlLabelsParam exposing                   
                                    (fstItemTopFrom, menuItemsInterval)       
import GameBase.Data.GameTypes exposing (BallOfWool, Kitten(..), Model)
import TuringMachine.TuringTypes exposing (MachineCfg, Direction(..)) 
import GameBase.Proccessing.WorkWithCfg exposing (getHeadCfg)

import Svg exposing (Svg, image)                                                
import Svg.Attributes exposing (width, height, x, y, xlinkHref)                 
import Time exposing (Time) 


--GAME CAT PARAMETERS--------------------------------------------
gameCatY : Int                                                             
gameCatY =                                                                 
  basketY + 35                                                         


gameCatX : Int -> Int -> Int -> Int                                          
gameCatX basketInd basketsNumb left =                                                   
  (basketX basketInd basketsNumb) - left


gameCatW : Int
gameCatW = 165


gameCatH : Int
gameCatH = 165
----------------------------------------------------------------


--LOOKING CAT PARAMETERS----------------------------------------
lookingCatY : Int
lookingCatY = 280 


lookingCatX : Int
lookingCatX = 0


lookingCatW : Int
lookingCatW = 513


lookingCatH : Int
lookingCatH = 509
---------------------------------------------------------------


--MENU CAT PARAMETERS------------------------------------------
menuCatY : Int -> Int
menuCatY itemInd =
  fstItemTopFrom + itemInd * menuItemsInterval


menuCatX : Int
menuCatX = 160


menuCatW : Int
menuCatW = 300


menuCatH : Int
menuCatH = 250
---------------------------------------------------------------


--CAT PROCCESSING FUNC-----------------------------------------
getCurCatPos : MachineCfg BallOfWool Kitten -> Int                           
getCurCatPos cfg = 
  case cfg.currDir of                                                           
    MoveRight -> 1                                                              
    MoveLeft -> -1                                                              
    Stay -> 0  


getCatColour : Kitten -> String                                                 
getCatColour state =                                                            
  case state of                                                                 
    White -> "W"                                                                
    LightGrey -> "LG"                                                           
    Grey -> "G"   
    Brown -> "B"
    Orange -> "O"                                                               
    Violet -> "V" 


catPushX : Int
catPushX = 55


catThinkX : Int
catThinkX = 45


updCatParam : Time -> Model -> Model                                            
updCatParam time model =                                                        
  let                                                                           
    cfg = (getHeadCfg model)                                                    
  in                                                                            
    { model                                                                     
        | imgParam =
            { catLeft = catPushX
            , menuCatTop = model.imgParam.menuCatTop
            , catPos = model.imgParam.catPos + (getCurCatPos cfg) 
            , catImg = "../img/saimonPush/SaimonPush" ++ 
                       (getCatColour cfg.currState) ++ ".png"
            , helpImg = model.imgParam.helpImg
            , finalImg = model.imgParam.finalImg
            }
        , options = 
            { winSize = model.options.winSize
            , timeUnit = model.options.timeUnit
            , whenGameStarts = model.options.whenGameStarts
            , currTime = time
            , tapeCellsNumb = model.options.tapeCellsNumb
            }                                                        
    } 
---------------------------------------------------------------                  


--DRAW FUNCTIONS-----------------------------------------------                                       
gameCatDraw : Model -> List (Svg msg)                                               
gameCatDraw m =                                                                 
  let                                                                           
    href = m.imgParam.catImg                                                         
    left = m.imgParam.catLeft                                                        
    basketInd = m.imgParam.catPos                                                         
  in                                                                            
    [image                                                                 
       [ x (toString (gameCatX basketInd m.options.tapeCellsNumb left) ++ "px")
       , y (toString (gameCatY) ++ "px") 
       , width  ((toString gameCatW) ++ "px")                                        
       , height ((toString gameCatH) ++ "px")                                       
       , xlinkHref (href)                                                    
       ]                                                                     
       []                                                                    
    ]   

                                                                               
lookingCatDraw : Model -> List (Svg msg)                                          
lookingCatDraw m =                                                                
  if m.flags.ifCatLooks == True then                                                  
    [ image                                                                 
        [ x ((toString lookingCatX) ++ "px") 
        , y ((toString lookingCatY) ++  "px")
        , width  ((toString lookingCatW) ++ "px")
        , height ((toString lookingCatH) ++ "px")
        , xlinkHref "../img/catLooks.png"                                       
        ]                                                                       
        []                                                                      
    ]                                                                           
  else []   


menuCatDraw : Model -> List (Svg msg)                                            
menuCatDraw m =                                                                  
  [ image                                                                   
      [ x ((toString menuCatX) ++ "px")                                                                
      , y ((toString m.imgParam.menuCatTop) ++ "px") 
      , width  ((toString menuCatW) ++ "px")                                          
      , height ((toString menuCatH) ++ "px")                                           
      , xlinkHref "../img/menuCat.png"                                          
      ]                                                                         
      []                                                                        
  ]

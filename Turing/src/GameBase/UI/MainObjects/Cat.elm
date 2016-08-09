module GameBase.UI.MainObjects.Cat exposing 
          (gameCatDraw, lookingCatDraw, menuCatDraw, updCatParam, menuCatY, 
           catThinkX, catShowSndItemY)

import GameBase.UI.MainObjects.Basket exposing (basketX, basketY)
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


gameCatX : Int -> Int -> Int                                           
gameCatX hpos left =                                                   
  (basketX hpos) - left


gameCatW : Int
gameCatW = 155


gameCatH : Int
gameCatH = 155
----------------------------------------------------------------


--LOOKING CAT PARAMETERS----------------------------------------
lookingCatY : Int
lookingCatY = 200


lookingCatX : Int
lookingCatX = 0


lookingCatW : Int
lookingCatW = 400


lookingCatH : Int
lookingCatH = 397
---------------------------------------------------------------


--MENU CAT PARAMETERS------------------------------------------
catShowFstItemY : Int
catShowFstItemY = 180


catShowSndItemY : Int                                                     
catShowSndItemY = 240 


menuItemsInterval : Int
menuItemsInterval = 60


menuCatY : Int -> Int
menuCatY itemInd =
  catShowFstItemY + itemInd * menuItemsInterval


menuCatX : Int
menuCatX = 60


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
            }                                                        
    } 
---------------------------------------------------------------                  


--DRAW FUNCTIONS-----------------------------------------------                                       
gameCatDraw : Model -> List (Svg msg)                                               
gameCatDraw model =                                                                 
  let                                                                           
    href = model.imgParam.catImg                                                         
    left = model.imgParam.catLeft                                                        
    hpos = model.imgParam.catPos                                                         
  in                                                                            
    [image                                                                 
       [ x (toString (gameCatX hpos left) ++ "px")
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

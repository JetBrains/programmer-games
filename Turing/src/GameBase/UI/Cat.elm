module GameBase.UI.Cat exposing (gameCatDraw, catLooksDraw, menuCatDraw, 
                                 updCatParam)

import Svg exposing (Svg, image) 
import Svg.Attributes exposing (width, height, x, y, xlinkHref)                 
import Time exposing (Time) 

import GameBase.UI.Basket exposing (basketLeftMarginI, basketTopMarginI)
import GameBase.Data.GameTypes exposing (BallOfWool, Kitten(..), Model)
import TuringMachine.TuringTypes exposing (MachineCfg, Direction(..)) 
import GameBase.Proccessing.WorkWithCfg exposing (getHeadCfg)


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
                                                                                
                       
getCurPosForCat : MachineCfg BallOfWool Kitten -> Int                           
getCurPosForCat cfg =                                                           
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


updCatParam : Time -> Model -> Model                                            
updCatParam time model =                                                        
  let                                                                           
    cfg = (getHeadCfg model)                                                    
  in                                                                            
    { model                                                                     
        | catImg = "../img/saimonPush/SaimonPush" ++                            
                   (getCatColour cfg.currState) ++ ".png"                       
        , catLeft = 55                                                          
        , catPos = model.catPos + (getCurPosForCat cfg)                         
        , currTime = time                                                       
    } 
                  
                                       
gameCatDraw : Model -> List (Svg msg)                                               
gameCatDraw model =                                                                 
  let                                                                           
    href = model.catImg                                                         
    left = model.catLeft                                                        
    hpos = model.catPos                                                         
  in                                                                            
    [image                                                                 
       [ x (catLeftMarginS hpos left)                                        
       , y catTopMarginS                                                     
       , width "155px"                                        
       , height "155px"                                       
       , xlinkHref (href)                                                    
       ]                                                                     
       []                                                                    
    ]   

                                                                               
catLooksDraw : Model -> List (Svg msg)                                          
catLooksDraw m =                                                                
  if m.ifCatLooks == True then                                                  
    [ image                                                                 
        [ x "0px"                                                               
        , y "200px"                                                             
        , width "400px"                                          
        , height "397px"                                         
        , xlinkHref "../img/catLooks.png"                                       
        ]                                                                       
        []                                                                      
    ]                                                                           
  else []   


menuCatDraw : Model -> List (Svg msg)                                            
menuCatDraw m =                                                                  
  [ image                                                                   
      [ x "60px"                                                                
      , y (toString m.menuCatTop ++ "px")                                       
      , width  "300px"                                           
      , height "250px"                                           
      , xlinkHref "../img/menuCat.png"                                          
      ]                                                                         
      []                                                                        
  ]

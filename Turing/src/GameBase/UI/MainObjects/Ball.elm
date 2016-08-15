module GameBase.UI.MainObjects.Ball exposing (ballsOfOneCfgDraw)

import Svg exposing (Svg, image) 
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 
import List exposing (head, drop)

import GameBase.Data.GameTypes exposing (BallOfWool(..))
import GameBase.UI.MainObjects.Basket exposing (basketX, basketY)


ballX : Int -> Int -> Int
ballX ballInd ballsNumb =
  (basketX ballInd ballsNumb) + 27


ballY : Int
ballY =
  (basketY) + 15
                                                                                

ballW : Int
ballW = 50


ballH : Int
ballH = 50


getBallColor : Maybe (Maybe BallOfWool) -> String                               
getBallColor inpVal =                                                           
  case inpVal of                                                                
    Just (Just Red) -> "Red"                                                    
    Just (Just Yellow) -> "Yellow"                                              
    Just (Just Green) -> "Green"                                                
    Just (Just Blue) -> "Blue"                                                  
    _ -> "Transp"                                                               
                                                                                
                                                                                
getNewBall : Int -> Int -> Maybe (Maybe BallOfWool) -> Svg msg                         
getNewBall ballInd ballsNumb inpVal =                                                         
  let                                                                           
    color = (getBallColor inpVal)                                               
  in                                                                            
    image                                                                   
      [ x (toString (ballX ballInd ballsNumb) ++ "px")                                               
      , y (toString (ballY) ++ "px")                                                    
      , width ((toString ballW) ++ "px")                                         
      , height ((toString ballH) ++ "px")                                          
      , xlinkHref ("../img/balls/" ++ color ++ "Ball.png")                    
      ]                                                                       
      []     


getInpV : List (Maybe BallOfWool) -> Int -> Maybe (Maybe BallOfWool)            
getInpV tape ballInd =                                                                
  head (drop (ballInd-1) tape)                                                        
                                                                                
                                                                                
ballsOfOneCfgDraw : Int -> Int -> List (Svg msg) -> List (Maybe BallOfWool) -> 
                    Int -> List (Svg msg)                                          
ballsOfOneCfgDraw ballsToDraw ballsNumb res tape hpos =
  if ballsToDraw > 0 then
    let 
      updRes = (res ++ [getNewBall (ballsToDraw-1+hpos) 
                                   ballsNumb 
                                   (getInpV tape ballsToDraw)])
    in
      (ballsOfOneCfgDraw (ballsToDraw-1) ballsNumb updRes tape hpos) 
  else res                                                                    

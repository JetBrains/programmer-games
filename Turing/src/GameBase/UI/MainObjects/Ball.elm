module GameBase.UI.MainObjects.Ball exposing (ballsOfOneTapeDraw)

import Svg exposing (Svg, image) 
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 
import List exposing (head, drop)

import GameBase.Data.GameTypes exposing (BallOfWool(..))
import GameBase.UI.MainObjects.Basket exposing (basketX, basketY)


ballX : Int -> Int                                                    
ballX ind =                                                           
  (basketX ind) + 27
                                                                                
                                                                                
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
                                                                                
                                                                                
getNewBall : Int -> Maybe (Maybe BallOfWool) -> Svg msg                         
getNewBall ind inpVal =                                                         
  let                                                                           
    color = (getBallColor inpVal)                                               
  in                                                                            
    image                                                                   
      [ x (toString (ballX ind) ++ "px")                                               
      , y (toString (ballY) ++ "px")                                                    
      , width ((toString ballW) ++ "px")                                         
      , height ((toString ballH) ++ "px")                                          
      , xlinkHref ("../img/balls/" ++ color ++ "Ball.png")                    
      ]                                                                       
      []     


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

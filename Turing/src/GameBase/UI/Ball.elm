module GameBase.UI.Ball exposing (ballsOfOneTapeDraw)

import Svg exposing (Svg)                                                 
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 
import List exposing (head, drop)

import GameBase.Data.GameTypes exposing (BallOfWool(..))
import GameBase.UI.Basket exposing (basketLeftMarginI, basketTopMarginI)

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

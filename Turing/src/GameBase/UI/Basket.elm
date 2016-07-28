module GameBase.UI.Basket exposing (allBasketsDraw, basketLeftMarginI, 
                                    basketTopMarginI)

import Svg exposing (Svg, image)       
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 


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
  image                                                                     
    [ x (basketLeftMarginS ind)                                                 
    , y basketTopMarginS                                                        
    , width "100px"                                              
    , height "100px"                                             
    , xlinkHref ("../img/basket/basket.png")                                    
    ]                                                                           
    []                                                                          
                                                                                
                                                                                
allBasketsDraw : Int -> List (Svg msg) -> List (Svg msg)                        
allBasketsDraw n res =                                                          
  if n > 0 then ( allBasketsDraw (n-1) (res ++ [getNewBasket (n-1)]) )          
  else res  

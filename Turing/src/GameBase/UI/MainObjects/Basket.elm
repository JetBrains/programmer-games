module GameBase.UI.MainObjects.Basket exposing 
          (allBasketsDraw, basketX, basketY)

import Svg exposing (Svg, image)       
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 


basketX : Int -> Int                                                  
basketX ind = ind*70 + (ind+1)*40 


basketY : Int                                                          
basketY = 410                                                          


basketW : Int
basketW = 100


basketH : Int
basketH = 100


getNewBasket : Int -> Svg msg                                                   
getNewBasket ind =                                                              
  image                                                                     
    [ x (toString (basketX ind) ++ "px")                                                 
    , y ((toString basketY) ++ "px")
    , width ((toString basketW) ++ "px")                                              
    , height ((toString basketH) ++ "px")                                             
    , xlinkHref ("../img/basket/basket.png")                                    
    ]                                                                           
    []                                                                          
                                                                                
                                                                                
allBasketsDraw : Int -> List (Svg msg) -> List (Svg msg)                        
allBasketsDraw n res =                                                          
  if n > 0 then ( allBasketsDraw (n-1) (res ++ [getNewBasket (n-1)]) )          
  else res  

module GameBase.UI.MainObjects.Basket exposing (allBasketsDraw, basketX, 
                        basketY, threeBaskets, fourBaskets, fiveBaskets, 
                        sixBaskets, sevenBaskets, eightBaskets, nineBaskets)

import Svg exposing (Svg, image)       
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 


threeBaskets : Int                                                              
threeBaskets = 3 

threeBasketsLeft : Int
threeBasketsLeft = 330

threeBasketsInterval : Int
threeBasketsInterval = 30
---------------------------------
fourBaskets : Int
fourBaskets = 4

fourBasketsLeft : Int
fourBasketsLeft = 265

fourBasketsInterval : Int
fourBasketsInterval = 30
---------------------------------
fiveBaskets : Int
fiveBaskets = 5

fiveBasketsLeft : Int                                                           
fiveBasketsLeft = 200  

fiveBasketsInterval : Int
fiveBasketsInterval = 30
---------------------------------
sixBaskets : Int
sixBaskets = 6

sixBasketsLeft : Int                                                           
sixBasketsLeft = 140   

sixBasketsInterval : Int
sixBasketsInterval = 30
---------------------------------
sevenBaskets : Int
sevenBaskets = 7

sevenBasketsLeft : Int                                                           
sevenBasketsLeft = 70   

sevenBasketsInterval : Int
sevenBasketsInterval = 30
---------------------------------
eightBaskets : Int
eightBaskets = 8

eightBasketsLeft : Int                                                           
eightBasketsLeft = 40   

eightBasketsInterval : Int
eightBasketsInterval = 20
---------------------------------
nineBaskets : Int
nineBaskets = 9

nineBasketsLeft : Int                                                           
nineBasketsLeft = 25

nineBasketsInterval : Int
nineBasketsInterval = 10
---------------------------------

basketX : Int -> Int -> Int 
basketX basketInd basketsNumb =
  case basketsNumb of
    3 -> 
      threeBasketsLeft + basketInd*basketW + basketInd*threeBasketsInterval   
    4 ->
      fourBasketsLeft  + basketInd*basketW + basketInd*fourBasketsInterval
    5 ->
      fiveBasketsLeft  + basketInd*basketW + basketInd*fiveBasketsInterval   
    6 ->
      sixBasketsLeft   + basketInd*basketW + basketInd*sixBasketsInterval   
    7 ->
      sevenBasketsLeft + basketInd*basketW + basketInd*sevenBasketsInterval   
    8 ->
      eightBasketsLeft + basketInd*basketW + basketInd*eightBasketsInterval   
    _ ->
      nineBasketsLeft  + basketInd*basketW + basketInd*nineBasketsInterval   

basketY : Int                                                          
basketY = 525

basketW : Int
basketW = 90

basketH : Int
basketH = 90

oneBasketDraw : Int -> Int -> Svg msg                                                   
oneBasketDraw basketInd basketsNumb =                                                              
  image                                                                     
    [ x (toString (basketX basketInd basketsNumb) ++ "px")                                                 
    , y ((toString basketY) ++ "px")
    , width ((toString basketW) ++ "px")                                              
    , height ((toString basketH) ++ "px")                                             
    , xlinkHref ("../img/basket/basket.png")                                    
    ]                                                                           
    []  

allBasketsDraw : Int -> List (Svg msg) -> Int -> List (Svg msg)                        
allBasketsDraw basketsToDraw res basketsNumb =                                                          
  if basketsToDraw > 0 
    then (allBasketsDraw (basketsToDraw-1) 
                         (res ++ [oneBasketDraw (basketsToDraw-1) basketsNumb])
                         basketsNumb)
  else res  

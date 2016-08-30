module GameBase.UI.MainObjects.DivSvgStyles exposing 
        (divStyle, svgStyle, mainRectW,  mainRectH, fullScreenImg)

import GameBase.Data.GameTypes exposing (Model)

import Html exposing (Html)
import Html.Attributes exposing (style) 
import Svg exposing (Svg, image)
import Svg.Attributes exposing 
                (width, height, x, y, version, viewBox, xlinkHref)


--MAIN DIV PARAMETERS--------------------------------------------
mainRectW : Int                                                                 
mainRectW = 1024
                                                                                

-- 700 px
mainRectH : Int                                                                 
mainRectH = 768                                                                
                                                                                
                                                                                
topMargin : Model -> String                                                     
topMargin model =                                                               
  (toString ((model.options.winSize.height - mainRectH)//2) ++ "px")                   
                                                                                
                                                                                
leftMargin : Model -> String                                                    
leftMargin model =                                                              
  (toString ((model.options.winSize.width - mainRectW)//2) ++ "px")      
-----------------------------------------------------------------


--FULL SCREEN IMG PARAMETERS-------------------------------------
fullScreenImgWH : Int
fullScreenImgWH = 1024


fullScreenImgXY : Int
fullScreenImgXY = 0
-----------------------------------------------------------------


fullScreenImg : String -> List (Svg msg)                                        
fullScreenImg href =                                                            
  [ image                                                                       
      [ x ((toString fullScreenImgXY) ++ "px")                                                                
      , y ((toString fullScreenImgXY) ++ "px")                                                                 
      , width  ((toString fullScreenImgWH) ++ "px")                                                          
      , height ((toString fullScreenImgWH) ++ "px")                                                           
      , xlinkHref href                                                          
      ]                                                                         
      []                                                                        
  ]     


divStyle : Model -> String -> Html.Attribute msg                                
divStyle model color =                                                          
  style                                                                         
    [ ( "position", "absolute" )                                                
    , ( "top", (topMargin model) )                                              
    , ( "left", (leftMargin model) )                                            
    , ( "width", (toString mainRectW) )                                         
    , ( "height", (toString mainRectH) )                                        
    , ( "border", "1px solid #000000" )                                         
    , ( "background-color", color )                                             
    ]                                                                           
                                                                                
                                                                                
svgStyle : List (Svg.Attribute msg)                                    
svgStyle =                                                                
  [ version "1.1"                                                               
  , width  (toString mainRectW)                                  
  , height (toString mainRectH)                                  
  , x "0"                                                                       
  , y "0"                                                                       
  , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH))     
  ] 

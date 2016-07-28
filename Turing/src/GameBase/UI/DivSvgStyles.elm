module GameBase.UI.DivSvgStyles exposing (divStyle, svgStyle, mainRectW, 
                                          mainRectH, fullScreenImg)

import GameBase.Data.GameTypes exposing (Model)

import Html exposing (Html)
import Html.Attributes exposing (style) 
import Svg exposing (Svg, image)
import Svg.Attributes exposing (width, height, x, y, version, viewBox, xlinkHref)  


mainRectW : Int                                                                 
mainRectW = 800                                                                 


mainRectH : Int                                                                 
mainRectH = 600                                                                 


topMargin : Model -> String                                                     
topMargin model =                                                               
  ((toString ((model.windSize.height - mainRectH)//2))++"px")                   


leftMargin : Model -> String                                                    
leftMargin model =                                                              
  ((toString ((model.windSize.width - mainRectW)//2))++"px")                    


fullScreenImg : String -> List (Svg msg)                                        
fullScreenImg href =                                                            
  [ image                                                                       
      [ x "0px"                                                                 
      , y "0px"                                                                 
      , width  "800px"                                                          
      , height "800px"                                                          
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
                                                                                
                                                                                
svgStyle : Model -> List (Svg.Attribute msg)                                    
svgStyle model =                                                                
  [ version "1.1"                                                               
  , width  (toString mainRectW)                                  
  , height (toString mainRectH)                                  
  , x "0"                                                                       
  , y "0"                                                                       
  , viewBox ("0 0 " ++ (toString mainRectW) ++ " " ++ (toString mainRectH))     
  ] 

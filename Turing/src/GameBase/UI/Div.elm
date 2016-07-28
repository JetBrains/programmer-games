module GameBase.UI.Div exposing (menuDiv, gameDiv, rulesDiv, authorsDiv, 
                                 finalDiv)

import Html exposing (Html, div) 
import Svg exposing (Svg, svg) 

import GameBase.Data.GameTypes exposing (Model)
import GameBase.UI.DivSvgStyles exposing (divStyle, svgStyle, fullScreenImg)
import GameBase.UI.Cat exposing (menuCatDraw)
import GameBase.UI.AddMainPanel exposing (addMainPanel)


menuDiv : Model -> List (Html msg)         
menuDiv model =                                                                 
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          (svgStyle model)                                                      
          ( (fullScreenImg "../img/windows/menu.jpg")                           
            ++                                                                  
            (menuCatDraw model)                                                  
          )                                                                     
      ]                                                                         
  ]                                                                             
                                                                                
                                                                                
gameDiv : Model -> List (Html msg)                                              
gameDiv model =                                                                 
  [ div                                                                         
      [ (divStyle model "grey") ]                                               
      [ (addMainPanel model) ]                                                  
  ]                                                                             
                                                                                
                                                                                
rulesDiv : Model -> List (Html msg)                                             
rulesDiv model =                                                                
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          (svgStyle model)                                                      
          (fullScreenImg "../img/windows/rules.jpg")                            
      ]                                                                         
  ]                                                                             
                                                                                
                                                                                
authorsDiv : Model -> List (Html msg)                                           
authorsDiv model =                                                              
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          (svgStyle model)                                                      
          (fullScreenImg "../img/windows/authors.jpg")                          
      ]                                                                         
  ]                                                                             
                                                                                
                                                                                
finalDiv : Model -> List (Html msg)                                             
finalDiv model =                                                                
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          (svgStyle model)                                                      
          (fullScreenImg model.finalImg)                                        
      ]                                                                         
  ]  

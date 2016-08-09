module GameBase.UI.MainObjects.Div exposing 
                 (menuDiv, gameDiv, rulesDiv, authorsDiv, finalDiv)

import GameBase.Data.GameTypes exposing (Model)
import GameBase.UI.MainObjects.DivSvgStyles exposing 
                                (divStyle, svgStyle, fullScreenImg)
import GameBase.UI.MainObjects.Cat exposing (menuCatDraw)
import GameBase.UI.MainObjects.AddMainPanel exposing (addMainPanel)

import Html exposing (Html, div)                                                
import Svg exposing (Svg, svg)     


menuDiv : Model -> List (Html msg)         
menuDiv model = 
  let
    menuImg = 
      if model.levels.currLevel < 2 
         then (fullScreenImg "../img/windows/menuWithoutContinue.jpg") 
      else (fullScreenImg "../img/windows/menu.jpg")   
  in
    [ div                                                                         
        [ (divStyle model "#ff1f15") ]                                            
        [ svg                                                                 
            svgStyle                                                      
            ( menuImg                           
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
          svgStyle                                                      
          (fullScreenImg "../img/windows/rules.jpg")                            
      ]                                                                         
  ]                                                                             
                                                                                
                                                                                
authorsDiv : Model -> List (Html msg)                                           
authorsDiv model =                                                              
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          svgStyle                                                      
          (fullScreenImg "../img/windows/authors.jpg")                          
      ]                                                                         
  ]                                                                             
                                                                                
                                                                                
finalDiv : Model -> List (Html msg)                                             
finalDiv model =                                                                
  [ div                                                                         
      [ (divStyle model "#ff1f15") ]                                            
      [ svg                                                                 
          svgStyle
          (fullScreenImg model.imgParam.finalImg)                                        
      ]                                                                         
  ]  

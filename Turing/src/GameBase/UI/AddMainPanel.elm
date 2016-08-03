module GameBase.UI.AddMainPanel exposing (addMainPanel)

import GameBase.UI.Ball exposing (ballsOfOneTapeDraw)
import GameBase.UI.Basket exposing (allBasketsDraw) 
import GameBase.UI.Cat exposing (gameCatDraw, catLooksDraw)
import GameBase.UI.ControlElements exposing (runButtonDraw, runFastDraw,               
                                             quesButtonDraw, helpMsgDraw, 
                                             levelDraw)
import GameBase.UI.TransTable.TransTableDraw exposing (transTableDraw, tableNotFullDraw) 
import GameBase.UI.DivSvgStyles exposing (fullScreenImg, svgStyle) 
import GameBase.Data.GameTypes exposing (Model)
import GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg) 

import Html exposing (Html)
import Svg exposing (Svg, svg, image)  
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 
import List exposing (head)


tableDraw : List (Svg msg)                                                      
tableDraw =                                                                     
  (fullScreenImg "../img/table.jpg")                                            
                                                                                
                                                                                
mirrorDraw : Int -> List (Svg msg)                                              
mirrorDraw level =                                                              
  [ Svg.image                                                                   
      [ x "30"                                                                  
      , y "55"                                                                  
      , Svg.Attributes.width "335px"                                            
      , Svg.Attributes.height "270px"                                           
      , xlinkHref ("../img/mirror/mirrorLevel" ++ (toString level) ++ ".png")   
      ]                                                                         
      []                                                                        
  ] 


addMainPanel : Model -> Html msg                                                
addMainPanel model =                                                            
  let                                                                           
    hpos = model.machine.initHeadPosForDraw                                     
    curTape = (getTapeFromCfg (head model.machineCfgs))                         
  in                                                                            
    svg                                                                     
      (svgStyle model)                                                        
      (                                                                       
        tableDraw                                                             
        ++                                                                    
        (mirrorDraw model.currLevel)                                          
        ++                                                                    
        (allBasketsDraw 7 [])                                                 
        ++                                                                    
        (ballsOfOneTapeDraw 7 [] curTape hpos)                                
        ++                                                                    
        (gameCatDraw model)                                                       
        ++                                                                    
        (transTableDraw model)                                      
        ++                                                                    
        runFastDraw                                                           
        ++                                                                    
        runButtonDraw                                                         
        ++                                                                    
        quesButtonDraw                                                        
        ++                                                                    
        (helpMsgDraw model.helpImg)                                           
        ++                                                                    
        (levelDraw model.currLevel model.maxLevel)
        ++                                                                    
        (catLooksDraw model)  
        ++
        (tableNotFullDraw model)
      )  

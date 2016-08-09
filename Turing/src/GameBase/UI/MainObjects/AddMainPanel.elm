module GameBase.UI.MainObjects.AddMainPanel exposing (addMainPanel)

import GameBase.UI.MainObjects.Ball exposing (ballsOfOneTapeDraw)
import GameBase.UI.MainObjects.Basket exposing (allBasketsDraw) 
import GameBase.UI.MainObjects.Cat exposing (gameCatDraw, lookingCatDraw)
import GameBase.UI.ControlObjects.ControlElements exposing 
    (runButtonDraw, runFastButtonDraw, quesButtonDraw, helpMsgDraw, levelDraw)
import GameBase.UI.TransTable.TransTableDraw exposing 
                                            (transTableDraw, tableNotFullDraw) 
import GameBase.UI.MainObjects.DivSvgStyles exposing (fullScreenImg, svgStyle) 
import GameBase.Data.GameTypes exposing (Model)
import GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg) 

import Html exposing (Html)
import Svg exposing (Svg, svg, image)  
import Svg.Attributes exposing (width, height, x, y, xlinkHref) 
import List exposing (head)


mirrorX : Int
mirrorX = 30

mirrorY : Int
mirrorY = 55

mirrorW : Int
mirrorW = 335

mirrorH : Int
mirrorH = 270


mirrorDraw : Int -> List (Svg msg)                                              
mirrorDraw level =                                                              
  [ image                                                                   
      [ x ((toString mirrorX) ++ "px")                                                                  
      , y ((toString mirrorY) ++ "px")                                                                  
      , width  ((toString mirrorW) ++ "px")                                           
      , height ((toString mirrorH) ++ "px")                      
      , xlinkHref ("../img/mirror/mirrorLevel" ++ (toString level) ++ ".png")   
      ]                                                                         
      []                                                                        
  ] 


tableDraw : List (Svg msg)                                                      
tableDraw =                                                                     
  (fullScreenImg "../img/table.jpg")


basketNumber : Int
basketNumber = 7


addMainPanel : Model -> Html msg                                                
addMainPanel model =                                                            
  let                                                                           
    hpos = model.modelMachine.machine.initHeadPosForDraw                                     
    curTape = (getTapeFromCfg (head model.modelMachine.machineCfgs))                         
  in                                                                            
    svg                                                                     
      svgStyle                                                        
      (                                                                       
        tableDraw                                                             
        ++                                                                    
        (mirrorDraw model.levels.currLevel)                                          
        ++                                                                    
        (allBasketsDraw basketNumber [])                                                 
        ++                                                                    
        (ballsOfOneTapeDraw basketNumber [] curTape hpos)                                
        ++                                                                    
        (gameCatDraw model)                                                       
        ++                                                                    
        (transTableDraw model)                                      
        ++                                                                    
        runFastButtonDraw                                                           
        ++                                                                    
        runButtonDraw                                                         
        ++                                                                    
        quesButtonDraw                                                        
        ++                                                                    
        (helpMsgDraw model.imgParam.helpImg)                                           
        ++                                                                    
        (levelDraw model.levels.currLevel model.levels.maxLevel)
        ++                                                                    
        (lookingCatDraw model)  
        ++
        (tableNotFullDraw model)
      )  

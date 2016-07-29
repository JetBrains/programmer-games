module GameBase.UI.AddMainPanel exposing (addMainPanel)

import GameBase.UI.Ball exposing (ballsOfOneTapeDraw)
import GameBase.UI.Basket exposing (allBasketsDraw) 
import GameBase.UI.Cat exposing (gameCatDraw, catLooksDraw)
import GameBase.UI.ControlElements exposing (runButtonDraw, runFastDraw,               
                                             quesButtonDraw, helpMsgDraw, 
                                             levelDraw)
import GameBase.UI.DivSvgStyles exposing (fullScreenImg, svgStyle) 
import GameBase.Data.GameTypes exposing (Model)
import GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg)               

import Html exposing (Html)
import Svg exposing (Svg, svg, image, text, text')  
import Svg.Attributes exposing (width, height, x, y, xlinkHref, 
                                fontStyle, fontSize)   
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


transTableDraw : Int -> List (Svg msg)                                          
transTableDraw level =                                                          
  [ Svg.image                                                                   
        [ x "380px"                                                             
        , y "60px"                                                              
        , Svg.Attributes.width "460px"                                          
        , Svg.Attributes.height "250px"                                         
        , xlinkHref ("../img/level" ++ (toString level) ++ "/transTable.png")   
        ]                                                                       
        []                                                                      
  ]


levelDraw : Int -> Int -> List (Svg msg)                                        
levelDraw level max =                                                           
  [ text'                                                                       
      [ x "695px"                                                               
      , y "360px"                                                               
      , fontStyle "italic"                                                      
      , fontSize "30px"                                                         
      ]                                                                         
      [ text ((toString level) ++ "/" ++ (toString max)) ]                      
  ]


tableNotFullDraw : Model -> List (Svg msg)                                      
tableNotFullDraw m =   
  if m.ifTableFull == False
     then [ text'                                                                       
              [ x "20px"                                                               
              , y "370px"                                                               
              , fontStyle "italic"                                                      
              , fontSize "15px"                                                         
              ]                                                                         
              [ text "Table is not full, fill the gaps to run the machine!" ]                      
          ]
  else []   

------------------------------------------------------------
                                                                                
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
        (transTableDraw model.currLevel)                                      
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

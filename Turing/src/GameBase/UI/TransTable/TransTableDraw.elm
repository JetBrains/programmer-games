module GameBase.UI.TransTable.TransTableDraw exposing (transTableDraw, 
                                                       tableNotFullDraw, 
                                                       trTableW,trTableH)

import GameBase.UI.TransTable.TransTableMargins exposing (trTableX, trTableY)
import GameBase.UI.TransTable.CellsDraw exposing (cellsProccessing,cellW,cellH)
import GameBase.Data.GameTypes exposing (Model)           

import Svg exposing (Svg, image, text, text')                              
import Svg.Attributes exposing (width,height,x,y,xlinkHref,fontStyle,fontSize, 
                                fontFamily, fontWeight)  


trTableW : Int 
trTableW = 800 


trTableH : Int
trTableH = 350


-- draw trans table with img from level dir
transTableDraw : Model -> List (Svg msg)                                          
transTableDraw m =  
  ( [image                                                                   
      [ x ((toString trTableX) ++ "px")
      , y ((toString trTableY) ++ "px")
      , width ((toString trTableW) ++ "px")   
      , height ((toString trTableH) ++ "px") 
      , xlinkHref ("../img/transTable/transTableLevel" ++ 
                   (toString m.levels.currLevel) ++ ".png")
      ]
      []  
    ]
    ++
    (cellsProccessing m.transTables.trTableUser)
  )
                 

notFullMsgX : Int
notFullMsgX = 60


notFullMsgY : Int
notFullMsgY = 400 


-- draw msg about table is not full
tableNotFullDraw : Model -> List (Svg msg)                                      
tableNotFullDraw m =                                                            
  if m.flags.ifTableFull == False                                                     
     then [ text'                                                               
              [ x ((toString notFullMsgX) ++ "px")                                                        
              , y ((toString notFullMsgY) ++ "px")      
              --, fontFamily "Courier"
              --, fontWeight "bold"
              , fontStyle "italic"                                              
              , fontSize "16px"              
              ]                                                                 
              [ text "Table is not full, fill the gaps to run the machine!" ]   
          ]                                                                     
  else [] 

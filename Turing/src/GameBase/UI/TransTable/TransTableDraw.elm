module GameBase.UI.TransTable.TransTableDraw exposing (transTableDraw, 
                                                       tableNotFullDraw, 
                                                       trTableW,trTableH)

import GameBase.UI.TransTable.TransTableMargins exposing (trTableX, trTableY)
import GameBase.UI.TransTable.CellsDraw exposing (cellsProccessing,cellW,cellH)
import GameBase.Data.GameTypes exposing (Model)           

import Svg exposing (Svg, image, text, text')                              
import Svg.Attributes exposing (width,height,x,y,xlinkHref,fontStyle,fontSize)  


trTableW : Int 
trTableW = 460


trTableH : Int
trTableH = 250


-- draw trans table with img from level dir
transTableDraw : Model -> List (Svg msg)                                          
transTableDraw m =  
  ( [image                                                                   
      [ x ((toString trTableX) ++ "px")
      , y ((toString trTableY) ++ "px")
      , width ((toString trTableW) ++ "px")   
      , height ((toString trTableH) ++ "px")   
      , xlinkHref ( "../img/level" ++ (toString m.levels.currLevel) ++ 
                    "/transTable.png" )   
      ]
      []  
    ]
    ++
    (cellsProccessing m.transTables.trTableUser)
  )
                 

notFullMsgX : Int
notFullMsgX = 20


notFullMsgY : Int
notFullMsgY = 350


-- draw msg about table is not full
tableNotFullDraw : Model -> List (Svg msg)                                      
tableNotFullDraw m =                                                            
  if m.flags.ifTableFull == False                                                     
     then [ text'                                                               
              [ x ((toString notFullMsgX) ++ "px")                                                        
              , y ((toString notFullMsgY) ++ "px")                                                       
              , fontStyle "italic"                                              
              , fontSize "15px"                                                 
              ]                                                                 
              [ text "Table is not full, fill the gaps to run the machine!" ]   
          ]                                                                     
  else [] 

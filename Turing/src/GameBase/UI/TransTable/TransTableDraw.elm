module GameBase.UI.TransTable.TransTableDraw exposing (transTableDraw, 
                                                       tableNotFullDraw, 
                                                       trTableWidth, 
                                                       trTableHeight)

import GameBase.UI.TransTable.TransTableMargins exposing (trTableLeftMargin, 
                                               trTableTopMargin)
import GameBase.UI.TransTable.CellsDraw exposing (cellsProccessing, cellWidth, 
                                                  cellHeight)
import GameBase.Data.GameTypes exposing (Model)           

import Svg exposing (Svg, image, text, text')                              
import Svg.Attributes exposing (width,height,x,y,xlinkHref,fontStyle,fontSize)  


trTableWidth : Int 
trTableWidth = 460


trTableHeight : Int
trTableHeight = 250


-- draw trans table with img from level dir
transTableDraw : Model -> List (Svg msg)                                          
transTableDraw m =  
  ( [image                                                                   
      [ x ((toString trTableLeftMargin) ++ "px")
      , y ((toString trTableTopMargin) ++ "px")
      , width ((toString trTableWidth) ++ "px")   
      , height ((toString trTableHeight) ++ "px")   
      , xlinkHref ( "../img/level" ++ (toString m.currLevel) ++ 
                    "/transTable.png" )   
      ]
      []  
    ]
    ++
    (cellsProccessing m.trTableUser)
  )
                                                                                

-- draw msg about table is not full
tableNotFullDraw : Model -> List (Svg msg)                                      
tableNotFullDraw m =                                                            
  if m.ifTableFull == False                                                     
     then [ text'                                                               
              [ x "20px"                                                        
              , y "350px"                                                       
              , fontStyle "italic"                                              
              , fontSize "15px"                                                 
              ]                                                                 
              [ text "Table is not full, fill the gaps to run the machine!" ]   
          ]                                                                     
  else [] 

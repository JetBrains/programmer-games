module GameBase.UI.TransTable exposing (transTableDraw, tableNotFullDraw) 

import GameBase.Data.GameTypes exposing (Model, BallOfWool(..), Kitten(..))           
import TuringMachine.TuringTypes exposing (Cell(..), Direction(..),
                                           UserKeyValue, UserTransTable)
import GameBase.Proccessing.TranslateTables exposing (checkIfTableFull) 

import Svg exposing (Svg, image, text, text')                              
import Svg.Attributes exposing (width,height,x,y,xlinkHref,fontStyle,fontSize)  
import Array exposing (length, isEmpty, slice, get)
import List exposing (drop, head, isEmpty)


-----------------------------------------------------------------------------

-- check if symbol is not empty
ifSymbNotEmpty : Cell (Maybe BallOfWool) -> Bool  
ifSymbNotEmpty symb =                                                           
  case symb of                                                                  
    EmptyCell -> False                                                          
    _ -> True                                                                   
                                                                                

-- check if state is not empty
ifStateNotEmpty : Cell Kitten -> Bool                                           
ifStateNotEmpty state =                                                         
  case state of                                                                 
    EmptyCell -> False                                                          
    _ -> True                                                                   
                                                                                

-- check if dir is not empty
ifDirNotEmpty : Cell Direction -> Bool                                          
ifDirNotEmpty dir =                                                             
  case dir of                                                                   
    EmptyCell -> False                                                          
    _ -> True                                                                   
                                                                                

-- check for whole value and update result if find empty value
ifValueNotEmpty : Maybe (UserKeyValue BallOfWool Kitten) -> 
                  List ((Kitten, Maybe BallOfWool), String) ->
                  List ((Kitten, Maybe BallOfWool), String)
ifValueNotEmpty maybeUserKV res =                                                   
  case maybeUserKV of                                                           
    Just userKV ->
      if (ifSymbNotEmpty userKV.value.symb) == False &&
         (ifStateNotEmpty userKV.value.state) == True &&
         (ifDirNotEmpty userKV.value.dir) == True
              then (res ++ [(userKV.key, "symb")])
      else if (ifStateNotEmpty userKV.value.state) == False &&
              (ifSymbNotEmpty userKV.value.symb) == True &&
              (ifDirNotEmpty userKV.value.dir) == True
              then (res ++ [(userKV.key, "state")])
      else if (ifDirNotEmpty userKV.value.dir) == False &&
              (ifSymbNotEmpty userKV.value.symb) == True &&
              (ifStateNotEmpty userKV.value.state) == True
              then (res ++ [(userKV.key, "dir")])
      else if (ifSymbNotEmpty userKV.value.symb) == False &&
              (ifStateNotEmpty userKV.value.state) == False &&
              (ifDirNotEmpty userKV.value.dir) == True
              then (res ++ [(userKV.key, "symb"), (userKV.key, "state")])
      else if (ifSymbNotEmpty userKV.value.symb) == False &&
              (ifDirNotEmpty userKV.value.dir) == False &&
              (ifStateNotEmpty userKV.value.state) == True
              then (res ++ [(userKV.key, "symb"), (userKV.key, "dir")])
      else if (ifStateNotEmpty userKV.value.state) == False && 
              (ifDirNotEmpty userKV.value.dir) == False &&
              (ifSymbNotEmpty userKV.value.symb) == True
              then (res ++ [(userKV.key, "state"), (userKV.key, "dir")])
      else if (ifSymbNotEmpty userKV.value.symb) == False &&                    
              (ifStateNotEmpty userKV.value.state) == False &&                  
              (ifDirNotEmpty userKV.value.dir) == False
              then (res ++ [(userKV.key, "symb"), (userKV.key, "state"), 
                            (userKV.key, "dir")])       
      else res 
    Nothing -> res

----------------------------------------------------------------------------

-- get key and element name (string) for empty cells
getListOfQuesCoord : UserTransTable BallOfWool Kitten -> 
                     List ((Kitten, Maybe BallOfWool), String) ->
                     List ((Kitten, Maybe BallOfWool), String)
getListOfQuesCoord initTable res =  
  let         
    updRes = (ifValueNotEmpty (get 0 initTable) res)
    len = (length initTable)                                                    
  in                                                                            
    if (Array.isEmpty initTable) == False 
      then (getListOfQuesCoord (slice 1 len initTable) updRes)                    
    else res


-- elem [0..2], column [0..2]
-- symb is 0 elem, dir is 1 elem, state is 2 elem
fromParamToLeftMargin : Int -> Int -> Int
fromParamToLeftMargin elem column =
  trTableLeftMargin + 155 + column*80 + elem*15


-- ifDir is 0 or 1 (bool), row [0..4]
-- "dir" is 1, _ is 0
fromParamToTopMargin : Int -> Int -> Int                                       
fromParamToTopMargin ifDir row =                                             
  trTableTopMargin + 35 + ifDir*20 + row*40 


-- get left margin from key parameters for question image
quesLeftMargin : ((Kitten, Maybe BallOfWool), String) -> Int
quesLeftMargin coord = 
  case coord of
    ((White, sym), "symb") -> (fromParamToLeftMargin 0 0) 
    ((White, sym), "state") -> (fromParamToLeftMargin 2 0)
    ((White, sym), _) -> (fromParamToLeftMargin 1 0)
    ((LightGrey, sym), "symb") -> (fromParamToLeftMargin 0 1) 
    ((LightGrey, sym), "state") -> (fromParamToLeftMargin 2 1) 
    ((LightGrey, sym), _) -> (fromParamToLeftMargin 1 1)
    ((Grey, sym), "symb") -> (fromParamToLeftMargin 0 2) 
    ((Grey, sym), "state") -> (fromParamToLeftMargin 2 2)
    ((Grey, sym), _) -> (fromParamToLeftMargin 1 2)
    ((st, sym), _) -> (fromParamToLeftMargin 0 3)


-- get top margin from key parameters for question image
quesTopMargin : ((Kitten, Maybe BallOfWool), String) -> Int 
quesTopMargin coord = 
  case coord of
    ((st, Just Red), "dir") -> (fromParamToTopMargin 1 0) 
    ((st, Just Red), s) -> (fromParamToTopMargin 0 0)
    ((st, Just Yellow), "dir") -> (fromParamToTopMargin 1 1)
    ((st, Just Yellow), s) -> (fromParamToTopMargin 0 1)
    ((st, Just Green), "dir") -> (fromParamToTopMargin 1 2)
    ((st, Just Green), s) -> (fromParamToTopMargin 0 2)
    ((st, Just Blue), "dir") -> (fromParamToTopMargin 1 3)
    ((st, Just Blue), s) -> (fromParamToTopMargin 0 3)
    ((st, Nothing), "dir") -> (fromParamToTopMargin 1 4)
    ((st, Nothing), s) -> (fromParamToTopMargin 0 4)


quesWidth : Int 
quesWidth = 12

quesHeight : Int
quesHeight = 26


-- svg msg for one (head) question
oneQuesDraw : Maybe ((Kitten, Maybe BallOfWool), String) -> List (Svg msg) 
oneQuesDraw maybeCoord =
  case maybeCoord of 
    Just c ->
      [ image                                                                     
          [ x (toString (quesLeftMargin c) ++ "px")
          , y (toString (quesTopMargin c) ++ "px")
          , width ((toString quesWidth) ++ "px")  
          , height ((toString quesHeight) ++ "px")
          , xlinkHref ("../img/elements/quesSmall.png")
          ]
          []
      ]
    Nothing ->
      [ image                                                                   
          [ x "5px"
          , y "5px"
          , width ((toString quesWidth) ++ "px")
          , height ((toString quesHeight) ++ "px")
          , xlinkHref ("../img/elements/quesSmall.png")
          ]                                                                     
          []                                                                    
      ]   


-- get all questions` images (svg msg) 
-- from ques coordinates (key + elem_name_string)
allQuesInTableDraw : List ((Kitten, Maybe BallOfWool), String) -> 
                     List (Svg msg) -> List (Svg msg)
allQuesInTableDraw coordList res =
  let 
    headCoord = (head coordList)
    updRes = res ++ (oneQuesDraw headCoord)
    updCList = drop 1 coordList
  in
    if (List.isEmpty coordList) == False
      then (allQuesInTableDraw updCList updRes) 
    else res


-- if table is not full - get list of ques coordinates (key + elem_name_string)
-- and then get svg msg for all questions
gapsInTableProccess : UserTransTable BallOfWool Kitten -> List (Svg msg)
gapsInTableProccess initTable =
  if (checkIfTableFull initTable True) == False
     then (allQuesInTableDraw (getListOfQuesCoord initTable []) [])
  else []

-----------------------------------------------------------------------------

trTableLeftMargin : Int
trTableLeftMargin = 380

trTableTopMargin : Int
trTableTopMargin = 60

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
      --, xlinkHref ( "../img/level" ++ 
      --              (toString m.currLevel) ++ 
      --              "/transTable.png"
      --            )   
      , xlinkHref ("../img/transTableCommon.png")
      ]
      []  
    ]
    ++
    (gapsInTableProccess m.trTableInit)
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

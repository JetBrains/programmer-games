module GameBase.UI.TransTable exposing (transTableDraw, tableNotFullDraw) 

import GameBase.Data.GameTypes exposing (Model, BallOfWool(..), Kitten(..))           
import TuringMachine.TuringTypes exposing (Cell(..), Direction(..), 
                                           UserKeyValue, UserValue, 
                                           UserTransTable)
import GameBase.Proccessing.TranslateTables exposing (checkIfTableFull) 

import Svg exposing (Svg, image, text, text')                              
import Svg.Attributes exposing (width,height,x,y,xlinkHref,fontStyle,fontSize)  
import Array exposing (length, isEmpty, slice, get)
import List exposing (drop, head, isEmpty)

-----------------------------------------------------------------------------
--PARAMETERS-----------------------------------------------------------------

trTableLeftMargin : Int 
trTableLeftMargin = 380 
                                                                                
trTableTopMargin : Int 
trTableTopMargin = 60 
                                                                                
trTableWidth : Int 
trTableWidth = 460
                                                                                
trTableHeight : Int
trTableHeight = 250

quesWidth : Int 
quesWidth = 12
                                                                                
quesHeight : Int 
quesHeight = 26

-----------------------------------------------------------------------------
--COMMON FUNC FOR EMPTY, STABLE AND USER CELLS PROCCESS----------------------

-- check if symbol is not empty
ifSymbEmpty : Cell (Maybe BallOfWool) -> Bool  
ifSymbEmpty symb =                                                           
  case symb of                                                                  
    EmptyCell -> True                                                          
    _ -> False                                                                   
                                                                                

-- check if state is not empty
ifStateEmpty : Cell Kitten -> Bool                                           
ifStateEmpty state =                                                         
  case state of                                                                 
    EmptyCell -> True                                                         
    _ -> False                                                                
                                                                                

-- check if dir is not empty
ifDirEmpty : Cell Direction -> Bool                                          
ifDirEmpty dir =                                                             
  case dir of                                                                   
    EmptyCell -> True                                                          
    _ -> False                                                           


-- elem [0..2], column [0..2]                                                   
-- symb is 0 elem, dir is 1 elem, state is 2 elem                               
fromParamToLeftMargin : Int -> Int -> Int                                       
fromParamToLeftMargin elem column =                                             
  trTableLeftMargin + 155 + column*80 + elem*20
                                                                                
                                                                                
-- ifDir is 0 or 1 (bool), row [0..4]                                           
-- "dir" is 1, _ is 0                                                           
fromParamToTopMargin : Int -> Int -> Int                                        
fromParamToTopMargin ifDir row =                                                
  trTableTopMargin + 40 + ifDir*20 + row*40                                     
                                                                                
----------------------------------------------------------------------------  
-- FUNC FOR EMPTY CELLS PROCCESS-------------------------------------------- 

-- get left margin from key parameters for question image                       
emptyCellLeftMargin : ( (Kitten, Maybe BallOfWool)
                      , String) -> Int                    
emptyCellLeftMargin key =                                                            
  case key of                                                                   
    ((White, sym), "symb") -> (fromParamToLeftMargin 0 0)
    ((White, sym), "state") -> (fromParamToLeftMargin 2 0)                      
    ((White, sym),  _) -> (fromParamToLeftMargin 1 0)                            
    ((LightGrey, sym), "symb") -> (fromParamToLeftMargin 0 1)                   
    ((LightGrey, sym), "state") -> (fromParamToLeftMargin 2 1)                  
    ((LightGrey, sym),  _) -> (fromParamToLeftMargin 1 1)                        
    ((Grey, sym), "symb") -> (fromParamToLeftMargin 0 2)                        
    ((Grey, sym), "state") -> (fromParamToLeftMargin 2 2)                       
    ((Grey, sym),  _) -> (fromParamToLeftMargin 1 2)                             
    ((st, sym),  _) -> (fromParamToLeftMargin 0 3)                               
                                                                                
                                                                                
-- get top margin from key parameters for question image                        
emptyCellTopMargin : ( (Kitten, Maybe BallOfWool)
                     , String) -> Int                     
emptyCellTopMargin key =                                                             
  case key of                                                                   
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


-- check for whole value and update result if find empty value                  
ifValueHasEmpty : Maybe (UserKeyValue BallOfWool Kitten) ->                     
                  List ((Kitten, Maybe BallOfWool), String) ->                  
                  List ((Kitten, Maybe BallOfWool), String)                     
ifValueHasEmpty maybeUserKV res =                                               
  case maybeUserKV of                                                           
    Just userKV ->                                                              
      if (ifSymbEmpty userKV.value.symb) == True &&                             
         (ifStateEmpty userKV.value.state) == False &&                          
         (ifDirEmpty userKV.value.dir) == False                                 
              then (res ++ [(userKV.key, "symb")])                              
      else if (ifStateEmpty userKV.value.state) == True &&                      
              (ifSymbEmpty userKV.value.symb) == False &&                       
              (ifDirEmpty userKV.value.dir) == False                            
              then (res ++ [(userKV.key, "state")])                             
      else if (ifDirEmpty userKV.value.dir) == True &&                          
              (ifSymbEmpty userKV.value.symb) == False &&                       
              (ifStateEmpty userKV.value.state) == False                        
              then (res ++ [(userKV.key, "dir")])                               
      else if (ifSymbEmpty userKV.value.symb) == True &&                        
              (ifStateEmpty userKV.value.state) == True &&                      
              (ifDirEmpty userKV.value.dir) == False                            
              then (res ++ [(userKV.key, "symb"), (userKV.key, "state")])       
      else if (ifSymbEmpty userKV.value.symb) == True &&                        
              (ifDirEmpty userKV.value.dir) == True &&                          
              (ifStateEmpty userKV.value.state) == False                        
              then (res ++ [(userKV.key, "symb"), (userKV.key, "dir")])         
      else if (ifStateEmpty userKV.value.state) == True &&                      
              (ifDirEmpty userKV.value.dir) == True &&                          
              (ifSymbEmpty userKV.value.symb) == False                          
              then (res ++ [(userKV.key, "state"), (userKV.key, "dir")])        
      else if (ifSymbEmpty userKV.value.symb) == True &&                        
              (ifStateEmpty userKV.value.state) == True &&                      
              (ifDirEmpty userKV.value.dir) == True                             
              then (res ++ [(userKV.key, "symb"), (userKV.key, "state"),        
                            (userKV.key, "dir")])                               
      else res                                                                  
    Nothing -> res 

-- get key and element name (string) for empty cells
getEmptyCellsKeys : UserTransTable BallOfWool Kitten -> 
                     List ((Kitten, Maybe BallOfWool), String) ->
                     List ((Kitten, Maybe BallOfWool), String)
getEmptyCellsKeys initTable res =  
  let         
    updRes = (ifValueHasEmpty (get 0 initTable) res)
    len = (length initTable)                                                    
  in                                                                            
    if (Array.isEmpty initTable) == False 
      then (getEmptyCellsKeys (slice 1 len initTable) updRes)                    
    else res


-- svg msg for one (head) question
oneEmptyCellDraw : Maybe ((Kitten, Maybe BallOfWool), String) -> List (Svg msg) 
oneEmptyCellDraw maybeKey =
  case maybeKey of 
    Just k ->
      [ image                                                                     
          [ x (toString (emptyCellLeftMargin k) ++ "px")
          , y (toString (emptyCellTopMargin k) ++ "px")
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
allEmptyCellsDraw : List ((Kitten, Maybe BallOfWool), String) -> 
                    List (Svg msg) -> List (Svg msg)
allEmptyCellsDraw keys res =
  let 
    headKey = (head keys)
    updRes = res ++ (oneEmptyCellDraw headKey)
    updKeyList = drop 1 keys
  in
    if (List.isEmpty keys) == False
      then (allEmptyCellsDraw updKeyList updRes) 
    else res


-- if table is not full - get list of ques coordinates (key + elem_name_string)
-- and then get svg msg for all questions
emptyCellsProccess : UserTransTable BallOfWool Kitten -> List (Svg msg)
emptyCellsProccess initTable =
  if (checkIfTableFull initTable True) == False
     then (allEmptyCellsDraw (getEmptyCellsKeys initTable []) [])
  else []

-----------------------------------------------------------------------------
-- FUNC FOR STABLE OR USER CELLS PROCCESS------------------------------------ 

-- get left margin from key parameters for question image                       
stableUserCellLeftMargin : 
                      ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) -> Int                                               
stableUserCellLeftMargin key =  
  case key of                                                                   
    ((White, sym), new, "symb") -> (fromParamToLeftMargin 0 0)                  
    ((White, sym), new, "state") -> (fromParamToLeftMargin 2 0)                     
    ((White, sym), new, _) -> (fromParamToLeftMargin 1 0)                           
    ((LightGrey, sym), new, "symb") -> (fromParamToLeftMargin 0 1)                  
    ((LightGrey, sym), new, "state") -> (fromParamToLeftMargin 2 1)                 
    ((LightGrey, sym), new, _) -> (fromParamToLeftMargin 1 1)                       
    ((Grey, sym), new, "symb") -> (fromParamToLeftMargin 0 2)                       
    ((Grey, sym), new, "state") -> (fromParamToLeftMargin 2 2)                      
    ((Grey, sym), new, _) -> (fromParamToLeftMargin 1 2)                            
    ((st, sym), new, _) -> (fromParamToLeftMargin 0 3)                              
                                                                                
                                                                                
-- get top margin from key parameters for question image                        
stableUserCellTopMargin : 
                      ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) -> Int                                                
stableUserCellTopMargin key = 
  case key of                                                                   
    ((st, Just Red), new,  "dir") -> (fromParamToTopMargin 1 0)                     
    ((st, Just Red), new, s) -> (fromParamToTopMargin 0 0)                          
    ((st, Just Yellow), new, "dir") -> (fromParamToTopMargin 1 1)                   
    ((st, Just Yellow), new, s) -> (fromParamToTopMargin 0 1)                       
    ((st, Just Green), new, "dir") -> (fromParamToTopMargin 1 2)                    
    ((st, Just Green), new, s) -> (fromParamToTopMargin 0 2)                        
    ((st, Just Blue), new, "dir") -> (fromParamToTopMargin 1 3)                     
    ((st, Just Blue), new, s) -> (fromParamToTopMargin 0 3)                         
    ((st, Nothing), new, "dir") -> (fromParamToTopMargin 1 4)                       
    ((st, Nothing), new, s) -> (fromParamToTopMargin 0 4)  


-- check for whole value and update result if find stable or user value 
ifValueHasStableUser : Maybe (UserKeyValue BallOfWool Kitten) ->         
                 List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) ->                  
                 List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String)                     
ifValueHasStableUser maybeUserKV res = 
  case maybeUserKV of                                                           
    Just userKV ->                                                              
      if (ifSymbEmpty userKV.value.symb) == False &&                         
         (ifStateEmpty userKV.value.state) == True &&                        
         (ifDirEmpty userKV.value.dir) == True                               
              then (res ++ [(userKV.key, userKV.value, "symb")])
      else if (ifStateEmpty userKV.value.state) == False &&                  
              (ifSymbEmpty userKV.value.symb) == True &&                     
              (ifDirEmpty userKV.value.dir) == True                          
              then (res ++ [(userKV.key, userKV.value, "state")])
      else if (ifDirEmpty userKV.value.dir) == False &&                      
              (ifSymbEmpty userKV.value.symb) == True &&                     
              (ifStateEmpty userKV.value.state) == True                      
              then (res ++ [(userKV.key, userKV.value, "dir")]) 
      else if (ifSymbEmpty userKV.value.symb) == False &&                    
              (ifStateEmpty userKV.value.state) == False &&                  
              (ifDirEmpty userKV.value.dir) == True                          
              then (res ++ [ (userKV.key, userKV.value, "symb")
                           , (userKV.key, userKV.value, "state")
                           ])
      else if (ifSymbEmpty userKV.value.symb) == False &&                    
              (ifDirEmpty userKV.value.dir) == False &&                      
              (ifStateEmpty userKV.value.state) == True                      
              then (res ++ [ (userKV.key, userKV.value, "symb")
                           , (userKV.key, userKV.value, "dir")
                           ])         
      else if (ifStateEmpty userKV.value.state) == False &&                  
              (ifDirEmpty userKV.value.dir) == False &&                      
              (ifSymbEmpty userKV.value.symb) == True                        
              then (res ++ [ (userKV.key, userKV.value, "state")
                           , (userKV.key, userKV.value, "dir")
                           ])        
      else if (ifSymbEmpty userKV.value.symb) == False &&                    
              (ifStateEmpty userKV.value.state) == False &&                  
              (ifDirEmpty userKV.value.dir) == False                         
              then (res ++ [ (userKV.key, userKV.value, "symb")
                           , (userKV.key, userKV.value, "state")
                           , (userKV.key, userKV.value, "dir")
                           ])                               
      else res                                                                  
    Nothing -> res                                                              
                   

-- get key and element name (string) for stable or user cells
getStableUserCellsKeys : UserTransTable BallOfWool Kitten ->                         
                 List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) ->                                        
                 List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String)   
getStableUserCellsKeys initTable res =                                               
  let                                                                           
    updRes = (ifValueHasStableUser (get 0 initTable) res)                            
    len = (length initTable)                                                    
  in                                                                            
    if (Array.isEmpty initTable) == False                                       
      then (getStableUserCellsKeys (slice 1 len initTable) updRes)                  
    else res   


--30 30 20 15
-- svg msg for one (head) stable or user cell
oneStableUserCellDraw : 
                Maybe ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) -> List (Svg msg) 
oneStableUserCellDraw maybeKey =                                                     
  case maybeKey of                                                              
    Just k ->
      let
        href = 
          case k of
            ((st, sym), {state, symb, dir}, "state") -> 
              case state of
                StableCell White -> 
                  "../img/saimonHead/SaimonHeadWVerySm.png"
                StableCell LightGrey -> 
                  "../img/saimonHead/SaimonHeadLGVerySm.png"
                StableCell Grey -> 
                  "../img/saimonHead/SaimonHeadGVerySm.png"
                StableCell Orange -> 
                  "../img/saimonHead/SaimonHeadOVerySm.png"
                StableCell Violet -> 
                  "../img/saimonHead/SaimonHeadSBVerySm.png"
                UserCell White ->                                            
                  "../img/saimonHead/SaimonHeadWVerySm.png"                    
                UserCell LightGrey ->                                        
                  "../img/saimonHead/SaimonHeadLGVerySm.png"                  
                UserCell Grey ->                                             
                  "../img/saimonHead/SaimonHeadGVerySm.png"                    
                UserCell Orange ->                                           
                  "../img/saimonHead/SaimonHeadOVerySm.png"                    
                UserCell Violet ->                                           
                  "../img/saimonHead/SaimonHeadSBVerySm.png"   
                EmptyCell ->
                  "../img/elements/quesSmall.png"
            ((st, sym), {state, symb, dir}, "symb") -> 
              case symb of
                StableCell (Just Blue) -> 
                  "../img/ballInBasket/blueBall.png"
                StableCell (Just Green) -> 
                  "../img/ballInBasket/greenBall.png"
                StableCell (Just Red) -> 
                  "../img/ballInBasket/redBall.png"
                StableCell (Just Yellow) -> 
                  "../img/ballInBasket/yellowBall.png"
                StableCell Nothing ->                                 
                  "../img/ballInBasket/transpBall.png"    
                UserCell (Just Blue) ->                                             
                  "../img/ballInBasket/blueBall.png"                             
                UserCell (Just Green) ->                                            
                  "../img/ballInBasket/greenBall.png"                            
                UserCell (Just Red) ->                                              
                  "../img/ballInBasket/redBall.png"                           
                UserCell (Just Yellow) ->                                        
                  "../img/ballInBasket/yellowBall.png" 
                UserCell Nothing ->                                   
                  "../img/ballInBasket/transpBall.png"  
                EmptyCell ->
                  "../img/elements/quesSmall.png"
            ((st, sym), {state, symb, dir}, _) -> 
              case dir of
                StableCell MoveRight -> 
                  "../img/elements/arrowR.png"
                StableCell MoveLeft -> 
                  "../img/elements/arrowL.png" 
                StableCell Stay ->
                  "../img/elements/arrowTransp.png"
                UserCell MoveRight ->                                     
                  "../img/elements/arrowR.png"                              
                UserCell MoveLeft ->                                      
                  "../img/elements/arrowL.png"  
                UserCell Stay ->
                  "../img/elements/arrowTransp.png"
                EmptyCell ->
                  "../img/elements/quesSmall.png"  
      in
        [ image                                                                   
            [ x (toString (stableUserCellLeftMargin k) ++ "px")
            , y (toString (stableUserCellTopMargin k) ++ "px")
            , width ((toString 20) ++ "px")
            , height ((toString 20) ++ "px")
            , xlinkHref href
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


-- get all kitten or ball_of_wool images (svg msg)
-- from coordinates (key + elem_name_string)
allStableUserCellsDraw : 
                 List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                      , String) -> List (Svg msg) -> List (Svg msg) 
allStableUserCellsDraw keys res =                                                    
  let                                                                           
    headKey = (head keys)                                                       
    updRes = res ++ (oneStableUserCellDraw headKey)                                  
    updKeyList = drop 1 keys                                                    
  in                                                                            
    if (List.isEmpty keys) == False                                             
      then (allStableUserCellsDraw updKeyList updRes)                                
    else res 


-- get svg msg for all stable or user cells in table                                    
stableUserCellsProccess : UserTransTable BallOfWool Kitten -> List (Svg msg)        
stableUserCellsProccess initTable =                                                 
  allStableUserCellsDraw (getStableUserCellsKeys initTable []) [] 

-----------------------------------------------------------------------------

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
    (stableUserCellsProccess m.trTableInit)
    ++
    (emptyCellsProccess m.trTableInit)
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

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


trTableLeftMargin : Int 
trTableLeftMargin = 380 

trTableTopMargin : Int 
trTableTopMargin = 60 

trTableWidth : Int 
trTableWidth = 460

trTableHeight : Int
trTableHeight = 250

cellWidth : Int
cellWidth = 20

cellHeight : Int
cellHeight = 20


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


-- get left margin from key parameters
cellLeftMargin : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                 , String) -> Int                                               
cellLeftMargin key =  
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
                                                                                
                                                                                
-- get top margin from key parameters
cellTopMargin : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                , String) -> Int                                                
cellTopMargin key = 
  case key of                                                                   
    ((st, Just Red), new, "dir") -> (fromParamToTopMargin 1 0)                     
    ((st, Just Red), new, s) -> (fromParamToTopMargin 0 0)                          
    ((st, Just Yellow), new, "dir") -> (fromParamToTopMargin 1 1)                   
    ((st, Just Yellow), new, s) -> (fromParamToTopMargin 0 1)                       
    ((st, Just Green), new, "dir") -> (fromParamToTopMargin 1 2)                    
    ((st, Just Green), new, s) -> (fromParamToTopMargin 0 2)                        
    ((st, Just Blue), new, "dir") -> (fromParamToTopMargin 1 3)                     
    ((st, Just Blue), new, s) -> (fromParamToTopMargin 0 3)                         
    ((st, Nothing), new, "dir") -> (fromParamToTopMargin 1 4)                       
    ((st, Nothing), new, s) -> (fromParamToTopMargin 0 4)  


getHeadKey : Maybe (UserKeyValue BallOfWool Kitten) ->
             List ((Kitten, Maybe BallOfWool)
                  , UserValue BallOfWool Kitten, String) ->
             List ((Kitten, Maybe BallOfWool)
                  , UserValue BallOfWool Kitten, String)
getHeadKey maybeUserKV res = 
  case maybeUserKV of
    Just userKV ->
      (res ++ [ (userKV.key, userKV.value, "symb")                                  
              , (userKV.key, userKV.value, "state")                                         
              , (userKV.key, userKV.value, "dir")                                           
              ]
      )   
    Nothing -> res


-- get key, value and element name (string)
getKeys : UserTransTable BallOfWool Kitten ->                         
          List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
               , String) ->                                        
          List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
               , String)   
getKeys initTable res =                                               
  let                                                                           
    updRes = (getHeadKey (get 0 initTable) res)                            
    len = (length initTable)                                                    
  in                                                                            
    if (Array.isEmpty initTable) == False                                       
      then (getKeys (slice 1 len initTable) updRes)                  
    else res   


-- svg msg for one (head) cell
oneCellDraw : Maybe ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                    , String) -> List (Svg msg) 
oneCellDraw maybeKey =                                                     
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
            [ x (toString (cellLeftMargin k) ++ "px")
            , y (toString (cellTopMargin k) ++ "px")
            , width ((toString cellWidth) ++ "px")
            , height ((toString cellHeight) ++ "px")
            , xlinkHref href
            ]                                                                     
            []                                                                    
        ]                                                                         
    Nothing ->                                                                  
      [ image 
          [ x "5px" 
          , y "5px"
          , width ((toString cellWidth) ++ "px")  
          , height ((toString cellHeight) ++ "px") 
          , xlinkHref ("../img/elements/quesSmall.png")
          ] 
          [] 
      ]


-- svg msg for all cells
allCellsDraw : List ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten
                    , String) -> List (Svg msg) -> List (Svg msg) 
allCellsDraw keys res =                                                    
  let                                                                           
    headKey = (head keys)                                                       
    updRes = res ++ (oneCellDraw headKey)                                  
    updKeyList = drop 1 keys                                                    
  in                                                                            
    if (List.isEmpty keys) == False                                             
      then (allCellsDraw updKeyList updRes)                                
    else res 


cellsProccessing : UserTransTable BallOfWool Kitten -> List (Svg msg)        
cellsProccessing initTable =                                                 
  allCellsDraw (getKeys initTable []) [] 

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
    (cellsProccessing m.trTableInit)
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

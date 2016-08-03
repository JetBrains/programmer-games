module GameBase.UI.TransTable.CellsDraw exposing (cellsProccessing, cellWidth, 
                                                  cellHeight)

import GameBase.UI.TransTable.TransTableMargins exposing (cellLeftMargin, cellTopMargin)   
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))     
import TuringMachine.TuringTypes exposing (Cell(..), Direction(..),             
                                           UserKeyValue, UserValue,             
                                           UserTransTable)                      

import Svg exposing (image, Svg)
import Svg.Attributes exposing (width, height, x, y, xlinkHref)  
import Array exposing (length, slice, get)                             
import List exposing (drop, head)  


cellWidth : Int                                                                 
cellWidth = 20                                                                  


cellHeight : Int                                                                
cellHeight = 20 


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
getKeys table res =                                                         
  let                                                                           
    updRes = (getHeadKey (get 0 table) res)                                 
    len = (length table)                                                    
  in                                                                            
    if (Array.isEmpty table) == False                                       
      then (getKeys (slice 1 len table) updRes)
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
cellsProccessing table =                                                    
  allCellsDraw (getKeys table []) []  

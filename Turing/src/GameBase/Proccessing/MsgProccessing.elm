module GameBase.Proccessing.MsgProccessing exposing (clickMsgProccessing, 
                                                    moveMsgProccessing, 
                                                    tickMsgProccessing)

import GameBase.Data.LevelsData exposing (machine1, transTable1, input1, 
                                          expectedResult1, expectedPos1, 
                                          usedCats1, usedBalls1, 
                                          machine2, transTable2, input2, 
                                          expectedResult2, expectedPos2,
                                          usedCats2, usedBalls2)
import GameBase.Proccessing.WorkWithCfg exposing (getAllCfgs, getNextCfg)
import GameBase.Proccessing.CheckResult exposing (checkResult)
import GameBase.Data.GameTypes exposing (Msg(..), Model, Position, 
                                         BallOfWool(..), Kitten(..))
import GameBase.Data.Init exposing (initModel)
import GameBase.UI.Cat exposing (updCatParam)
import GameBase.UI.TransTable.EmptyCellsCoord exposing (getEmptyCellsCoord)
import GameBase.UI.TransTable.TransTableDraw exposing (trTableWidth,     
                                                       trTableHeight)  
import GameBase.UI.TransTable.TransTableMargins exposing (trTableLeftMargin,    
                                                          trTableTopMargin)
import TuringMachine.TuringTypes exposing (Cell(..), Direction(..), 
                                           UserTransTable)

import List exposing (head, drop, take)
import Array exposing (get, set)
import Task exposing (perform) 
import Time exposing (Time, inSeconds, now, second, millisecond)
import Window exposing (size)


getInitByLevel : Int -> Model                                                   
getInitByLevel level =                                                          
  case level of                                                                 
    1 -> (initModel machine1 transTable1 input1 expectedResult1 1 expectedPos1 
                    usedCats1 usedBalls1) 
    _ -> (initModel machine2 transTable2 input2 expectedResult2 2 expectedPos2 
                    usedCats2 usedBalls2) 
                                                                                
                                                                                
contPlayModel : Model -> Model                                                  
contPlayModel model =                                                           
  { model                                                                       
      | ifStart = False                                                         
      , ifPlay = True                                                           
      , ifEnd = False                                                           
  }  


setPushFlag model =                                                             
  { model | ifPushRun = True }

                                                                                
setTime : Time -> Model -> Model                                                
setTime time model =                                                            
  { model                                                                       
      | timeUnit = time                                                         
  }    


clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )                   
clickMsgProccessing m pos =                                                     
  if m.ifStart == True                                                          
     then if pos.y >= 190 && pos.y <= 217 && pos.x >= 360 && pos.x <= 447       
                  then ( { m                                                    
                            | ifStart = False                                   
                            , ifPlay = True                                     
                          }                                                     
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )                                                        
          else if pos.y >= 247 && pos.y <= 274 && pos.x >= 360 && pos.x <= 465  
                  then ( { m                                                    
                            | ifStart = False                                   
                            , ifRules = True                                    
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else if pos.y >= 300 && pos.y <= 327 && pos.x >= 360 && pos.x <= 713  
                  then ( { m                                                    
                            | ifStart = False                                   
                            , ifAuthors = True                                  
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none)                                                    
  else if m.ifAuthors == True || m.ifRules == True                              
     then if pos.y >= 520 && pos.y <= 570 && pos.x >= 515 && pos.x <= 765       
                  then ( { m                                                    
                            | ifStart = True                                    
                            , ifAuthors = False                                 
                            , ifRules = False                                   
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none)                                                    
  -- if win all levels                                                          
  else if m.ifEnd == True && m.finalImg == "../img/finalImg/final.png"          
     then if pos.y >= 350 && pos.y <= 380 && pos.x >= 155 && pos.x <= 680       
                  then ( (getInitByLevel 1)                                     
                       , perform (\_ -> Debug.crash "task") WindowSize size
                       )                                                        
          else (m, Cmd.none)                                                    
  -- if go to the next level                                                    
  else if m.ifEnd == True && m.finalImg == "../img/finalImg/pos.jpg"            
     then if pos.y >= 170 && pos.y <= 200 && pos.x >= 190 && pos.x <= 670       
                  then ( (getInitByLevel m.currLevel)                           
                         |> contPlayModel                                       
                       , perform (\_ -> Debug.crash "task") WindowSize size 
                       )                                                        
          else (m, Cmd.none)     
  -- if go to the current level again                                           
  else if m.ifEnd == True && m.finalImg == "../img/finalImg/neg.png"            
     then if pos.y >= 520 && pos.y <= 560 && pos.x >= 315 && pos.x <= 668       
                  then ( (getInitByLevel m.currLevel)                           
                         |> contPlayModel                                       
                       , perform (\_ -> Debug.crash "task") WindowSize size 
                       )                                                        
          else (m, Cmd.none)                                                    
  else if m.ifPlay == True                                                      
     then if m.ifCatLooks == True &&                                            
             pos.y >= 0 && pos.y <= 600 && pos.x >= 0 && pos.x <= 800           
                  then ( {m                                                     
                            | ifCatLooks = False                                
                            , whenGameStarts = m.currTime                       
                         }                                                      
                       , Cmd.none                                               
                       )                                                        
          -- if return from gameWindow to main menu                             
          else if pos.y >= 20 && pos.y <= 35 && pos.x >= 540 && pos.x <= 740    
                  then ( (getInitByLevel 1)                                     
                       , perform (\_ -> Debug.crash "task") WindowSize size 
                       )      
          else if pos.y >= trTableTopMargin && 
                  pos.y <= (trTableTopMargin + trTableHeight) &&
                  pos.x >= trTableLeftMargin &&
                  pos.x <= (trTableLeftMargin + trTableWidth)
                  then (clickTrTableProccessing m pos)
          else if pos.y >= 325 && pos.y <= 380 && pos.x >= 535 && pos.x <= 580  
                  then (clickRunProccessing m second)                           
          else if pos.y >= 335 && pos.y <= 375 && pos.x >= 465 && pos.x <= 505  
                  then (clickRunProccessing m millisecond)                      
          else if pos.y >= 322 && pos.y <= 377 && pos.x >= 624 && pos.x <= 654  
                  then (clickHelpProccessing m)                                 
          else (m, Cmd.none)                                                    
  else (m, Cmd.none) 


-- get State from usedCats array by click number
-- provide running through cats arr by click  
getStateByClick : Int -> Model -> Cell Kitten
getStateByClick clickNum m =
  let
    len = (Array.length m.usedCats)
  in
    if clickNum >= len
      then getStateByClick (clickNum - len) m
    else 
      case (get clickNum m.usedCats) of
        Just state -> state
        Nothing -> EmptyCell


-- get Symb from usedBalls array by click number                                  
-- provide running through balls arr by click  
getSymbByClick : Int -> Model -> Cell (Maybe BallOfWool)                                 
getSymbByClick clickNum m =
  let                                                                           
    len = (Array.length m.usedBalls)                                                   
  in                                                                            
    if clickNum >= len                                                          
      then getSymbByClick (clickNum - len) m                                   
    else 
      case (get clickNum m.usedBalls) of
        Just symb -> symb                                                     
        Nothing -> EmptyCell


-- get Dir from usedDirs array by click number
-- provide running through dirs arr by click
getDirByClick : Int -> Model -> Cell Direction
getDirByClick clickNum m =
  let                                                                           
    len = (Array.length m.usedDirs)                                                   
  in                                                                            
    if clickNum >= len                                                          
      then getDirByClick (clickNum - len) m 
    else 
      case (get clickNum m.usedDirs) of
        Just dir -> dir                                                         
        Nothing -> EmptyCell 


-- change user transition table when click if click pos coordinates 
-- are the same as some coord of empty cells
clickTrTableProccessing : Model -> Position -> ( Model, Cmd Msg )               
clickTrTableProccessing m pos = 
  let
    emptyCellsCoord = (getEmptyCellsCoord m.trTableInit [] 0) 
    (kvIndForChange, elemForChange) = 
          (getIndIfClickOnEmpty pos emptyCellsCoord)
    kvForChange = 
      case (get kvIndForChange m.trTableUser) of
        Just kv -> kv
        Nothing -> { key = (Violet, Nothing)
                   , value = { state = EmptyCell 
                             , symb = EmptyCell
                             , dir = EmptyCell}
                   , clickNum = 0          
                   }
  in 
    if kvIndForChange > -1
       then let
              (newState, newSymb, newDir) = 
                case elemForChange of
                  "state" -> 
                    ( (getStateByClick kvForChange.clickNum m)
                    , kvForChange.value.symb
                    , kvForChange.value.dir) 
                  "symb" -> 
                    ( kvForChange.value.state
                    , (getSymbByClick kvForChange.clickNum m)
                    , kvForChange.value.dir)
                  _ -> 
                    ( kvForChange.value.state
                    , kvForChange.value.symb
                    , (getDirByClick kvForChange.clickNum m))
            in
              ( { m | trTableUser = (set kvIndForChange 
                                         { key = kvForChange.key 
                                         , value =
                                            { state = newState
                                            , symb  = newSymb
                                            , dir   = newDir
                                            }
                                         , clickNum = kvForChange.clickNum + 1
                                         }
                                         m.trTableUser
                                     )
                }
              , Cmd.none
              )
    else (m, Cmd.none)


-- run throught the list of empty cells coordinates and if pos coordinates 
-- are in one of these intervals - return array index of KV for changing
getIndIfClickOnEmpty :  Position -> List (Int, Int, Int, Int, Int, String) -> 
                        (Int, String)
getIndIfClickOnEmpty pos list =
  let 
    (topFrom, topTo, leftFrom, leftTo, arrInd, elemName) = 
      case (head list) of
        Just coord -> coord 
        Nothing -> (-1, -1, -1, -1, -1, "")
        
  in
     if (List.isEmpty list) == False
        then if pos.y >= topFrom && pos.y <= topTo && 
                pos.x >= leftFrom && pos.x <= leftTo
                then (arrInd, elemName)
             else getIndIfClickOnEmpty pos (drop 1 list)   
     else (-1, "")  


-- draw help img if click on help
clickHelpProccessing : Model -> ( Model, Cmd Msg )                              
clickHelpProccessing model =                                                    
  if model.helpImg == " "                                                       
    then ({model | helpImg = "../img/help.png"}, Cmd.none)                      
  else ({model | helpImg = " "}, Cmd.none)                                      
                                                                                

-- check if table full and run the machine
clickRunProccessing : Model -> Time -> ( Model, Cmd Msg )                       
clickRunProccessing model time =                                                
  let                                                                           
    updModel = (getAllCfgs model)
  in                                                                            
    if updModel.ifTableFull == False                                            
       then (updModel, Cmd.none)
    else 
      ( setPushFlag updModel
        |> updCatParam time
        |> setTime time
      , Cmd.none                                                                
      )  


-- proccessing cat`s movements (cursor movements) in menu window
moveMsgProccessing : Model -> Position -> ( Model, Cmd Msg )                    
moveMsgProccessing m pos =                                                      
  if m.ifStart == True                                                          
     then if pos.y >= 190 && pos.y <= 217 && pos.x >= 360 && pos.x <= 447       
                  then ( { m                                                    
                            | menuCatTop = 180                                  
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else if pos.y >= 247 && pos.y <= 274 && pos.x >= 360 && pos.x <= 465  
                  then ( { m                                                    
                            | menuCatTop = 240                                  
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else if pos.y >= 300 && pos.y <= 327 && pos.x >= 360 && pos.x <= 713  
                  then ( { m                                                    
                            | menuCatTop = 300                                  
                          }                                                     
                       , Cmd.none                                               
                       )                                                        
          else (m, Cmd.none)                                                    
  else (m, Cmd.none)   


-- proccessing time parameters for checking result, 
-- getting next cfg for drawing, drawing CatLooks
tickMsgProccessing : Model -> Time -> ( Model, Cmd Msg )                        
tickMsgProccessing m time =                                                     
  if m.whenGameStarts == 0 && m.ifPlay == True                                  
     then ( { m                                                                 
                | whenGameStarts = time                                         
                , currTime = time                                               
            }                                                                   
          , Cmd.none                                                            
          )                                                                     
  else if m.whenGameStarts > 0 && m.ifPlay == True &&                           
          m.ifPushRun == False                                                  
          then if ((inSeconds m.currTime) - (inSeconds m.whenGameStarts)) > 40  
                  then ({m | ifCatLooks = True, currTime = time}, Cmd.none)     
               else ({m | currTime = time}, Cmd.none)                           
  else if m.ifPlay == True && m.ifPushRun == True                               
     then if (List.length m.machineCfgs) > 1                                         
             then ( (getNextCfg m)                                              
                    |> updCatParam time                                         
                  , Cmd.none                                                    
                  )                                                             
          else (checkResult m time)                                             
  else                                                                          
    ({m | currTime = time}, Cmd.none)

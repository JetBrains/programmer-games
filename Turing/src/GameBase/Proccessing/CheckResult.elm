module GameBase.Proccessing.CheckResult exposing (checkResult)

import GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg, getHeadCfg)
import GameBase.Data.GameTypes exposing (BallOfWool, Kitten, Model)

import List exposing (head)                                                     
import Time exposing (Time) 


getResTape : Model -> List (Maybe BallOfWool)                                   
getResTape model =                                                              
  (head model.modelMachine.machineCfgs)                                                      
  |> getTapeFromCfg                                                             
                                                                                
                                                                                
getResState : Model -> Kitten                                                   
getResState m =                                                                 
  let                                                                           
    {currState, currDir, tapeCfg} = (getHeadCfg m)                              
  in                                                                            
    currState                                                                   
                                                                                
                                                                                
ifCorrect : Model -> Bool                                                       
ifCorrect m =                                                                   
  if (getResTape m) == m.expResults.expRes &&                                              
     (getResState m) == m.modelMachine.machine.acceptState &&                                
     m.imgParam.catPos == m.expResults.expPos                                                       
     then True                                                                  
  else False                                                                    
                                                                                
                                                                                
resultModel : Model -> String -> Int -> Time -> (Model, Cmd msg)                
resultModel model href level time =                                             
  ( { model                                                                     
        | imgParam =
            { catLeft = model.imgParam.catLeft
            , menuCatTop = model.imgParam.menuCatTop
            , catPos = model.imgParam.catPos
            , catImg = model.imgParam.catImg
            , helpImg = model.imgParam.helpImg
            , finalImg = href 
            }   
        , levels =
            { currLevel = level 
            , maxLevel = model.levels.maxLevel
            }
        , flags =
            { ifPushRun = False
            , ifStart = model.flags.ifStart
            , ifPlay = False
            , ifRules = model.flags.ifRules 
            , ifAuthors = model.flags.ifAuthors
            , ifEnd = True  
            , ifCatLooks = model.flags.ifCatLooks 
            , ifTableFull = model.flags.ifTableFull
            }
        , options =
            { winSize = model.options.winSize 
            , timeUnit = model.options.timeUnit
            , whenGameStarts = model.options.whenGameStarts
            , currTime = time
            , tapeCellsNumb = model.options.tapeCellsNumb
            }
    }                                                                           
  , Cmd.none                                                                    
  )                                                                             
                                                                                

checkResult : Model -> Time -> (Model, Cmd msg)                                 
checkResult m time =                                                            
  let                                                                           
    newLev = m.levels.currLevel + 1                                                    
  in                                                                            
    if (ifCorrect m) && newLev > m.levels.maxLevel                                     
       then (resultModel m "../img/finalImg/final.png" newLev time)             
    else if (ifCorrect m) && (newLev < m.levels.maxLevel || 
                              newLev == m.levels.maxLevel)      
       then (resultModel m "../img/finalImg/pos.jpg" newLev time)               
    else (resultModel m "../img/finalImg/neg.png" m.levels.currLevel time)  

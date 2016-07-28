module GameBase.Proccessing.CheckResult exposing (checkResult)

import GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg, getHeadCfg)
import GameBase.Data.GameTypes exposing (BallOfWool, Kitten, Model)

import List exposing (head)                                                     
import Time exposing (Time) 


getResTape : Model -> List (Maybe BallOfWool)                                   
getResTape model =                                                              
  (head model.machineCfgs)                                                      
  |> getTapeFromCfg                                                             
                                                                                
                                                                                
getResState : Model -> Kitten                                                   
getResState m =                                                                 
  let                                                                           
    {currState, currDir, tapeCfg} = (getHeadCfg m)                              
  in                                                                            
    currState                                                                   
                                                                                
                                                                                
ifCorrect : Model -> Bool                                                       
ifCorrect m =                                                                   
  if (getResTape m) == m.expRes &&                                              
     (getResState m) == m.machine.acceptState &&                                
     m.catPos == m.expPos                                                       
     then True                                                                  
  else False                                                                    
                                                                                
                                                                                
resultModel : Model -> String -> Int -> Time -> (Model, Cmd msg)                
resultModel model href level time =                                             
  ( { model                                                                     
        | finalImg = href                                                       
        , currLevel = level                                                     
        , ifEnd = True                                                          
        , ifPlay = False                                                        
        , ifPushRun = False                                                     
        , currTime = time                                                       
    }                                                                           
  , Cmd.none                                                                    
  )                                                                             
                                                                                
                                                                                
checkResult : Model -> Time -> (Model, Cmd msg)                                 
checkResult m time =                                                            
  let                                                                           
    newLev = m.currLevel + 1                                                    
  in                                                                            
    if (ifCorrect m) && newLev > m.maxLevel                                     
       then (resultModel m "../img/finalImg/final.png" newLev time)             
    else if (ifCorrect m) && (newLev < m.maxLevel || newLev == m.maxLevel)      
       then (resultModel m "../img/finalImg/pos.jpg" newLev time)               
    else (resultModel m "../img/finalImg/neg.png" m.currLevel time)  

module GameBase.Data.Init exposing (init, initModel)

import Time exposing (second)
import Task exposing (perform)
import Window exposing (Size, size)

import TuringMachine.InitUpdate exposing (initMachineCfg) 
import TuringMachine.TuringTypes exposing (Machine, UserTransTable)
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..), 
                                         Model, Msg(..)) 


init : Machine BallOfWool Kitten -> UserTransTable BallOfWool Kitten ->             
       List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> Int ->             
       Int -> (Model, Cmd Msg)                                                  
init machine table inp expRes level expPos =                                    
  ( (initModel machine table inp expRes level expPos)                           
  , perform (\_ -> Debug.crash "task") WindowSize size                          
  )


initModel : Machine BallOfWool Kitten -> UserTransTable BallOfWool Kitten ->        
            List (Maybe BallOfWool) -> List (Maybe BallOfWool) -> Int ->        
            Int -> Model                                                        
initModel machine table inp expRes level expPos =                               
  { windSize = (Size 1855 980)                                           
  , timeUnit = second                                                           
  , whenGameStarts = 0                                                          
  , currTime = 0                                                                
  , input = inp                                                                 
  , machine = machine                                                           
  , machineCfgs = [(initMachineCfg machine inp machine.initHeadPosForMach)]     
  , trTableInit = table                                                         
  , trTableUser = table                                                         
  , catLeft = 45                                                                
  , menuCatTop = 180                                                            
  , catPos = machine.initHeadPosForDraw                                         
  , catImg = "../img/saimonThink/SaimonThinkW.png"                              
  , helpImg = " "                                                               
  , finalImg = " "                                                              
  , currLevel = level                                                           
  , maxLevel = 2                                                                
  , expPos = expPos                                                             
  , expRes = expRes                                                             
  , ifPushRun = False                                                           
  , ifStart = True                                                              
  , ifPlay = False                                                              
  , ifRules = False                                                             
  , ifAuthors = False                                                           
  , ifEnd = False                                                               
  , ifCatLooks = False                                                          
  }  

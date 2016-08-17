module GameBase.Data.Init exposing (init, getInitByLevel)

import TuringMachine.InitUpdate exposing (initMachineCfg) 
import TuringMachine.TuringTypes exposing (Machine, UserTransTable, 
                                           Direction(..), Cell(..))
import GameBase.Data.GameTypes exposing                                         
                        (BallOfWool(..), Kitten(..), Position, Msg(..), Model,  
                         ModelOptions, ModelMachine, ModelTransTables,          
                         ModelImgParam, ModelLevels, ModelExpResults,           
                         ModelFlags, ModelObjectsSet)  
import GameBase.Data.LevelsData.DemoLevelData exposing                                     
        (levelsNumber, basketsNumbDemo, machineDemo, inputDemo, transTableDemo, 
         expectedResultDemo, expectedPosDemo, usedCatsDemo, usedBallsDemo)  
import GameBase.Data.LevelsData.TasksBlock1Data exposing                                   
            (basketsNumb1_1, machine1_1, input1_1, transTable1_1,                               
             expectedResult1_1, expectedPos1_1, usedCats1_1, usedBalls1_1,      
             basketsNumb1_2, machine1_2, input1_2, transTable1_2,                               
             expectedResult1_2, expectedPos1_2, usedCats1_2, usedBalls1_2,      
             basketsNumb1_3, machine1_3, input1_3, transTable1_3,                               
             expectedResult1_3, expectedPos1_3, usedCats1_3, usedBalls1_3)  
import GameBase.Data.LevelsData.TasksBlock2Data exposing                        
            (basketsNumb2_1, machine2_1, input2_1, transTable2_1,                               
             expectedResult2_1, expectedPos2_1, usedCats2_1, usedBalls2_1,      
             basketsNumb2_2, machine2_2, input2_2, transTable2_2,                               
             expectedResult2_2, expectedPos2_2, usedCats2_2, usedBalls2_2,
             basketsNumb2_3, machine2_3, input2_3, transTable2_3,                               
             expectedResult2_3, expectedPos2_3, usedCats2_3, usedBalls2_3) 
import GameBase.Data.LevelsData.TasksBlock3Data_Light exposing                  
            (basketsNumb3_1, machine3_1, input3_1, transTable3_1,               
             expectedResult3_1, expectedPos3_1, usedCats3_1, usedBalls3_1,      
             basketsNumb3_2, machine3_2, input3_2, transTable3_2,               
             expectedResult3_2, expectedPos3_2, usedCats3_2, usedBalls3_2,      
             basketsNumb3_3, machine3_3, input3_3, transTable3_3,               
             expectedResult3_3, expectedPos3_3, usedCats3_3, usedBalls3_3,      
             basketsNumb3_4, machine3_4, input3_4, transTable3_4,               
             expectedResult3_4, expectedPos3_4, usedCats3_4, usedBalls3_4) 
import GameBase.Data.LevelsData.TasksBlock3Data_Hard exposing                   
            (basketsNumb3_5, machine3_5, input3_5, transTable3_5,               
             expectedResult3_5, expectedPos3_5, usedCats3_5, usedBalls3_5,      
             basketsNumb3_6, machine3_6, input3_6, transTable3_6,               
             expectedResult3_6, expectedPos3_6, usedCats3_6, usedBalls3_6,      
             basketsNumb3_7, machine3_7, input3_7, transTable3_7,               
             expectedResult3_7, expectedPos3_7, usedCats3_7, usedBalls3_7,      
             basketsNumb3_8, machine3_8, input3_8, transTable3_8,               
             expectedResult3_8, expectedPos3_8, usedCats3_8, usedBalls3_8,      
             basketsNumb3_9, machine3_9, input3_9, transTable3_9,               
             expectedResult3_9, expectedPos3_9, usedCats3_9, usedBalls3_9)                          
import GameBase.Data.LevelsData.TasksBlock4Data exposing                        
            (basketsNumb4_1, machine4_1, input4_1, transTable4_1,               
             expectedResult4_1, expectedPos4_1, usedCats4_1, usedBalls4_1,      
             basketsNumb4_2, machine4_2, input4_2, transTable4_2,               
             expectedResult4_2, expectedPos4_2, usedCats4_2, usedBalls4_2,      
             basketsNumb4_3, machine4_3, input4_3, transTable4_3,               
             expectedResult4_3, expectedPos4_3, usedCats4_3, usedBalls4_3,
             basketsNumb4_4, machine4_4, input4_4, transTable4_4,               
             expectedResult4_4, expectedPos4_4, usedCats4_4, usedBalls4_4,
             basketsNumb4_5, machine4_5, input4_5, transTable4_5,               
             expectedResult4_5, expectedPos4_5, usedCats4_5, usedBalls4_5,
             basketsNumb4_6, machine4_6, input4_6, transTable4_6,               
             expectedResult4_6, expectedPos4_6, usedCats4_6, usedBalls4_6,
             basketsNumb4_7, machine4_7, input4_7, transTable4_7,               
             expectedResult4_7, expectedPos4_7, usedCats4_7, usedBalls4_7)
import GameBase.Data.LevelsData.TasksBlock5Data exposing                        
            (basketsNumb5_1, machine5_1, input5_1, transTable5_1,               
             expectedResult5_1, expectedPos5_1, usedCats5_1, usedBalls5_1,      
             basketsNumb5_2, machine5_2, input5_2, transTable5_2,               
             expectedResult5_2, expectedPos5_2, usedCats5_2, usedBalls5_2,
             basketsNumb5_3, machine5_3, input5_3, transTable5_3,               
             expectedResult5_3, expectedPos5_3, usedCats5_3, usedBalls5_3,
             basketsNumb5_4, machine5_4, input5_4, transTable5_4,               
             expectedResult5_4, expectedPos5_4, usedCats5_4, usedBalls5_4,
             basketsNumb5_5, machine5_5, input5_5, transTable5_5,               
             expectedResult5_5, expectedPos5_5, usedCats5_5, usedBalls5_5,
             basketsNumb5_6, machine5_6, input5_6, transTable5_6,               
             expectedResult5_6, expectedPos5_6, usedCats5_6, usedBalls5_6,
             basketsNumb5_7, machine5_7, input5_7, transTable5_7,               
             expectedResult5_7, expectedPos5_7, usedCats5_7, usedBalls5_7) 
import GameBase.UI.MainObjects.Cat exposing (catThinkX)
import GameBase.UI.ControlObjects.ControlLabelsParam exposing (menuItemTopFrom)   

import Time exposing (second)                                                   
import Task exposing (perform)                                                  
import Window exposing (Size, size)                                             
import Array exposing (Array, fromList)    


initWinSize : Size
initWinSize = (Size 1855 980)


init : Int -> List (Maybe BallOfWool) -> Machine BallOfWool Kitten ->
  UserTransTable BallOfWool Kitten -> Int -> Int -> List (Maybe BallOfWool) ->  
  Array (Cell Kitten) -> Array (Cell (Maybe BallOfWool)) -> (Model, Cmd Msg) 
init basketsNumb inp machine table level expPos expRes usedCats usedBalls =
  ((initModel initWinSize basketsNumb inp machine 
              table level expPos expRes usedCats usedBalls) 
  , perform (\_ -> Debug.crash "task") WindowSize size                          
  )


initModel : Size -> Int -> List (Maybe BallOfWool) -> Machine BallOfWool Kitten 
  -> UserTransTable BallOfWool Kitten -> Int -> Int -> List (Maybe BallOfWool)  
  -> Array (Cell Kitten) -> Array (Cell (Maybe BallOfWool)) -> Model
initModel winSize basketsNumb inp machine table 
          level expPos expRes usedCats usedBalls =                               
  { options = (initOptions winSize basketsNumb)
  , modelMachine = (initModelMachine machine inp)
  , transTables = (initTransTables table)
  , imgParam = (initImgParam machine)
  , levels = (initLevels level)
  , expResults = (initExpResults expPos expRes)
  , flags = initFlags
  , usedObj = (initObjectsSet usedCats usedBalls)
  }


initOptions : Size -> Int -> ModelOptions
initOptions winSize basketsNumb =
  { winSize        = winSize                                                            
  , timeUnit       = second 
  , whenGameStarts = 0                                                          
  , currTime       = 0  
  , tapeCellsNumb  = basketsNumb
  }


initModelMachine : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
                   ModelMachine
initModelMachine machine inp = 
  { input       = inp                                                                 
  , machine     = machine                                                           
  , machineCfgs = [(initMachineCfg machine inp machine.initHeadPosForMach)]
  }


initTransTables : UserTransTable BallOfWool Kitten -> ModelTransTables
initTransTables initTable =
  { trTableInit = initTable                                                         
  , trTableUser = initTable 
  }


initImgParam : Machine BallOfWool Kitten -> ModelImgParam
initImgParam machine =
  { catLeft    = catThinkX
  , menuCatTop = (menuItemTopFrom 1) 
  , catPos     = machine.initHeadPosForDraw + machine.initHeadPosForMach
  , catImg     = "../img/saimonThink/SaimonThinkW.png"                              
  , helpImg    = " "                                                               
  , finalImg   = " "  
  }

initLevels : Int -> ModelLevels
initLevels level = 
  { currLevel = level                                                           
  , maxLevel  = levelsNumber  
  }


initExpResults : Int -> List (Maybe BallOfWool) -> ModelExpResults
initExpResults expPos expRes =
  { expPos = expPos                                                             
  , expRes = expRes 
  }


initFlags : ModelFlags                                                  
initFlags =
  { ifPushRun   = False                                                           
  , ifStart     = True                                                              
  , ifPlay      = False                                                              
  , ifRules     = False                                                             
  , ifAuthors   = False                                                           
  , ifEnd       = False                                                               
  , ifCatLooks  = False                                                          
  , ifTableFull = True 
  }


initObjectsSet : Array (Cell Kitten) -> Array (Cell (Maybe BallOfWool)) -> 
                 ModelObjectsSet
initObjectsSet usedCats usedBalls =
  { usedCats  = usedCats
  , usedBalls = usedBalls
  , usedDirs  = (fromList [UserCell MoveLeft, UserCell MoveRight])
  }  


getInitByLevel : Int -> Model -> Model                                          
getInitByLevel level oldModel =                                                 
  case level of 
    1 -> -- Demo
      (initModel oldModel.options.winSize basketsNumbDemo inputDemo 
                        machineDemo transTableDemo 1 expectedPosDemo 
                        expectedResultDemo usedCatsDemo usedBallsDemo)                     
    2 -> -- 1_1
      (initModel oldModel.options.winSize basketsNumb1_1 input1_1 machine1_1 
                            transTable1_1 2 expectedPos1_1 expectedResult1_1 
                            usedCats1_1 usedBalls1_1)
    3 -> -- 1_2
      (initModel oldModel.options.winSize basketsNumb1_2 input1_2 machine1_2 
                            transTable1_2 3 expectedPos1_2 expectedResult1_2 
                            usedCats1_2 usedBalls1_2)  
    4 -> -- 1_3
      (initModel oldModel.options.winSize basketsNumb1_3 input1_3 machine1_3 
                            transTable1_3 4 expectedPos1_3 expectedResult1_3 
                            usedCats1_3 usedBalls1_3)  
    5 -> -- 2_1
      (initModel oldModel.options.winSize basketsNumb2_1 input2_1 machine2_1 
                            transTable2_1 5 expectedPos2_1 expectedResult2_1 
                            usedCats2_1 usedBalls2_1)  
    6 -> -- 2_2
      (initModel oldModel.options.winSize basketsNumb2_2 input2_2 machine2_2 
                            transTable2_2 6 expectedPos2_2 expectedResult2_2 
                            usedCats2_2 usedBalls2_2) 
    7 -> -- 2_3
      (initModel oldModel.options.winSize basketsNumb2_3 input2_3 machine2_3 
                            transTable2_3 7 expectedPos2_3 expectedResult2_3 
                            usedCats2_3 usedBalls2_3)  
    8 -> -- 3_1
      (initModel oldModel.options.winSize basketsNumb3_1 input3_1 machine3_1 
                            transTable3_1 8 expectedPos3_1 expectedResult3_1 
                            usedCats3_1 usedBalls3_1)     
    9 -> -- 3_2
      (initModel oldModel.options.winSize basketsNumb3_2 input3_2 machine3_2 
                            transTable3_2 9 expectedPos3_2 expectedResult3_2 
                            usedCats3_2 usedBalls3_2)
    10 -> -- 3_3
      (initModel oldModel.options.winSize basketsNumb3_3 input3_3 machine3_3 
                           transTable3_3 10 expectedPos3_3 expectedResult3_3 
                           usedCats3_3 usedBalls3_3) 
    11 -> -- 3_4
      (initModel oldModel.options.winSize basketsNumb3_4 input3_4 machine3_4 
                            transTable3_4 11 expectedPos3_4 expectedResult3_4 
                            usedCats3_4 usedBalls3_4)  
    12 -> -- 3_5
      (initModel oldModel.options.winSize basketsNumb3_5 input3_5 machine3_5 
                           transTable3_5 12 expectedPos3_5 expectedResult3_5 
                           usedCats3_5 usedBalls3_5) 
    13 -> -- 3_6
      (initModel oldModel.options.winSize basketsNumb3_6 input3_6 machine3_6    
                           transTable3_6 13 expectedPos3_6 expectedResult3_6    
                           usedCats3_6 usedBalls3_6)
    14 -> -- 3_7
      (initModel oldModel.options.winSize basketsNumb3_7 input3_7 machine3_7    
                           transTable3_7 14 expectedPos3_7 expectedResult3_7    
                           usedCats3_7 usedBalls3_7)   
    15 -> -- 3_8                                                               
      (initModel oldModel.options.winSize basketsNumb3_8 input3_8 machine3_8    
                           transTable3_8 15 expectedPos3_8 expectedResult3_8    
                           usedCats3_8 usedBalls3_8)                            
    16 -> -- 3_9                                                                
      (initModel oldModel.options.winSize basketsNumb3_9 input3_9 machine3_9    
                           transTable3_9 16 expectedPos3_9 expectedResult3_9    
                           usedCats3_9 usedBalls3_9)  
    17 -> -- 4_1
      (initModel oldModel.options.winSize basketsNumb4_1 input4_1 machine4_1    
                           transTable4_1 17 expectedPos4_1 expectedResult4_1    
                           usedCats4_1 usedBalls4_1)  
    18 -> -- 4_2
      (initModel oldModel.options.winSize basketsNumb4_2 input4_2 machine4_2    
                           transTable4_2 18 expectedPos4_2 expectedResult4_2    
                           usedCats4_2 usedBalls4_2)  
    19 -> -- 4_3
      (initModel oldModel.options.winSize basketsNumb4_3 input4_3 machine4_3    
                           transTable4_3 19 expectedPos4_3 expectedResult4_3    
                           usedCats4_3 usedBalls4_3)  
    20 -> -- 4_4
      (initModel oldModel.options.winSize basketsNumb4_4 input4_4 machine4_4    
                           transTable4_4 20 expectedPos4_4 expectedResult4_4    
                           usedCats4_4 usedBalls4_4)      
    21 -> -- 4_5
      (initModel oldModel.options.winSize basketsNumb4_5 input4_5 machine4_5    
                           transTable4_5 21 expectedPos4_5 expectedResult4_5    
                           usedCats4_5 usedBalls4_5)
    22 -> -- 4_6
      (initModel oldModel.options.winSize basketsNumb4_6 input4_6 machine4_6    
                           transTable4_6 22 expectedPos4_6 expectedResult4_6    
                           usedCats4_6 usedBalls4_6) 
    23 -> -- 4_7
      (initModel oldModel.options.winSize basketsNumb4_7 input4_7 machine4_7    
                           transTable4_7 23 expectedPos4_7 expectedResult4_7    
                           usedCats4_7 usedBalls4_7) 
    24 -> -- 5_1
      (initModel oldModel.options.winSize basketsNumb5_1 input5_1 machine5_1    
                           transTable5_1 24 expectedPos5_1 expectedResult5_1    
                           usedCats5_1 usedBalls5_1)        
    25 -> -- 5_2
      (initModel oldModel.options.winSize basketsNumb5_2 input5_2 machine5_2    
                           transTable5_2 25 expectedPos5_2 expectedResult5_2    
                           usedCats5_2 usedBalls5_2)
    26 -> -- 5_3
      (initModel oldModel.options.winSize basketsNumb5_3 input5_3 machine5_3    
                           transTable5_3 26 expectedPos5_3 expectedResult5_3    
                           usedCats5_3 usedBalls5_3)   
    27 -> -- 5_4
      (initModel oldModel.options.winSize basketsNumb5_4 input5_4 machine5_4    
                           transTable5_4 27 expectedPos5_4 expectedResult5_4    
                           usedCats5_4 usedBalls5_4) 
    28 -> -- 5_5
      (initModel oldModel.options.winSize basketsNumb5_5 input5_5 machine5_5    
                           transTable5_5 28 expectedPos5_5 expectedResult5_5    
                           usedCats5_5 usedBalls5_5)
    29 -> -- 5_6
      (initModel oldModel.options.winSize basketsNumb5_6 input5_6 machine5_6    
                           transTable5_6 29 expectedPos5_6 expectedResult5_6    
                           usedCats5_6 usedBalls5_6)
    _  -> -- 5_7
      (initModel oldModel.options.winSize basketsNumb5_7 input5_7 machine5_7    
                           transTable5_7 30 expectedPos5_7 expectedResult5_7    
                           usedCats5_7 usedBalls5_7)   

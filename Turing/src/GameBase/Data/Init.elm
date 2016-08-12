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
            (levelsNumber, machineDemo, inputDemo, transTableDemo,              
             expectedResultDemo, expectedPosDemo, usedCatsDemo, usedBallsDemo)  
import GameBase.Data.LevelsData.TasksBlock1Data exposing                                   
            (machine1_1, input1_1, transTable1_1,                               
             expectedResult1_1, expectedPos1_1, usedCats1_1, usedBalls1_1,      
             machine1_2, input1_2, transTable1_2,                               
             expectedResult1_2, expectedPos1_2, usedCats1_2, usedBalls1_2,      
             machine1_3, input1_3, transTable1_3,                               
             expectedResult1_3, expectedPos1_3, usedCats1_3, usedBalls1_3)  
import GameBase.Data.LevelsData.TasksBlock2Data exposing                        
            (machine2_1, input2_1, transTable2_1,                               
             expectedResult2_1, expectedPos2_1, usedCats2_1, usedBalls2_1,      
             machine2_2, input2_2, transTable2_2,                               
             expectedResult2_2, expectedPos2_2, usedCats2_2, usedBalls2_2,
             machine2_3, input2_3, transTable2_3,                               
             expectedResult2_3, expectedPos2_3, usedCats2_3, usedBalls2_3) 
import GameBase.Data.LevelsData.TasksBlock3Data exposing                        
            (machine3_1, input3_1, transTable3_1,                               
             expectedResult3_1, expectedPos3_1, usedCats3_1, usedBalls3_1,      
             machine3_2, input3_2, transTable3_2,                               
             expectedResult3_2, expectedPos3_2, usedCats3_2, usedBalls3_2,      
             machine3_3, input3_3, transTable3_3,                               
             expectedResult3_3, expectedPos3_3, usedCats3_3, usedBalls3_3,
             machine3_4, input3_4, transTable3_4,                               
             expectedResult3_4, expectedPos3_4, usedCats3_4, usedBalls3_4, 
             machine3_5, input3_5, transTable3_5,                               
             expectedResult3_5, expectedPos3_5, usedCats3_5, usedBalls3_5) 
import GameBase.UI.MainObjects.Cat exposing (catThinkX)
import GameBase.UI.ControlObjects.ControlLabelsParam exposing (menuItemTopFrom)   

import Time exposing (second)                                                   
import Task exposing (perform)                                                  
import Window exposing (Size, size)                                             
import Array exposing (Array, fromList)    


initWinSize : Size
initWinSize = (Size 1855 980)


init : List (Maybe BallOfWool) -> Machine BallOfWool Kitten ->
  UserTransTable BallOfWool Kitten -> Int -> Int -> List (Maybe BallOfWool) ->  
  Array (Cell Kitten) -> Array (Cell (Maybe BallOfWool)) -> (Model, Cmd Msg) 
init inp machine table level expPos expRes usedCats usedBalls =
  ((initModel initWinSize inp machine table level expPos expRes usedCats 
              usedBalls) 
  , perform (\_ -> Debug.crash "task") WindowSize size                          
  )


initModel : Size -> List (Maybe BallOfWool) -> Machine BallOfWool Kitten ->                  
  UserTransTable BallOfWool Kitten -> Int -> Int -> List (Maybe BallOfWool) -> 
  Array (Cell Kitten) -> Array (Cell (Maybe BallOfWool)) -> Model
initModel winSize inp machine table level expPos expRes usedCats usedBalls =                               
  { options = (initOptions winSize)
  , modelMachine = (initModelMachine machine inp)
  , transTables = (initTransTables table)
  , imgParam = (initImgParam machine)
  , levels = (initLevels level)
  , expResults = (initExpResults expPos expRes)
  , flags = initFlags
  , usedObj = (initObjectsSet usedCats usedBalls)
  }


initOptions : Size -> ModelOptions
initOptions winSize =
  { winSize        = winSize                                                            
  , timeUnit       = second                                                           
  , whenGameStarts = 0                                                          
  , currTime       = 0  
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
      (initModel oldModel.options.winSize inputDemo machineDemo transTableDemo 
              1 expectedPosDemo expectedResultDemo usedCatsDemo usedBallsDemo)                     
    2 -> -- 1_1
      (initModel oldModel.options.winSize input1_1 machine1_1 transTable1_1 2 
                    expectedPos1_1 expectedResult1_1 usedCats1_1 usedBalls1_1)
    3 -> -- 1_2
      (initModel oldModel.options.winSize input1_2 machine1_2 transTable1_2 3   
                    expectedPos1_2 expectedResult1_2 usedCats1_2 usedBalls1_2)  
    4 -> -- 1_3
      (initModel oldModel.options.winSize input1_3 machine1_3 transTable1_3 4   
                    expectedPos1_3 expectedResult1_3 usedCats1_3 usedBalls1_3)  
    5 -> -- 2_1
      (initModel oldModel.options.winSize input2_1 machine2_1 transTable2_1 5   
                    expectedPos2_1 expectedResult2_1 usedCats2_1 usedBalls2_1)  
    6 -> -- 2_2
      (initModel oldModel.options.winSize input2_2 machine2_2 transTable2_2 6
       expectedPos2_2 expectedResult2_2 usedCats2_2 usedBalls2_2) 
    7 -> -- 2_3
      (initModel oldModel.options.winSize input2_3 machine2_3 transTable2_3 7   
       expectedPos2_3 expectedResult2_3 usedCats2_3 usedBalls2_3)  
    8 -> -- 3_1
      (initModel oldModel.options.winSize input3_1 machine3_1 transTable3_1 8
       expectedPos3_1 expectedResult3_1 usedCats3_1 usedBalls3_1)     
    9 -> -- 3_2
      (initModel oldModel.options.winSize input3_2 machine3_2 transTable3_2 9  
       expectedPos3_2 expectedResult3_2 usedCats3_2 usedBalls3_2)
    10 -> -- 3_3
      (initModel oldModel.options.winSize input3_3 machine3_3 transTable3_3 10   
       expectedPos3_3 expectedResult3_3 usedCats3_3 usedBalls3_3) 
    11 -> -- 3_4
      (initModel oldModel.options.winSize input3_4 machine3_4 transTable3_4 11  
       expectedPos3_4 expectedResult3_4 usedCats3_4 usedBalls3_4)  
    _ -> -- 3_5
      (initModel oldModel.options.winSize input3_5 machine3_5 transTable3_5 12  
       expectedPos3_5 expectedResult3_5 usedCats3_5 usedBalls3_5) 

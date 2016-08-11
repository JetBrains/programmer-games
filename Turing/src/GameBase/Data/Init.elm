module GameBase.Data.Init exposing (init, getInitByLevel)

import TuringMachine.InitUpdate exposing (initMachineCfg) 
import TuringMachine.TuringTypes exposing (Machine, UserTransTable, 
                                           Direction(..), Cell(..))
import GameBase.Data.GameTypes exposing                                         
                        (BallOfWool(..), Kitten(..), Position, Msg(..), Model,  
                         ModelOptions, ModelMachine, ModelTransTables,          
                         ModelImgParam, ModelLevels, ModelExpResults,           
                         ModelFlags, ModelObjectsSet)  
import GameBase.Data.LevelsData exposing 
  (levelsNumber, machineDemo, inputDemo, transTableDemo, expectedResultDemo,    
  expectedPosDemo, usedCatsDemo, usedBallsDemo, machine1_1, input1_1,          
  transTable1_1, expectedResult1_1, expectedPos1_1, usedCats1_1, usedBalls1_1, 
  machine1_2, input1_2, transTable1_2, expectedResult1_2, expectedPos1_2,      
  usedCats1_2, usedBalls1_2, machine1_3, input1_3, transTable1_3,              
  expectedResult1_3, expectedPos1_3, usedCats1_3, usedBalls1_3)  
import GameBase.UI.MainObjects.Cat exposing 
                             (catThinkX, catShowSndItemY)  

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
  , menuCatTop = catShowSndItemY
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
    _ -> -- 1_3
      (initModel oldModel.options.winSize input1_3 machine1_3 transTable1_3 4   
                    expectedPos1_3 expectedResult1_3 usedCats1_3 usedBalls1_3)  

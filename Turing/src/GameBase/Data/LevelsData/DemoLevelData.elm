module GameBase.Data.LevelsData.DemoLevelData exposing 
       (levelsNumber, basketsNumbDemo, machineDemo, inputDemo, transTableDemo, 
        expectedResultDemo, expectedPosDemo, usedCatsDemo, usedBallsDemo)
                                                                                
import TuringMachine.TuringTypes exposing ( Direction(..), Machine,             
                                            UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (threeBaskets)  

import Array exposing (Array, fromList)                                         
                                                                                
                                                                                
levelsNumber : Int                                                              
levelsNumber = 12


--DEMO MACHINE----------------------------------------------------------------- 
                                                                                
-- recolor input ball in red color                                              

basketsNumbDemo : Int
basketsNumbDemo = threeBaskets

machineDemo : Machine BallOfWool Kitten                                         
machineDemo =                                                                   
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 0
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTableDemo : UserTransTable BallOfWool Kitten                               
transTableDemo =                                                                
  fromList                                                                      
    [ { key = (White, Just Yellow)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ] 

inputDemo : List (Maybe BallOfWool)                                             
inputDemo =                                                                     
  [Just Yellow]                                                                 
                                                                                
expectedResultDemo : List (Maybe BallOfWool)                                    
expectedResultDemo =                                                            
  [Just Red, Nothing]                                                           
                                                                                
expectedPosDemo : Int                                                           
expectedPosDemo = 2                                                      
                                                                                
usedCatsDemo : Array (Cell Kitten)                                              
usedCatsDemo = fromList [UserCell White]                                        
                                                                                
usedBallsDemo : Array (Cell (Maybe BallOfWool))                                 
usedBallsDemo = fromList [UserCell (Just Yellow), UserCell (Just Red),          
                          UserCell Nothing]                                     
-------------------------------------------------------------------------------

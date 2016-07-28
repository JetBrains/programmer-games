module GameBase.Data.LevelsData exposing (machine1, transTable1, input1, 
                                          expectedResult1, expectedPos1, 
                                          machine2, transTable2, input2, 
                                          expectedResult2, expectedPos2)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))
import TuringMachine.TuringTypes exposing (Direction(..), Machine, TransTable)
import TuringMachine.RunTuring exposing (transFunc)

import Array exposing (fromList)

------------------------------------------------------------------------------  

machine1 : Machine BallOfWool Kitten                                            
machine1 =                                                                      
  { transition = (transFunc transTable1 (Violet, Nothing, MoveLeft))            
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable1 : TransTable BallOfWool Kitten                                      
transTable1 =                                                                   
  fromList                                                                      
    [ { key = (White, Just Yellow),                                             
        value = (Orange, Just Red, MoveRight)                                   
      }                                                                         
    ]                                                                           
                                                                                
input1 : List (Maybe BallOfWool)                                                
input1 =                                                                        
  [Just Yellow]                                                                 
                                                                                
expectedResult1 : List (Maybe BallOfWool)                                       
expectedResult1 =                                                               
  [Just Red, Nothing]                                                           
                                                                                
expectedPos1 : Int                                                              
expectedPos1 = 2                                                                
                                                                                
------------------------------------------------------------------------------ 

machine2 : Machine BallOfWool Kitten                                            
machine2 =                                                                      
  { transition = (transFunc transTable2 (Violet, Nothing, MoveLeft))            
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable2 : TransTable BallOfWool Kitten                                      
transTable2 =                                                                   
  fromList                                                                      
    [ { key = (White, Just Red),                                                
        value = (White, Just Red, MoveRight)}                                   
    , { key = (White, Just Yellow),                                             
        value = (White, Just Yellow, MoveRight)}                                
    , { key = (White, Just Green),                                              
       value = (White, Just Green, MoveRight)}                                  
    , { key = (White, Just Blue),                                               
        value = (White, Just Blue, MoveRight)}                                  
    , { key = (White, Nothing),                                                 
        value = (LightGrey, Just Red, MoveLeft)}                                
    , { key = (LightGrey, Just Red),                                            
        value = (LightGrey, Just Red, MoveLeft)}                                
    , { key = (LightGrey, Just Yellow),                                         
        value = (LightGrey, Just Yellow, MoveLeft)}                             
    , { key = (LightGrey, Just Green),                                          
        value = (LightGrey, Just Green, MoveLeft)}                              
    , { key = (LightGrey, Just Blue),                                           
        value = (LightGrey, Just Blue, MoveLeft)}                               
    , { key = (LightGrey, Nothing),                                             
        value = (Orange, Just Yellow, MoveRight)}                               
    ]                                                                           
                                                                                
input2 : List (Maybe BallOfWool)                                                
input2 =                                                                        
  [Just Red, Just Yellow, Just Green, Just Blue]                                
                                                                                
expectedResult2 : List (Maybe BallOfWool)                                       
expectedResult2 =                                                               
  [Just Blue, Just Red, Just Yellow, Just Green, Just Blue, Just Red]           
                                                                                
expectedPos2 : Int                                                              
expectedPos2 = 1                                                                
                                                                                
------------------------------------------------------------------------------  

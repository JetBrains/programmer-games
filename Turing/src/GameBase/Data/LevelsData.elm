module GameBase.Data.LevelsData exposing (machine1, transTable1, input1, 
                                          expectedResult1, expectedPos1, 
                                          machine2, transTable2, input2, 
                                          expectedResult2, expectedPos2)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))
import TuringMachine.TuringTypes exposing ( Direction(..), Machine, 
                                            UserTransTable, Cell(..))
import TuringMachine.RunTuring exposing (transFunc)

import Array exposing (fromList)

------------------------------------------------------------------------------  

machine1 : Machine BallOfWool Kitten                                            
machine1 =                                                                      
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))           
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                           

transTable1 : UserTransTable BallOfWool Kitten                                      
transTable1 =                                                                   
  fromList                                                                      
    [ { key = (White, Just Yellow)                                            
      , value = { state = StableCell (Orange)
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveRight)
                }
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
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))      
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable2 : UserTransTable BallOfWool Kitten                                      
transTable2 =                                                                   
  fromList                                                                      
    [ { key = (White, Just Red)                                                
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Red)
                , dir   = StableCell (MoveRight)
                }
      }                         
    , { key = (White, Just Yellow)                                             
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Yellow)
                , dir   = StableCell (MoveRight)
                }
      }                                
    , { key = (White, Just Green)
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Green)
                , dir   = StableCell (MoveRight)
                }
      }                                  
    , { key = (White, Just Blue)                                               
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Blue)
                , dir   = StableCell (MoveRight)
                }
      }                                  
    , { key = (White, Nothing)                                                
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveLeft)
                }
      }                                
    , { key = (LightGrey, Just Red)                                            
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveLeft)
                }
      }                                
    , { key = (LightGrey, Just Yellow)                                         
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Yellow)
                , dir   = StableCell (MoveLeft)
                }
      }                             
    , { key = (LightGrey, Just Green)                                          
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveLeft)
                }
      }                              
    , { key = (LightGrey, Just Blue)                                           
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Blue) 
                , dir   = StableCell (MoveLeft)
                }
      }                               
    , { key = (LightGrey, Nothing)                                            
      , value = { state = EmptyCell
                , symb  = StableCell (Just Yellow)
                , dir   = StableCell (MoveRight)
                }
      }                               
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

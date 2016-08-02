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
      , value = { state = EmptyCell --StableCell (White)
                , symb  = EmptyCell --StableCell (Just Red)
                , dir   = EmptyCell --StableCell (MoveRight)
                }
      }                         
    , { key = (White, Just Yellow)                                             
      , value = { state = EmptyCell --StableCell (White)
                , symb  = EmptyCell --StableCell (Just Yellow)
                , dir   = EmptyCell --StableCell (MoveRight)
                }
      }                                
    , { key = (White, Just Green)
      , value = { state = EmptyCell --StableCell (White)
                , symb  = EmptyCell --StableCell (Just Green)
                , dir   = EmptyCell --StableCell (MoveRight)
                }
      }                                  
    , { key = (White, Just Blue)                                               
      , value = { state = EmptyCell --StableCell (White)
                , symb  = EmptyCell --StableCell (Just Blue)
                , dir   = EmptyCell --StableCell (MoveRight)
                }
      }                                  
    , { key = (White, Nothing)                                                
      , value = { state = EmptyCell   
                , symb  = EmptyCell 
                , dir   = EmptyCell --StableCell (MoveLeft)
                }
      }                                
    , { key = (LightGrey, Just Red)                                            
      , value = { state = EmptyCell --StableCell (LightGrey)
                , symb  = EmptyCell --StableCell (Just Red) 
                , dir   = EmptyCell --StableCell (MoveLeft)
                }
      }                                
    , { key = (LightGrey, Just Yellow)                                         
      , value = { state = EmptyCell --StableCell (LightGrey) 
                , symb  = EmptyCell --StableCell (Just Yellow)
                , dir   = EmptyCell 
                }
      }                             
    , { key = (LightGrey, Just Green)                                          
      , value = { state = EmptyCell --StableCell (LightGrey) 
                , symb  = EmptyCell --StableCell (Just Green) 
                , dir   = EmptyCell --StableCell (MoveLeft)
                }
      }                              
    , { key = (LightGrey, Just Blue)                                           
      , value = { state = EmptyCell --StableCell (LightGrey) 
                , symb  = EmptyCell --StableCell (Just Blue) 
                , dir   = EmptyCell --StableCell (MoveLeft)
                }
      }                               
    , { key = (LightGrey, Nothing)                                            
      , value = { state = EmptyCell --StableCell (Orange) 
                , symb  = EmptyCell --StableCell (Just Yellow)
                , dir   = EmptyCell --StableCell (MoveRight)
                }
      }   
    , { key = (Grey, Just Red)                                             
      , value = { state = EmptyCell --StableCell (LightGrey)                    
                , symb  = EmptyCell --StableCell (Just Red)                     
                , dir   = EmptyCell --StableCell (MoveLeft)                     
                }                                                               
      }                                                                         
    , { key = (Grey, Just Yellow)                                          
      , value = { state = EmptyCell --StableCell (LightGrey)                    
                , symb  = EmptyCell --StableCell (Just Yellow)                  
                , dir   = EmptyCell                                             
                }                                                               
      }                                                                         
    , { key = (Grey, Just Green)                                           
      , value = { state = EmptyCell --StableCell (LightGrey)                    
                , symb  = EmptyCell --StableCell (Just Green)                   
                , dir   = EmptyCell --StableCell (MoveLeft)                     
                }                                                               
      }                                                                         
    , { key = (Grey, Just Blue)                                            
      , value = { state = EmptyCell --StableCell (LightGrey)                    
                , symb  = EmptyCell --StableCell (Just Blue)                    
                , dir   = EmptyCell --StableCell (MoveLeft)                     
                }                                                               
      }                                                                         
    , { key = (Grey, Nothing)                                              
      , value = { state = EmptyCell --StableCell (Orange)                       
                , symb  = EmptyCell --StableCell (Just Yellow)                  
                , dir   = EmptyCell --StableCell (MoveRight)                    
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

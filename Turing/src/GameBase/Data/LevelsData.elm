module GameBase.Data.LevelsData exposing (machine1, transTable1, input1,        
                                          expectedResult1, expectedPos1,        
                                          usedCats1, usedBalls1,          
                                          machine2, transTable2, input2,      
                                          expectedResult2, expectedPos2,        
                                          usedCats2, usedBalls2, levelsNumber)  

import TuringMachine.TuringTypes exposing ( Direction(..), Machine, 
                                            UserTransTable, Cell(..))
import TuringMachine.RunTuring exposing (transFunc)
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))

import Array exposing (Array, fromList)


levelsNumber : Int
levelsNumber = 2

machine1 : Machine BallOfWool Kitten                                            
machine1 =                                                                      
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))           
  , initHeadPosForDraw = 3
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
      , clickNum = 0
      }                                                                         
    ]                                                                           
                                                                                
input1 : List (Maybe BallOfWool)                                                
input1 =                                                                        
  [Just Yellow]                                                                 
                                                                                
expectedResult1 : List (Maybe BallOfWool)                                       
expectedResult1 =                                                               
  [Just Red, Nothing]                                                           
                                                                                
expectedPos1 : Int                                                              
expectedPos1 = 4

usedCats1 : Array (Cell Kitten)
usedCats1 = fromList [UserCell White]

usedBalls1 : Array (Cell (Maybe BallOfWool))
usedBalls1 = fromList [UserCell (Just Yellow), UserCell (Just Red), 
                       UserCell Nothing]

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
      , clickNum = 0          
      }                         
    , { key = (White, Just Yellow)                                             
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Yellow)
                , dir   = StableCell (MoveRight)
                }
      , clickNum = 0
      }                                
    , { key = (White, Just Green)
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Green)
                , dir   = StableCell (MoveRight)
                }
      , clickNum = 0
      }                                  
    , { key = (White, Just Blue)                                               
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Blue)
                , dir   = StableCell (MoveRight)
                }
      , clickNum = 0
      }                                  
    , { key = (White, Nothing)                                                
      , value = { state = EmptyCell   
                , symb  = EmptyCell 
                , dir   = StableCell (MoveLeft)
                }
      , clickNum = 0
      }                                
    , { key = (LightGrey, Just Red)                                            
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveLeft)
                }
      , clickNum = 0
      }                                
    , { key = (LightGrey, Just Yellow)                                         
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Yellow)
                , dir   = EmptyCell 
                }
      , clickNum = 0
      }                             
    , { key = (LightGrey, Just Green)                                          
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveLeft)
                }
      , clickNum = 0
      }                              
    , { key = (LightGrey, Just Blue)                                           
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Blue) 
                , dir   = StableCell (MoveLeft)
                }
      , clickNum = 0
      }                               
    , { key = (LightGrey, Nothing)                                            
      , value = { state = StableCell (Orange) 
                , symb  = EmptyCell
                , dir   = StableCell (MoveRight)
                }
      , clickNum = 0
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

usedCats2 : Array (Cell Kitten) 
usedCats2 = fromList [UserCell White, UserCell LightGrey]                                                         
                                                                                
usedBalls2 : Array (Cell (Maybe BallOfWool))  
usedBalls2 = fromList [UserCell (Just Red), UserCell (Just Yellow), 
                       UserCell (Just Green), UserCell (Just Blue), 
                       UserCell Nothing]

module GameBase.Data.LevelsData.TasksBlock3Data_Light exposing                        
            (basketsNumb3_1, machine3_1, input3_1, transTable3_1,                               
             expectedResult3_1, expectedPos3_1, usedCats3_1, usedBalls3_1,      
             basketsNumb3_2, machine3_2, input3_2, transTable3_2,                               
             expectedResult3_2, expectedPos3_2, usedCats3_2, usedBalls3_2,      
             basketsNumb3_3, machine3_3, input3_3, transTable3_3,                               
             expectedResult3_3, expectedPos3_3, usedCats3_3, usedBalls3_3, 
             basketsNumb3_4, machine3_4, input3_4, transTable3_4,                               
             expectedResult3_4, expectedPos3_4, usedCats3_4, usedBalls3_4)
import TuringMachine.TuringTypes exposing (Direction(..), Machine,             
                                           UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (nineBaskets) 
--(threeBaskets, fourBaskets, fiveBaskets)

import Array exposing (Array, fromList)


-- BLOCK 3 : Balls replacement in the input word ("in place")------------------


-- 3_1 - Change input ball to green ball

basketsNumb3_1 : Int                                                            
basketsNumb3_1 = nineBaskets --threeBaskets

machine3_1 : Machine BallOfWool Kitten                                          
machine3_1 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 4 --1
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_1 : UserTransTable BallOfWool Kitten                               
transTable3_1 =                                                                
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]    

input3_1 : List (Maybe BallOfWool)                                              
input3_1 =                                                                      
  [Just Red]                                                                     
                                                                                
expectedResult3_1 : List (Maybe BallOfWool)                                     
expectedResult3_1 =                                                             
  [Just Green, Nothing]                                  
                                                                                
expectedPos3_1 : Int                                                            
expectedPos3_1 = 5 --2                                                             
                                                                                
usedCats3_1 : Array (Cell Kitten)                                               
usedCats3_1 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]

usedBalls3_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_2 - Change one last ball 

basketsNumb3_2 : Int                                                            
basketsNumb3_2 = nineBaskets --fiveBaskets

machine3_2 : Machine BallOfWool Kitten                                          
machine3_2 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 3 --1
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_2 : UserTransTable BallOfWool Kitten                                
transTable3_2 =                                                                 
  fromList 
    [ { key   = (White, Just Yellow)                                                  
      , value = { state = EmptyCell                                            
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                             
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Nothing)                                              
      , value = { state = EmptyCell                               
                , symb  = StableCell (Nothing)                       
                , dir   = StableCell (MoveLeft)                             
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (LightGrey, Just Yellow)                                             
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    ]
                                                                                
input3_2 : List (Maybe BallOfWool)                                              
input3_2 =                                                                      
  [Just Yellow, Just Yellow, Just Yellow, Nothing]                                                                    
                                                                                
expectedResult3_2 : List (Maybe BallOfWool)                                     
expectedResult3_2 =                                                             
  [Just Yellow, Just Yellow, Just Blue, Nothing]

expectedPos3_2 : Int                                                            
expectedPos3_2 = 4 --2                                                              
                                                                                
usedCats3_2 : Array (Cell Kitten)                                               
usedCats3_2 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_3 - Replace all balls with blue ball

basketsNumb3_3 : Int                                                            
basketsNumb3_3 = nineBaskets --fiveBaskets       

machine3_3 : Machine BallOfWool Kitten                                          
machine3_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 3 --1
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_3 : UserTransTable BallOfWool Kitten                                
transTable3_3 =                                                                 
  fromList   
    [ { key   = (White, Just Red)                                              
      , value = { state = EmptyCell            
                , symb  = StableCell (Just Blue)                       
                , dir   = StableCell (MoveRight)                              
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (White)                                
                , symb  = EmptyCell
                , dir   = StableCell (MoveRight)          
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Just Green)                                              
      , value = { state = StableCell (White)      
                , symb  = StableCell (Just Blue)                       
                , dir   = StableCell (MoveRight)                             
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Nothing)
      , value = { state = StableCell (Orange) 
                , symb  = StableCell (Nothing) 
                , dir   = StableCell (MoveLeft)  
                } 
      , clickNum = 0
      }
    ]
                                                                                
input3_3 : List (Maybe BallOfWool)                                              
input3_3 =                                                                      
  [Just Red, Just Yellow, Just Green, Nothing]                                                                    
                                                                                
expectedResult3_3 : List (Maybe BallOfWool)                                     
expectedResult3_3 =                                                             
  [Just Blue, Just Blue, Just Blue, Nothing] 

expectedPos3_3 : Int                                                            
expectedPos3_3 = 5 --3                                                              
                                                                                
usedCats3_3 : Array (Cell Kitten)                                               
usedCats3_3 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown] 
                                                                                
usedBalls3_3 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_3 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_4 - Swap two balls

basketsNumb3_4 : Int                                                            
basketsNumb3_4 = nineBaskets --fourBaskets

machine3_4 : Machine BallOfWool Kitten                                          
machine3_4 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 3 --1
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_4 : UserTransTable BallOfWool Kitten                                
transTable3_4 =                                                                 
  fromList 
    [ { key   = (White, Just Yellow)                                                
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Blue)                                            
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (White, Just Blue)                                              
      , value = { state = StableCell (White)                               
                , symb  = EmptyCell        
                , dir   = StableCell (MoveRight)                              
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Nothing)                                                
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Nothing)
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    ]
                                                                                
input3_4 : List (Maybe BallOfWool)                                              
input3_4 =                                                                      
  [Just Yellow, Just Blue, Nothing]                                  
                                                                                
expectedResult3_4 : List (Maybe BallOfWool)                                     
expectedResult3_4 =                                                             
  [Just Blue, Just Yellow, Nothing]

expectedPos3_4 : Int                                                            
expectedPos3_4 = 4 --2                                                      
                                                                                
usedCats3_4 : Array (Cell Kitten)                                               
usedCats3_4 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_4 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_4 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
-------------------------------------------------------------------------------

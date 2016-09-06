module GameBase.Data.LevelsData.TasksBlock3Data_Hard exposing                        
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

import TuringMachine.TuringTypes exposing (Direction(..), Machine,              
                                           UserTransTable, Cell(..))            
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (nineBaskets)
--(fiveBaskets, sixBaskets, sevenBaskets, eightBaskets, nineBaskets)            
                                                                                
import Array exposing (Array, fromList)                                         
                                                                                
                                                                                
-- BLOCK 3 : Balls replacement in the input word ("in place")------------------


-- 3_5 - Delete all balls except the first ball (dont change empty word)
                                                                                
basketsNumb3_5 : Int                                                            
basketsNumb3_5 = nineBaskets --fiveBaskets                                                    
                                                                                
machine3_5 : Machine BallOfWool Kitten                                          
machine3_5 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 2 --0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_5 : UserTransTable BallOfWool Kitten                                
transTable3_5 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = EmptyCell                                
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)                               
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Green)                                               
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Blue)                                                
      , value = { state = StableCell (LightGrey)                     
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                    
    , { key   = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (LightGrey, Just Red)                                             
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Nothing)                                   
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Nothing)                       
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input3_5 : List (Maybe BallOfWool)                                              
input3_5 =                                                                      
  [Just Red, Just Yellow, Just Green, Just Blue, Nothing]                       
                                                                                
expectedResult3_5 : List (Maybe BallOfWool)                                     
expectedResult3_5 =                                                             
  [Just Red, Nothing, Nothing, Nothing, Nothing]                                
                                                                                
expectedPos3_5 : Int                                                            
expectedPos3_5 = 5 --3                                                              
                                                                                
usedCats3_5 : Array (Cell Kitten)                                               
usedCats3_5 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_5 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_5 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_6 - Delete all balls except the last ball (dont change empty word)  
                                                                                
basketsNumb3_6 : Int                                                            
basketsNumb3_6 = nineBaskets --sixBaskets                                                     
                                                                                
machine3_6 : Machine BallOfWool Kitten                                          
machine3_6 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1 --0                                                      
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_6 : UserTransTable BallOfWool Kitten                                
transTable3_6 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = EmptyCell                                    
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Green)                                               
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Blue)                                                
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Blue)                                
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
    , { key   = (LightGrey, Just Red)                                             
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = EmptyCell                                     
                , symb  = EmptyCell                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Red)                                                  
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                                
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Nothing)
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Grey, Just Blue)                                                 
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input3_6 : List (Maybe BallOfWool)                                              
input3_6 =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green, Just Blue, Nothing]              
                                                                                
expectedResult3_6 : List (Maybe BallOfWool)                                     
expectedResult3_6 =                                                             
  [Nothing, Nothing, Nothing, Nothing, Just Blue, Nothing]                      
                                                                                
expectedPos3_6 : Int                                                            
expectedPos3_6 = 2 --1                                                              
                                                                                
usedCats3_6 : Array (Cell Kitten)                                               
usedCats3_6 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_6 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_6 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_7 - Put green balls on the left side, yellow balls on the right side

basketsNumb3_7 : Int                                                            
basketsNumb3_7 = nineBaskets --eightBaskets

machine3_7 : Machine BallOfWool Kitten                                          
machine3_7 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
   
transTable3_7 : UserTransTable BallOfWool Kitten                                
transTable3_7 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Green)                                               
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Yellow)                                              
      , value = { state = EmptyCell                                              
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
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (LightGrey)                                            
                , symb  = EmptyCell                                              
                , dir   = StableCell (MoveRight)                                            
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (LightGrey, Just Yellow)                                         
      , value = { state = StableCell (LightGrey)                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (LightGrey, Nothing)                                                
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    ]

{-- alternative transTable3_7 and input3_7                                                                           
transTable3_7 : UserTransTable BallOfWool Kitten                                
transTable3_7 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Green)                                               
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Yellow)                                              
      , value = { state = EmptyCell                                
                , symb  = StableCell (Just Yellow)                              
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
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = EmptyCell                           
                , symb  = EmptyCell                  
                , dir   = EmptyCell                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                                
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = EmptyCell                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                              
      , value = { state = EmptyCell                                
                , symb  = EmptyCell                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]

input3_7 : List (Maybe BallOfWool)                                              
input3_7 =                                                                      
  [Just Green, Just Yellow, Just Green, Just Yellow, Just Green, Just Yellow]   
--}

input3_7 : List (Maybe BallOfWool)                                              
input3_7 =                                                                      
  [Just Green, Just Green, Just Yellow, Just Yellow, Just Green, Just Yellow]   
                                                                           
expectedResult3_7 : List (Maybe BallOfWool)                                     
expectedResult3_7 =                                                             
  [Just Green, Just Green, Just Green, Just Yellow, Just Yellow, Just Yellow,   
   Nothing]                                                                     
                                                                                
expectedPos3_7 : Int                                                            
expectedPos3_7 = 6                                                              
                                                                                
usedCats3_7 : Array (Cell Kitten)                                               
usedCats3_7 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_7 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_7 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_8 - Change yellow balls to green balls (in input word) and put all yellow 
-- balls after input word

basketsNumb3_8 : Int                                                            
basketsNumb3_8 = nineBaskets --sevenBaskets                                                   
                                                                                
machine3_8 : Machine BallOfWool Kitten                                          
machine3_8 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1 --0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }

transTable3_8 : UserTransTable BallOfWool Kitten                                
transTable3_8 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Yellow)                                              
      , value = { state = EmptyCell
                , symb  = EmptyCell
                , dir   = StableCell (MoveRight)
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Blue)                                                
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
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                               
                , symb  = StableCell (Just Yellow)                           
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                             
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Yellow)                    
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                              
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Green)                                               
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Blue)                                                
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Nothing)                                                  
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]   

input3_8 : List (Maybe BallOfWool)                                              
input3_8 =                                                                      
  [Just Yellow, Just Blue, Just Yellow, Just Blue]                              
                                                                                
expectedResult3_8 : List (Maybe BallOfWool)                                     
expectedResult3_8 =                                                             
  [Just Green, Just Blue, Just Green, Just Blue, Nothing,                       
   Just Yellow, Just Yellow]                                                    
                                                                                
expectedPos3_8 : Int                                                            
expectedPos3_8 = 4 --3                                                              
                                                                                
usedCats3_8 : Array (Cell Kitten)                                               
usedCats3_8 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_8 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_8 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 3_9 - Change each second ball to yellow ball                               
                                                                                
basketsNumb3_9 : Int                                                            
basketsNumb3_9 = nineBaskets                                                    
                                                                                
machine3_9 : Machine BallOfWool Kitten                                          
machine3_9 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_9 : UserTransTable BallOfWool Kitten                                
transTable3_9 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = EmptyCell                               
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)                                  
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Green)                                               
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Blue)                                                
      , value = { state = StableCell (LightGrey)                               
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
    , { key   = (LightGrey, Just Red)                                             
      , value = { state = StableCell (White)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (White)                                    
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input3_9 : List (Maybe BallOfWool)                                              
input3_9 =                                                                      
  [Just Red, Just Yellow, Just Green, Just Blue, Just Red, Just Green,          
   Just Blue, Just Red, Nothing]                                                
                                                                                
expectedResult3_9 : List (Maybe BallOfWool)                                     
expectedResult3_9 =                                                             
  [Just Red, Just Yellow, Just Green, Just Yellow, Just Red, Just Yellow,       
   Just Blue, Just Yellow, Nothing]                                             
                                                                                
expectedPos3_9 : Int                                                            
expectedPos3_9 = 7                                                              
                                                                                
usedCats3_9 : Array (Cell Kitten)                                               
usedCats3_9 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls3_9 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_9 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
-------------------------------------------------------------------------------

module GameBase.Data.LevelsData.TasksBlock5Data_Deletion exposing                                   
            (basketsNumb5_1, machine5_1, input5_1, transTable5_1,                               
             expectedResult5_1, expectedPos5_1, usedCats5_1, usedBalls5_1,
             basketsNumb5_2, machine5_2, input5_2, transTable5_2,                               
             expectedResult5_2, expectedPos5_2, usedCats5_2, usedBalls5_2,
             basketsNumb5_3, machine5_3, input5_3, transTable5_3,
             expectedResult5_3, expectedPos5_3, usedCats5_3, usedBalls5_3,
             basketsNumb5_4, machine5_4, input5_4, transTable5_4,               
             expectedResult5_4, expectedPos5_4, usedCats5_4, usedBalls5_4)

import TuringMachine.TuringTypes exposing (Direction(..), Machine,             
                                           UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (fourBaskets, fiveBaskets,
                                                sevenBaskets, nineBaskets)

import Array exposing (Array, fromList)


-- BLOCK 5 : deletion and insertion of symbols (by compression and extension of
-- word)-----------------------------------------------------------------------


-------DELETION----------------------------------------------------------------

-- 5_1 - Delete the second ball, if it exists, else change the word 
-- to empty word

basketsNumb5_1 : Int                                                            
basketsNumb5_1 = fourBaskets

machine5_1 : Machine BallOfWool Kitten                                          
machine5_1 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_1 : UserTransTable BallOfWool Kitten                                
transTable5_1 = 
  fromList  
    [ { key   = (White, Just Red)                                              
      , value = { state = EmptyCell 
                , symb  = EmptyCell                       
                , dir   = StableCell (MoveRight)                              
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Just Yellow)                                                
      , value = { state = StableCell (Grey)                                      
                , symb  = StableCell (Nothing)                                   
                , dir   = StableCell (MoveRight)                                   
                }                                                                 
      , clickNum = 0                                                              
      }   
    , { key   = (White, Just Green)                                              
      , value = { state = StableCell (Brown)                                             
                , symb  = StableCell (Nothing)                              
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
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (LightGrey, Just Yellow)                                              
      , value = { state = StableCell (Orange)                                     
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                           
    , { key   = (LightGrey, Just Green)                                          
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (LightGrey, Nothing)                                          
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                             
    , { key   = (Grey, Just Red)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                        
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }      
    , { key   = (Grey, Just Green)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
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
    , { key   = (Brown, Just Red)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                   
    , { key   = (Brown, Just Yellow)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Brown, Just Green)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (Brown, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    ]      
                                                                                
input5_1 : List (Maybe BallOfWool)                                              
input5_1 =                                                                      
  [Just Red, Just Yellow, Just Green, Nothing]                                                                     
                                                                                
expectedResult5_1 : List (Maybe BallOfWool)                                     
expectedResult5_1 =                                                             
  [Nothing, Just Red, Just Green, Nothing]                                           
                                                                                
expectedPos5_1 : Int                                                            
expectedPos5_1 = 2

usedCats5_1 : Array (Cell Kitten)                                               
usedCats5_1 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_2 - Delete first occurrence of green ball, if it exists, else change the 
-- word to empty word

basketsNumb5_2 : Int                                                            
basketsNumb5_2 = fiveBaskets 

machine5_2 : Machine BallOfWool Kitten                                          
machine5_2 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_2 : UserTransTable BallOfWool Kitten                                
transTable5_2 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Yellow)                                                
      , value = { state = StableCell (LightGrey)                                 
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (White, Just Blue)                                              
      , value = { state = EmptyCell                                              
                , symb  = EmptyCell                            
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }       
    , { key   = (White, Just Green)                                                
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Nothing)                                  
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
    , { key   = (LightGrey, Just Yellow)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Yellow)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                 
    , { key   = (LightGrey, Just Blue)                                          
      , value = { state = EmptyCell                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (LightGrey, Just Green)                                            
      , value = { state = StableCell (Orange)                                     
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (LightGrey, Nothing)                                           
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (Grey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Blue)                                            
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (Grey, Just Green)                                           
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (Grey, Nothing)                                                
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    ] 

input5_2 : List (Maybe BallOfWool)                                              
input5_2 =                                                                      
  [Just Blue, Just Yellow, Just Green, Just Green, Nothing]                                                  
                                                                                
expectedResult5_2 : List (Maybe BallOfWool)                                     
expectedResult5_2 =                                                             
  [Nothing, Just Blue, Just Yellow, Just Green, Nothing]                                                 
                                                                                
expectedPos5_2 : Int                                                            
expectedPos5_2 = 3
                                                                                
usedCats5_2 : Array (Cell Kitten)                                               
usedCats5_2 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_3 - Delete all yellow balls from the word, if they exist, else dont change 
-- the word

basketsNumb5_3 : Int                                                            
basketsNumb5_3 = sevenBaskets                                                    
                                                                                
machine5_3 : Machine BallOfWool Kitten                                          
machine5_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_3 : UserTransTable BallOfWool Kitten                                
transTable5_3 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                              
      , value = { state = StableCell (LightGrey)                                            
                , symb  = EmptyCell                                          
                , dir   = StableCell (MoveRight)                               
                }                                                               
      , clickNum = 0                                                            
      }             
    , { key   = (White, Just Yellow)                                                 
      , value = { state = StableCell (White)                                
                , symb  = StableCell (Nothing)                                  
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
      , value = { state = EmptyCell                                     
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (LightGrey, Just Yellow)                                              
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                            
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (LightGrey, Nothing)                                          
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Grey, Just Red)                                             
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = EmptyCell                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                              
      , value = { state = EmptyCell 
                , symb  = StableCell (Nothing)                                 
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }   
    ]                                                                           
                                                                                
input5_3 : List (Maybe BallOfWool)                                              
input5_3 =                                                                      
  [Just Red, Just Yellow, Just Red, Just Yellow, Just Red, Nothing, Nothing]                                 
                                                                                
expectedResult5_3 : List (Maybe BallOfWool)                                     
expectedResult5_3 =                                                             
  [Nothing, Nothing, Nothing, Just Red, Just Red, Just Red, Nothing]                                 
                                                                                
expectedPos5_3 : Int                                                            
expectedPos5_3 = 4                                                         
                                                                                
usedCats5_3 : Array (Cell Kitten)                                               
usedCats5_3 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_3 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_3 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_4 - Delete each pair of identical balls

basketsNumb5_4 : Int                                                            
basketsNumb5_4 = nineBaskets                                                    
                                                                                
machine5_4 : Machine BallOfWool Kitten                                          
machine5_4 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                     
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_4 : UserTransTable BallOfWool Kitten                                
transTable5_4 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Nothing)                             
                , dir   = StableCell (MoveRight)                                             
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (White, Just Blue)                                              
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
    , { key   = (LightGrey, Just Yellow)                                              
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Nothing)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                                
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                                  
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (Grey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                    
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Blue)                                            
      , value = { state = EmptyCell                                     
                , symb  = EmptyCell                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                              
      , value = { state = EmptyCell                                   
                , symb  = StableCell (Just Blue)                                  
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
    , { key   = (Brown, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Blue)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Nothing)                                                   
      , value = { state = StableCell (White)                                   
                , symb  = StableCell (Nothing)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }    
    ]                                                                           
                                                                                
input5_4 : List (Maybe BallOfWool)                                              
input5_4 =                                                                      
  [Just Blue, Just Yellow, Just Blue, Just Blue, Just Blue, Just Yellow, 
   Nothing, Nothing, Nothing] 

expectedResult5_4 : List (Maybe BallOfWool)                                     
expectedResult5_4 =                                                             
  [Nothing, Nothing, Nothing, Just Blue, Just Yellow, Just Blue, Just Yellow, 
   Nothing, Nothing]                                   
                                                                                
expectedPos5_4 : Int                                                            
expectedPos5_4 = 7

usedCats5_4 : Array (Cell Kitten)                                               
usedCats5_4 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_4 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_4 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
-------------------------------------------------------------------------------

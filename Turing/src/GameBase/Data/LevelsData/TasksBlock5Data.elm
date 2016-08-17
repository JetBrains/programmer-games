module GameBase.Data.LevelsData.TasksBlock5Data exposing                                   
            (basketsNumb5_1, machine5_1, input5_1, transTable5_1,                               
             expectedResult5_1, expectedPos5_1, usedCats5_1, usedBalls5_1,
             basketsNumb5_2, machine5_2, input5_2, transTable5_2,                               
             expectedResult5_2, expectedPos5_2, usedCats5_2, usedBalls5_2,
             basketsNumb5_3, machine5_3, input5_3, transTable5_3,
             expectedResult5_3, expectedPos5_3, usedCats5_3, usedBalls5_3,
             basketsNumb5_4, machine5_4, input5_4, transTable5_4,               
             expectedResult5_4, expectedPos5_4, usedCats5_4, usedBalls5_4,
             basketsNumb5_5, machine5_5, input5_5, transTable5_5,               
             expectedResult5_5, expectedPos5_5, usedCats5_5, usedBalls5_5,
             basketsNumb5_6, machine5_6, input5_6, transTable5_6,               
             expectedResult5_6, expectedPos5_6, usedCats5_6, usedBalls5_6)
             {-basketsNumb5_7, machine5_7, input5_7, transTable5_7,               
             expectedResult5_7, expectedPos5_7, usedCats5_7, usedBalls5_7)
             basketsNumb5_8, machine5_8, input5_8, transTable5_8,               
             expectedResult5_8, expectedPos5_8, usedCats5_8, usedBalls5_8,    
             basketsNumb5_9, machine5_9, input5_9, transTable5_9,               
             expectedResult5_9, expectedPos5_9, usedCats5_9, usedBalls5_9)-}    

import TuringMachine.TuringTypes exposing (Direction(..), Machine,             
                                           UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing 
       (fourBaskets, fiveBaskets, sevenBaskets, eightBaskets, nineBaskets)

import Array exposing (Array, fromList)


-- BLOCK 5 : deletion and insertion of symbols (by compression and extension of
-- word)-----------------------------------------------------------------------


-------DELETION----------------------------------------------------------------

-- 5_1 - Delete the second ball, if it exists, else change word to empty

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
    [ { key = (White, Just Red)                                              
      , value = { state = EmptyCell 
                , symb  = EmptyCell                       
                , dir   = StableCell (MoveRight)                              
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (White, Just Yellow)                                                
      , value = { state = StableCell (Grey)                                      
                , symb  = StableCell (Nothing)                                   
                , dir   = StableCell (MoveRight)                                   
                }                                                                 
      , clickNum = 0                                                              
      }   
    , { key = (White, Just Green)                                              
      , value = { state = StableCell (Brown)                                             
                , symb  = StableCell (Nothing)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (White, Nothing)                                               
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (LightGrey, Just Red)                                          
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Just Yellow)                                              
      , value = { state = StableCell (Orange)                                     
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                           
    , { key = (LightGrey, Just Green)                                          
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
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
    , { key = (Grey, Just Red)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                        
    , { key = (Grey, Just Yellow)                                               
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }      
    , { key = (Grey, Just Green)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                        
    , { key = (Grey, Nothing)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                      
    , { key = (Brown, Just Red)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                   
    , { key = (Brown, Just Yellow)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Brown, Just Green)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Brown, Nothing)                                              
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
                        UserCell Brown]
                                                                                
usedBalls5_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_2 - Delete first occurrence of green ball, if it exists,
-- else change word to empty

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
    [ { key = (White, Just Yellow)                                                
      , value = { state = StableCell (LightGrey)                                 
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (White, Just Blue)                                              
      , value = { state = EmptyCell                                              
                , symb  = EmptyCell                            
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }       
    , { key = (White, Just Green)                                                
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                       
    , { key = (White, Nothing)                                               
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Just Yellow)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Yellow)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                 
    , { key = (LightGrey, Just Blue)                                          
      , value = { state = EmptyCell                                     
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (LightGrey, Just Green)                                            
      , value = { state = StableCell (Orange)                                     
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                
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
    , { key = (Grey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Blue)                                            
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Grey, Just Green)                                           
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (Grey, Nothing)                                                
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
                        UserCell Brown]                                         
                                                                                
usedBalls5_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_3 - Delete all yellow balls from word, if they exist, 
-- else dont change word

basketsNumb5_3 : Int                                                            
basketsNumb5_3 = sevenBaskets                                                    
                                                                                
machine5_3 : Machine BallOfWool Kitten                                          
machine5_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_3 : UserTransTable BallOfWool Kitten                                
transTable5_3 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                              
      , value = { state = StableCell (LightGrey)                                            
                , symb  = EmptyCell                                          
                , dir   = StableCell (MoveRight)                               
                }                                                               
      , clickNum = 0                                                            
      }             
    , { key = (White, Just Yellow)                                                 
      , value = { state = StableCell (White)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (White, Nothing)                                                 
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Just Red)                                          
      , value = { state = EmptyCell                                     
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Just Yellow)                                              
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                            
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (LightGrey, Nothing)                                          
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      } 

    , { key = (Grey, Just Red)                                             
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = EmptyCell                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Nothing)                                              
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
                        UserCell Brown]                                         
                                                                                
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
    [ { key = (White, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Nothing)                             
                , dir   = StableCell (MoveRight)                                             
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (White, Just Blue)                                              
      , value = { state = StableCell (Grey)                                            
                , symb  = StableCell (Nothing)                                            
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (White, Nothing)                                              
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (LightGrey, Just Yellow)                                              
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Nothing)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Just Blue)                                                
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Nothing)                                                  
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Grey, Just Yellow)                                          
      , value = { state = StableCell (LightGrey)                                    
                , symb  = StableCell (Just Blue)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Blue)                                            
      , value = { state = StableCell (Brown)                                     
                , symb  = StableCell (Nothing)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Nothing)                                              
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Blue)                                  
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Brown, Just Yellow)                                               
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Yellow)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Blue)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Nothing)                                                   
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
                        UserCell Brown]                                         
                                                                                
usedBalls5_4 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_4 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-------INSERTION---------------------------------------------------------------

-- 5_5 - If word is not empty, then put yellow ball after first ball, 
-- else dont change word 

basketsNumb5_5 : Int                                                            
basketsNumb5_5 = fiveBaskets

machine5_5 : Machine BallOfWool Kitten                                          
machine5_5 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 1
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_5 : UserTransTable BallOfWool Kitten                                
transTable5_5 = 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = StableCell (LightGrey)                                            
                , symb  = EmptyCell                                 
                , dir   = EmptyCell                               
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (White, Just Green)                                                 
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (LightGrey, Nothing)                                                  
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Red)                                  
                , dir   = EmptyCell                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Grey, Nothing)                                              
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Just Green)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }    
    , { key = (Brown, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                                 
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }    
    ]    

input5_5 : List (Maybe BallOfWool)                                              
input5_5 =                                                                      
  [Nothing, Just Red, Just Green]                                 
                                                                                
expectedResult5_5 : List (Maybe BallOfWool)                                     
expectedResult5_5 =                                                             
  [Just Red, Just Yellow, Just Green]                                 
                                                                                
expectedPos5_5 : Int                                                            
expectedPos5_5 = 1                                                              
                                                                                
usedCats5_5 : Array (Cell Kitten)                                               
usedCats5_5 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls5_5 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_5 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_6 - Insert/Past yellow ball after the first occurence of green ball, if it 
-- exists, else dont change word

basketsNumb5_6 : Int                                                            
basketsNumb5_6 = eightBaskets                                                    
                                                                                
machine5_6 : Machine BallOfWool Kitten                                          
machine5_6 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_6 : UserTransTable BallOfWool Kitten                                
transTable5_6 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = StableCell (White)                                             
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (White, Just Green)                                                 
      , value = { state = StableCell (LightGrey)                                          
                , symb  = StableCell (Just Yellow)                                 
                , dir   = StableCell (MoveLeft)                                
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
      , value = { state = StableCell (Orange)                                            
                , symb  = StableCell (Nothing)                                 
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (LightGrey, Just Red)                                                 
      , value = { state = StableCell (Grey)                                    
                , symb  = StableCell (Just Green)                                 
                , dir   = StableCell (MoveLeft)                                
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
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Green)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Grey, Just Red)                                             
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Red)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Green)                                           
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Red)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Blue)                                            
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Red)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Brown, Just Red)                                                  
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Blue)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Green)                                                
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Blue)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    ]

input5_6 : List (Maybe BallOfWool)                                              
input5_6 =                                                                      
  [Nothing, Just Red, Just Blue, Just Blue, Just Green, Just Red, Nothing]

expectedResult5_6 : List (Maybe BallOfWool)                                     
expectedResult5_6 =                                                             
  [Just Red, Just Blue, Just Blue, Just Green, Just Yellow, Just Red, Nothing]               
                                                                                
expectedPos5_6 : Int
expectedPos5_6 = 2
                                                                                
usedCats5_6 : Array (Cell Kitten)                                               
usedCats5_6 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls5_6 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_6 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- INSERTION AND DELETION------------------------------------------------------
{-
-- 5_7 - Расположить клубки по цветам радуги путем удаления и вставки (сортировка)

basketsNumb5_7 : Int                                                            
basketsNumb5_7 =
                                                                                
machine5_7 : Machine BallOfWool Kitten                                          
machine5_7 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_7 : UserTransTable BallOfWool Kitten                                
transTable5_7 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Blue)    
      , value = { state = EmptyCell                 
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (White, Nothing)                                                
      , value = { state = StableCell (Orange)                 
                , symb  = StableCell (Nothing)                               
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                   
    ]

input5_7 : List (Maybe BallOfWool)                                              
input5_7 =                                                                      
  [Just Blue, Just Green, Just Yellow, Just Red, Nothing]           
                                                                                
expectedResult5_7 : List (Maybe BallOfWool)                                     
expectedResult5_7 =                                                             
  [Just Red, Just Yellow, Just Green, Just Blue, Nothing]                                 
                                                                                
expectedPos5_7 : Int                                                            
expectedPos5_7 = 
                                                                                
usedCats5_7 : Array (Cell Kitten)                                               
usedCats5_7 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls5_7 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_7 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
-}

{-
-- 5_8 - Заменить в P каждое вхождение красного клубка на два зеленых 
-- (удалить красный, вставить два зеленых).
                                                                                
basketsNumb5_8 : Int                                                            
basketsNumb5_8 =                                                                
                                                                                
machine5_8 : Machine BallOfWool Kitten                                          
machine5_8 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_8 : UserTransTable BallOfWool Kitten                                
transTable5_8 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Blue)                                                
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input5_8 : List (Maybe BallOfWool)                                              
input5_8 =                                                                      
  []                                                                            
                                                                                
expectedResult5_8 : List (Maybe BallOfWool)                                     
expectedResult5_8 =                                                             
  []                                                                            
                                                                                
expectedPos5_8 : Int                                                            
expectedPos5_8 =                                                                
                                                                                
usedCats5_8 : Array (Cell Kitten)                                               
usedCats5_8 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls5_8 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_8 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
-}


{- 
-- 5_9 - Заменить в P каждую пару синих-желтых клубков на красный           
-- (удалить синий-желтый, вставить красный)                                       
                                                                                
basketsNumb5_9 : Int                                                            
basketsNumb5_9 =                                                                
                                                                                
machine5_9 : Machine BallOfWool Kitten                                          
machine5_9 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_9 : UserTransTable BallOfWool Kitten                                
transTable5_9 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Blue)                                                
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input5_9 : List (Maybe BallOfWool)                                              
input5_9 =                                                                      
  []                                                                            
                                                                                
expectedResult5_9 : List (Maybe BallOfWool)                                     
expectedResult5_9 =                                                             
  []                                                                            
                                                                                
expectedPos5_9 : Int                                                            
expectedPos5_9 =                                                                
                                                                                
usedCats5_9 : Array (Cell Kitten)                                               
usedCats5_9 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls5_9 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_9 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
-}
------------------------------------------------------------------------------- 

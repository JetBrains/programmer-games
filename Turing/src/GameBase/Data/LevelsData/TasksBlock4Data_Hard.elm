module GameBase.Data.LevelsData.TasksBlock4Data_Hard exposing                        
            (basketsNumb4_5, machine4_5, input4_5, transTable4_5,               
             expectedResult4_5, expectedPos4_5, usedCats4_5, usedBalls4_5,      
             basketsNumb4_6, machine4_6, input4_6, transTable4_6,               
             expectedResult4_6, expectedPos4_6, usedCats4_6, usedBalls4_6,      
             basketsNumb4_7, machine4_7, input4_7, transTable4_7,               
             expectedResult4_7, expectedPos4_7, usedCats4_7, usedBalls4_7)      
                                                                                
import TuringMachine.TuringTypes exposing (Direction(..), Machine,              
                                           UserTransTable, Cell(..))            
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (fourBaskets, sevenBaskets)            
                                                                                
import Array exposing (Array, fromList)


-- BLOCK 4 : analysis and comparison of symbols (by transition to different     
-- states)---------------------------------------------------------------------


-- 4_5 - If the first and the last balls are the same, then dont change the 
-- word, else change it to the empty word

basketsNumb4_5 : Int                                                            
basketsNumb4_5 = sevenBaskets                                                   
                                                                                
machine4_5 : Machine BallOfWool Kitten                                          
machine4_5 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_5 : UserTransTable BallOfWool Kitten                                
transTable4_5 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
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
                , symb  = StableCell (Just Red)                                 
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
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                               
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
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Red)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                                
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Red)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                              
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (Brown, Just Green)                                               
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Blue)                                                
      , value = { state = StableCell (Brown)                                    
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
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
                                                                                
input4_5 : List (Maybe BallOfWool)                                              
input4_5 =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green, Just Blue, Just Yellow, Nothing] 
                                                                                
expectedResult4_5 : List (Maybe BallOfWool)                                     
expectedResult4_5 =                                                             
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]               
                                                                                
expectedPos4_5 : Int                                                            
expectedPos4_5 = 1                                                              
                                                                                
usedCats4_5 : Array (Cell Kitten)                                               
usedCats4_5 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls4_5 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_5 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_6 - If input word contains even number of balls then dont change the word, 
-- else add the first ball to the right end
                                                                                
basketsNumb4_6 : Int                                                            
basketsNumb4_6 = fourBaskets                                                    
                                                                                
machine4_6 : Machine BallOfWool Kitten                                          
machine4_6 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_6 : UserTransTable BallOfWool Kitten                                
transTable4_6 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                 
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
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Red)                                 
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
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (LightGrey, Nothing)  -- odd number                                            
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Red)                                                  
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                                
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing) -- even number                                        
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input4_6 : List (Maybe BallOfWool)                                              
input4_6 =                                                                      
  [Just Red, Just Yellow, Just Green, Nothing]                                  
                                                                                
expectedResult4_6 : List (Maybe BallOfWool)                                     
expectedResult4_6 =                                                             
  [Just Red, Just Yellow, Just Green, Just Red]                                 
                                                                                
expectedPos4_6 : Int                                                            
expectedPos4_6 = 2                                                              
                                                                                
usedCats4_6 : Array (Cell Kitten)                                               
usedCats4_6 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls4_6 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_6 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_7 - Delete right half of the word
                                                                                
basketsNumb4_7 : Int                                                            
basketsNumb4_7 = sevenBaskets                                                   
                                                                                
machine4_7 : Machine BallOfWool Kitten                                          
machine4_7 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_7 : UserTransTable BallOfWool Kitten                                
transTable4_7 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Blue)                                                
      --change state to remember that it was Just Blue                          
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
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                             
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
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key   = (Grey, Just Yellow)                                               
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                              
      , value = { state = StableCell (Brown)                                    
                , symb  = EmptyCell                                             
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
    , { key   = (Brown, Just Green)                                               
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input4_7 : List (Maybe BallOfWool)                                              
input4_7 =                                                                      
  [Just Blue, Just Blue, Just Blue, Just Yellow, Just Yellow, Just Yellow,      
   Nothing]                                                                     
                                                                                
expectedResult4_7 : List (Maybe BallOfWool)                                     
expectedResult4_7 =                                                             
  [Just Blue, Just Blue, Just Blue, Nothing, Nothing, Nothing, Nothing]         
                                                                                
expectedPos4_7 : Int                                                            
expectedPos4_7 = 2                                                              
                                                                                
usedCats4_7 : Array (Cell Kitten)                                               
usedCats4_7 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls4_7 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_7 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
------------------------------------------------------------------------------- 

module GameBase.Data.LevelsData.TasksBlock5Data_Insertion exposing               
            (basketsNumb5_5, machine5_5, input5_5, transTable5_5,               
             expectedResult5_5, expectedPos5_5, usedCats5_5, usedBalls5_5,      
             basketsNumb5_6, machine5_6, input5_6, transTable5_6,               
             expectedResult5_6, expectedPos5_6, usedCats5_6, usedBalls5_6)
                                                                                
import TuringMachine.TuringTypes exposing (Direction(..), Machine,              
                                           UserTransTable, Cell(..))            
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (nineBaskets) 
--(fiveBaskets, eightBaskets)
                                                                                
import Array exposing (Array, fromList)                                         
                                                                                

-- BLOCK 5 : Deletion and insertion of the symbols (by compression and          
-- extension of the word)------------------------------------------------------


-------INSERTION---------------------------------------------------------------

-- 5_5 - If the word is not empty, then put yellow ball after first ball,
-- else dont change the word

basketsNumb5_5 : Int                                                            
basketsNumb5_5 = nineBaskets --fiveBaskets                                                    
                                                                                
machine5_5 : Machine BallOfWool Kitten                                          
machine5_5 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 3 --1                                                      
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_5 : UserTransTable BallOfWool Kitten                                
transTable5_5 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                             
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Green)                                               
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveLeft)                                 
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
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
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
expectedPos5_5 = 4 --2
                                                                                
usedCats5_5 : Array (Cell Kitten)                                               
usedCats5_5 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]                                         
                                                                                
usedBalls5_5 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_5 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 5_6 - Put yellow ball after the first occurence of green ball, if it      
-- exists, else dont change the word
                                                                                
basketsNumb5_6 : Int                                                            
basketsNumb5_6 = nineBaskets --eightBaskets                                                   
                                                                                
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
    [ { key   = (White, Just Red)                                                 
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Green)                                               
      , value = { state = EmptyCell                                
                , symb  = EmptyCell                              
                , dir   = EmptyCell                                 
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
    , { key   = (LightGrey, Just Red)                                             
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Green)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key   = (LightGrey, Just Green)                                           
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Red)                                                  
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                                
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Blue)                                                 
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Red)                                                 
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Brown, Just Green)                                               
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                                
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
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_6 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_6 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
-------------------------------------------------------------------------------

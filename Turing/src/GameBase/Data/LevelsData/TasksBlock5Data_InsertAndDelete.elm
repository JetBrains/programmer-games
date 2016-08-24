module GameBase.Data.LevelsData.TasksBlock5Data_InsertAndDelete exposing               
            (basketsNumb5_7, machine5_7, input5_7, transTable5_7,               
             expectedResult5_7, expectedPos5_7, usedCats5_7, usedBalls5_7,      
             basketsNumb5_8, machine5_8, input5_8, transTable5_8,               
             expectedResult5_8, expectedPos5_8, usedCats5_8, usedBalls5_8)      
                                                                                
import TuringMachine.TuringTypes exposing (Direction(..), Machine,              
                                           UserTransTable, Cell(..))            
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (sixBaskets, eightBaskets)
                                                                                
import Array exposing (Array, fromList)                                         
                                                                                
 
-- BLOCK 5 : Deletion and insertion of the symbols (by compression and          
-- extension of the word)------------------------------------------------------


-- INSERTION AND DELETION------------------------------------------------------

-- 5_7 - Replace each red ball by two green balls (delete red ball, insert two  
-- green balls)

basketsNumb5_7 : Int                                                            
basketsNumb5_7 = eightBaskets                                                   
                                                                                
machine5_7 : Machine BallOfWool Kitten                                          
machine5_7 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 3                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable5_7 : UserTransTable BallOfWool Kitten                                
transTable5_7 =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                                 
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                             
                , dir   = EmptyCell                                             
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
    , { key   = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                          
      , value = { state = StableCell (Grey)                                     
                , symb  = EmptyCell                                             
                , dir   = EmptyCell                                             
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
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
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
    , { key   = (Grey, Just Green)                                                
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input5_7 : List (Maybe BallOfWool)                                              
input5_7 =                                                                      
  [Nothing, Nothing, Nothing, Just Yellow, Just Red, Just Yellow, Just Red,     
   Nothing]                                                                     
                                                                                
expectedResult5_7 : List (Maybe BallOfWool)                                     
expectedResult5_7 =                                                             
  [Nothing, Just Yellow, Just Green, Just Green, Just Yellow, Just Green,       
   Just Green, Nothing]                                                         
                                                                                
expectedPos5_7 : Int                                                            
expectedPos5_7 = 6                                                              
                                                                                
usedCats5_7 : Array (Cell Kitten)                                               
usedCats5_7 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown, UserCell DarkBrown]                                         
                                                                                
usedBalls5_7 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_7 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing] 


-- 5_8 - Replace the first pair of yellow and blue balls by red ball (delete 
-- yellow-blue balls, insert red)
                                                                                
basketsNumb5_8 : Int                                                            
basketsNumb5_8 = sixBaskets                                                     
                                                                                
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
    [ { key   = (White, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (White, Just Blue)                                                
      , value = { state = StableCell (Grey)                                     
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
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }      
    , { key   = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                 
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
    , { key   = (Grey, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input5_8 : List (Maybe BallOfWool)                                              
input5_8 =                                                                      
  [Just Blue, Just Yellow, Just Yellow, Just Yellow, Just Blue, Nothing]        
                                                                                
expectedResult5_8 : List (Maybe BallOfWool)                                     
expectedResult5_8 =                                                             
  [Nothing, Just Blue, Just Yellow, Just Yellow, Just Red, Nothing]             
                                                                                
expectedPos5_8 : Int                                                            
expectedPos5_8 = 5                                                              
                                                                                
usedCats5_8 : Array (Cell Kitten)                                               
usedCats5_8 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls5_8 : Array (Cell (Maybe BallOfWool))                                  
usedBalls5_8 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
-------------------------------------------------------------------------------

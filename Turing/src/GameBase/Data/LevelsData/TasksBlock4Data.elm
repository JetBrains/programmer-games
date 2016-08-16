module GameBase.Data.LevelsData.TasksBlock4Data exposing                                   
            (basketsNumb4_1, machine4_1, input4_1, transTable4_1,                               
             expectedResult4_1, expectedPos4_1, usedCats4_1, usedBalls4_1,
             basketsNumb4_2, machine4_2, input4_2, transTable4_2,                               
             expectedResult4_2, expectedPos4_2, usedCats4_2, usedBalls4_2,
             basketsNumb4_3, machine4_3, input4_3, transTable4_3,
             expectedResult4_3, expectedPos4_3, usedCats4_3, usedBalls4_3,
             basketsNumb4_4, machine4_4, input4_4, transTable4_4,               
             expectedResult4_4, expectedPos4_4, usedCats4_4, usedBalls4_4,
             basketsNumb4_5, machine4_5, input4_5, transTable4_5,               
             expectedResult4_5, expectedPos4_5, usedCats4_5, usedBalls4_5,
             basketsNumb4_6, machine4_6, input4_6, transTable4_6,               
             expectedResult4_6, expectedPos4_6, usedCats4_6, usedBalls4_6, 
             basketsNumb4_7, machine4_7, input4_7, transTable4_7,               
             expectedResult4_7, expectedPos4_7, usedCats4_7, usedBalls4_7)

import TuringMachine.TuringTypes exposing (Direction(..), Machine,             
                                           UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing 
                            (fourBaskets, fiveBaskets, sevenBaskets)

import Array exposing (Array, fromList)


-- BLOCK 4 : analysis and comparison of symbols 
-- (by transition to different states)-----------------------------------------


-- 4_1 - Add same item, dont change empty word (_ 0 _ _ -> _ 0 0 _)
-- For Red, Yellow, Green first balls

basketsNumb4_1 : Int                                                            
basketsNumb4_1 = fourBaskets

machine4_1 : Machine BallOfWool Kitten                                          
machine4_1 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_1 : UserTransTable BallOfWool Kitten                                
transTable4_1 = 
  fromList  
    [ { key = (White, Just Red)                                              
      , value = { state = EmptyCell 
                , symb  = StableCell (Just Red)                       
                , dir   = StableCell (MoveRight)                              
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (White, Just Yellow)                                                
      , value = { state = EmptyCell                                      
                , symb  = StableCell (Just Yellow)                                   
                , dir   = StableCell (MoveRight)                                   
                }                                                                 
      , clickNum = 0                                                              
      }   
    , { key = (White, Just Green)                                                
      , value = { state = EmptyCell                                     
                , symb  = StableCell (Just Green)                                   
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
    , { key = (LightGrey, Nothing)                                                
      , value = { state = StableCell (Orange)                                     
                , symb  = StableCell (Just Red)                                   
                , dir   = StableCell (MoveLeft)                                   
                }                                                                 
      , clickNum = 0                                                              
      }   
    , { key = (Grey, Nothing)                                                
      , value = { state = StableCell (Orange)                                     
                , symb  = StableCell (Just Yellow)                                   
                , dir   = StableCell (MoveLeft)                                   
                }                                                                 
      , clickNum = 0                                                              
      }   
    , { key = (Brown, Nothing)                                              
      , value = { state = StableCell (Orange)                                  
                , symb  = StableCell (Just Green)                      
                , dir   = StableCell (MoveLeft)                             
                }                                                               
      , clickNum = 0  
      }                                                           
    ]      
                                                                                
input4_1 : List (Maybe BallOfWool)                                              
input4_1 =                                                                      
  [Just Red, Nothing, Nothing]                                                                     
                                                                                
expectedResult4_1 : List (Maybe BallOfWool)                                     
expectedResult4_1 =                                                             
  [Just Red, Just Red, Nothing]                                           
                                                                                
expectedPos4_1 : Int                                                            
expectedPos4_1 = 1

usedCats4_1 : Array (Cell Kitten)                                               
usedCats4_1 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown]
                                                                                
usedBalls4_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  


-- 4_2 - move the first (Yellow, Green, Blue) ball to the end
-- for empty inp word, one symb inp word, few symb inp word

basketsNumb4_2 : Int                                                            
basketsNumb4_2 = fiveBaskets

machine4_2 : Machine BallOfWool Kitten                                          
machine4_2 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_2 : UserTransTable BallOfWool Kitten                                
transTable4_2 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Yellow)                                              
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Just Green)                                               
      , value = { state = StableCell (Grey)                                             
                , symb  = StableCell (Nothing)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                       
    , { key = (White, Just Blue)                                               
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
    , { key = (LightGrey, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)                                   
                , symb  = StableCell (Just Yellow)                                 
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                   
    , { key = (LightGrey, Just Green)                                              
      , value = { state = StableCell (LightGrey)                                   
                , symb  = StableCell (Just Green)                                 
                , dir   = EmptyCell                                 
                }                                                               
      , clickNum = 0                                                            
      }                              
    , { key = (LightGrey, Just Blue)                                              
      , value = { state = StableCell (LightGrey)                                   
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                 
                }
      , clickNum = 0                                 
      }                             
    , { key = (LightGrey, Nothing)                                              
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Grey, Just Yellow)                                          
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Green)                                           
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Green)                               
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
    , { key = (Grey, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Brown, Just Yellow)                                          
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Green)                                           
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Blue)                                            
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                           
    , { key = (Brown, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Blue)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ] 

input4_2 : List (Maybe BallOfWool)                                              
input4_2 =                                                                      
  [Just Yellow, Just Green, Just Blue, Nothing]                                                  
                                                                                
expectedResult4_2 : List (Maybe BallOfWool)                                     
expectedResult4_2 =                                                             
  [Nothing, Just Green, Just Blue, Just Yellow]                                                 
                                                                                
expectedPos4_2 : Int                                                            
expectedPos4_2 = 2                                                              
                                                                                
usedCats4_2 : Array (Cell Kitten)                                               
usedCats4_2 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls4_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_3 - move the last (Yellow, Green) ball to the left end                   
-- for empty inp word, one symb inp word, few symb inp word                     
                                                                                
basketsNumb4_3 : Int                                                            
basketsNumb4_3 = fourBaskets                                                    
                                                                                
machine4_3 : Machine BallOfWool Kitten                                          
machine4_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 1                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_3 : UserTransTable BallOfWool Kitten                                
transTable4_3 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Yellow)                                              
      , value = { state = StableCell (White)                                             
                , symb  = StableCell (Just Yellow)                                          
                , dir   = EmptyCell                                
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
    , { key = (White, Nothing)                                                
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Nothing)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }   
    , { key = (LightGrey, Just Yellow)                                                
      , value = { state = StableCell (Grey)                                    
                , symb  = StableCell (Nothing)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Just Green)                                                
      , value = { state = EmptyCell                                    
                , symb  = EmptyCell
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Nothing)                                           
      , value = { state = StableCell (Orange)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Grey, Just Yellow)                                                
      , value = { state = StableCell (Grey)                                    
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Grey, Just Green)                                           
      , value = { state = StableCell (Grey)                                    
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Grey, Nothing)                                                
      , value = { state = StableCell (Orange)                                     
                , symb  = EmptyCell                               
                , dir   = StableCell (MoveRight)                                 
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
    , { key = (Brown, Just Green)                                                
      , value = { state = StableCell (Brown)                                     
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Nothing)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    ]                                                                           
                                                                                
input4_3 : List (Maybe BallOfWool)                                              
input4_3 =                                                                      
  [Nothing, Just Yellow, Just Green, Nothing]                                 
                                                                                
expectedResult4_3 : List (Maybe BallOfWool)                                     
expectedResult4_3 =                                                             
  [Just Green, Just Yellow, Nothing, Nothing]                                 
                                                                                
expectedPos4_3 : Int                                                            
expectedPos4_3 = 1                                                             
                                                                                
usedCats4_3 : Array (Cell Kitten)                                               
usedCats4_3 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls4_3 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_3 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_4 - Swap the first and the last symbols

basketsNumb4_4 : Int                                                            
basketsNumb4_4 = fiveBaskets                                                    
                                                                                
machine4_4 : Machine BallOfWool Kitten                                          
machine4_4 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 1                                                     
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable4_4 : UserTransTable BallOfWool Kitten                                
transTable4_4 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Yellow)                                              
      , value = { state = EmptyCell
                , symb  = EmptyCell                              
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
    , { key = (LightGrey, Just Green)                                          
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }       
    , { key = (LightGrey, Just Blue)                                           
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }        
    , { key = (LightGrey, Nothing)                                           
      , value = { state = EmptyCell                                
                , symb  = StableCell (Nothing)                               
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Grey, Just Blue)                                              
      , value = { state = EmptyCell                                     
                , symb  = EmptyCell                                  
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
    , { key = (Brown, Just Green)                                              
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Green)                              
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
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                 
    ]                                                                           
                                                                                
input4_4 : List (Maybe BallOfWool)                                              
input4_4 =                                                                      
  [Nothing, Just Yellow, Just Green, Just Blue, Nothing]                                   
                                                                                
expectedResult4_4 : List (Maybe BallOfWool)                                     
expectedResult4_4 =                                                             
  [Nothing, Just Blue, Just Green, Just Yellow, Nothing]                                   
                                                                                
expectedPos4_4 : Int                                                            
expectedPos4_4 = 2  

usedCats4_4 : Array (Cell Kitten)                                               
usedCats4_4 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls4_4 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_4 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_5 - If the first and the last balls are the same, then dont change word, 
-- else change it to empty word

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
    [ { key = (White, Just Red)                                                 
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                 
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
      , value = { state = StableCell (LightGrey)                                     
                , symb  = StableCell (Just Red)                                  
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
    , { key = (LightGrey, Just Green)                                              
      , value = { state = StableCell (LightGrey)                                     
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                             
    , { key = (LightGrey, Just Blue)                                              
      , value = { state = StableCell (LightGrey)                                     
                , symb  = StableCell (Just Blue)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                              
    , { key = (LightGrey, Nothing)                                              
      , value = { state = EmptyCell                                   
                , symb  = StableCell (Nothing)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Red)                                                   
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Red)                              
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }    
    , { key = (Grey, Just Yellow)                                                  
      , value = { state = EmptyCell                                   
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Grey, Just Green)                                                  
      , value = { state = StableCell (Brown)                                   
                , symb  = StableCell (Nothing)                                 
                , dir   = StableCell (MoveLeft)                                 
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
    , { key = (Brown, Just Red)                                                
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }           
    , { key = (Brown, Just Yellow)                                               
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Brown, Just Green)                                                
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }
    , { key = (Brown, Just Blue)                                               
      , value = { state = StableCell (Brown)                                    
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveLeft)                                 
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
                        UserCell Brown]                                         
                                                                                
usedBalls4_5 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_5 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_6 - If inp word contains even number of balls then dont change it, 
-- else add the first ball to the right end.

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
    [ { key = (White, Just Red)                                                 
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                 
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
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (LightGrey, Just Yellow)                                               
      , value = { state = EmptyCell                                
                , symb  = StableCell (Just Yellow)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (LightGrey, Just Green)                                                 
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Green)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                      
    , { key = (LightGrey, Nothing)  -- odd number                                            
      , value = { state = StableCell (Orange)                                             
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key = (Grey, Just Red)                                          
      , value = { state = StableCell (LightGrey)                                    
                , symb  = StableCell (Just Red)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (Grey, Just Yellow)                                              
      , value = { state = StableCell (LightGrey)                                             
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                 
    , { key = (Grey, Just Green)                                          
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Green)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                      
    , { key = (Grey, Nothing) -- even number                                        
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
                        UserCell Brown]                                         
                                                                                
usedBalls4_6 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_6 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 4_7 - Delete right half of word

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
    [ { key = (White, Just Blue)    
      --change state to remember that it was Just Blue
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
    , { key = (LightGrey, Just Yellow)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = EmptyCell                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                   
    , { key = (LightGrey, Just Blue)                                            
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }  
    , { key = (LightGrey, Nothing)                                            
      , value = { state = StableCell (Grey)                               
                , symb  = StableCell (Nothing)                                
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }    
    , { key = (Grey, Just Yellow)                                              
      , value = { state = EmptyCell                                     
                , symb  = EmptyCell                                  
                , dir   = StableCell (MoveLeft)                                 
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
    , { key = (Brown, Just Yellow)                                               
      , value = { state = StableCell (Brown)                                    
                , symb  = EmptyCell                                  
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
    , { key = (Brown, Just Green)                                              
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
                        UserCell Brown]                                         
                                                                                
usedBalls4_7 : Array (Cell (Maybe BallOfWool))                                  
usedBalls4_7 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
------------------------------------------------------------------------------- 

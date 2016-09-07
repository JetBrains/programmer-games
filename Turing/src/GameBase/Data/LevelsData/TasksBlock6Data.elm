module GameBase.Data.LevelsData.TasksBlock6Data exposing                        
            (basketsNumb6_1, machine6_1, input6_1, transTable6_1,                               
             expectedResult6_1, expectedPos6_1, usedCats6_1, usedBalls6_1,
             basketsNumb6_2, machine6_2, input6_2, transTable6_2,                               
             expectedResult6_2, expectedPos6_2, usedCats6_2, usedBalls6_2,
             basketsNumb6_3, machine6_3, input6_3, transTable6_3,                               
             expectedResult6_3, expectedPos6_3, usedCats6_3, usedBalls6_3,
             basketsNumb6_4, machine6_4, input6_4, transTable6_4,               
             expectedResult6_4, expectedPos6_4, usedCats6_4, usedBalls6_4)
import TuringMachine.TuringTypes exposing (Direction(..), Machine,             
                                           UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (nineBaskets)
--(sevenBaskets, eightBaskets, nineBaskets)

import Array exposing (Array, fromList)


-- BLOCK 6 : Get result on new place and fixing place on the tape--------------


-- 6_1 - Delete yellow balls from the word, if they exist, else dont change the 
-- word

basketsNumb6_1 : Int                                                            
basketsNumb6_1 = nineBaskets --eightBaskets

machine6_1 : Machine BallOfWool Kitten                                          
machine6_1 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1 --0
  , initHeadPosForMach = 1
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             

transTable6_1 : UserTransTable BallOfWool Kitten                               
transTable6_1 =                                                                
  fromList                                                                      
    [ { key   = (White, Just Red)
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Red)
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
    , { key   = (White, Nothing)
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveLeft)
                } 
      , clickNum = 0 
      }
    , { key   = (LightGrey, Just Red)
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Red)  
                , dir   = StableCell (MoveLeft) 
                } 
      , clickNum = 0  
      }   
    , { key   = (LightGrey, Just Yellow) 
      , value = { state = StableCell (LightGrey) 
                , symb  = StableCell (Just Yellow) 
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
    , { key   = (LightGrey, Nothing)
      , value = { state = StableCell (Grey)
                , symb  = StableCell (Nothing) 
                , dir   = StableCell (MoveRight) 
                }
      , clickNum = 0 
      }
    , { key   = (Grey, Just Red)                                             
      , value = { state = StableCell (Brown)                                
                , symb  = EmptyCell                                 
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Yellow)                                          
      , value = { state = EmptyCell                                
                , symb  = EmptyCell                              
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Just Green)                                           
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Nothing)                               
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Brown, Just Red)
      , value = { state = StableCell (Brown)
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveRight) 
                }  
      , clickNum = 0
      }  
    , { key   = (Brown, Just Yellow) 
      , value = { state = StableCell (Brown) 
                , symb  = StableCell (Just Yellow) 
                , dir   = StableCell (MoveRight)
                } 
      , clickNum = 0 
      } 
    , { key   = (Brown, Just Green) 
      , value = { state = StableCell (Brown) 
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveRight) 
                } 
      , clickNum = 0 
      }
    , { key   = (Brown, Nothing)   
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Just Red) 
                , dir   = StableCell (MoveLeft)
                }
      , clickNum = 0
      }
    ] 

input6_1 : List (Maybe BallOfWool)                                              
input6_1 =                                                                      
  [Nothing, Just Yellow, Just Yellow, Just Red, Just Yellow, Nothing, Nothing, 
   Nothing]    
                                                                                    
expectedResult6_1 : List (Maybe BallOfWool)                                     
expectedResult6_1 =                                                             
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just Red, Nothing]
                                                                                
expectedPos6_1 : Int                                                            
expectedPos6_1 = 7 --6
                                                                                
usedCats6_1 : Array (Cell Kitten)                                               
usedCats6_1 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]

usedBalls6_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls6_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 6_2 - Arrange balls in colors of the rainbow by creation new word near input 
-- word. Provide proccessing of the case where there isnt red or yellow ball in 
-- input word

basketsNumb6_2 : Int                                                            
basketsNumb6_2 = nineBaskets --sevenBaskets

machine6_2 : Machine BallOfWool Kitten                                          
machine6_2 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1 --0
  , initHeadPosForMach = 1
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable6_2 : UserTransTable BallOfWool Kitten                                
transTable6_2 =                                                                 
  fromList 
    [ { key   = (White, Just Red)                                                  
      , value = { state = EmptyCell                                            
                , symb  = StableCell (Just Red)
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
    , { key   = (White, Just Blue)                                                
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                
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
    , { key   = (LightGrey, Just Blue)                                              
      , value = { state = StableCell (LightGrey)                                    
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (LightGrey, Nothing)                                                
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Red)                                  
                , dir   = StableCell (MoveLeft)                                 
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
    , { key   = (Grey, Just Yellow)                                        
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }    
    , { key   = (Grey, Just Blue)                                          
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                
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
    , { key   = (Brown, Just Red)                                               
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = { state = StableCell (DarkBrown)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Blue)                                                
      , value = { state = StableCell (Orange)                                     
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      }    
    , { key   = (DarkBrown, Just Red)                                           
      , value = { state = StableCell (DarkBrown)                                
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (DarkBrown, Just Yellow)                                        
      , value = { state = StableCell (DarkBrown)                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (DarkBrown, Just Blue)                                          
      , value = { state = StableCell (DarkBrown)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (DarkBrown, Nothing)                                            
      , value = { state = StableCell (Orange)                                     
                , symb  = StableCell (Just Yellow)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }     
    ]
                                                                                
input6_2 : List (Maybe BallOfWool)                                              
input6_2 =                                                                      
  [Nothing, Just Yellow, Just Red, Just Blue, Nothing, Nothing, Nothing] 
                                                                                
expectedResult6_2 : List (Maybe BallOfWool)                                     
expectedResult6_2 =                                                             
  [Nothing, Just Yellow, Just Red, Just Blue, Just Red, Just Yellow, Nothing] 

expectedPos6_2 : Int 
expectedPos6_2 = 5 --4
                                                                                
usedCats6_2 : Array (Cell Kitten)                                               
usedCats6_2 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls6_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls6_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 6_3 - Turn the word (abb -> bba)

basketsNumb6_3 : Int                                                            
basketsNumb6_3 = nineBaskets --eightBaskets       

machine6_3 : Machine BallOfWool Kitten                                          
machine6_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0
  , initHeadPosForMach = 3
  , startState  = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable6_3 : UserTransTable BallOfWool Kitten                                
transTable6_3 =                                                                 
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
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }      
    , { key   = (White, Just Green)                                                 
      , value = { state = EmptyCell                                
                , symb  = StableCell (Just Yellow)                              
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (White, Nothing)                                               
      , value = { state = EmptyCell                                     
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
    , { key   = (LightGrey, Nothing)                                           
      , value = { state = StableCell (Brown)                                
                , symb  = StableCell (Just Red)                               
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Grey, Just Red)                                             
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Red)                                 
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
    , { key   = (Grey, Just Green)                                           
      , value = { state = StableCell (Grey)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Grey, Nothing)                                              
      , value = { state = StableCell (Brown)                                    
                , symb  = StableCell (Just Green)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (Brown, Just Red)                                                  
      , value = { state = StableCell (Brown)                                     
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Green)                                                
      , value = { state = StableCell (Brown)                                     
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveLeft)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key   = (Brown, Just Yellow)                                                   
      , value = { state = EmptyCell                                    
                , symb  = StableCell (Just Yellow)                               
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (DarkBrown, Just Red)                                          
      , value = { state = StableCell (Orange)                                
                , symb  = StableCell (Just Red)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }
    , { key   = (DarkBrown, Just Yellow)                                              
      , value = { state = StableCell (DarkBrown)                                    
                , symb  = StableCell (Nothing)                              
                , dir   = StableCell (MoveRight)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (DarkBrown, Just Green)                                             
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Just Green)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      } 
    ]
                                                                                
input6_3 : List (Maybe BallOfWool)                                              
input6_3 =                                                                      
  [Nothing, Just Red, Just Green, Just Green, Nothing, Nothing, Nothing, 
   Nothing] 
                                                                                
expectedResult6_3 : List (Maybe BallOfWool)                                     
expectedResult6_3 =  
  [Nothing, Nothing, Nothing, Nothing, Just Green, Just Green, Just Red, 
   Nothing] 

expectedPos6_3 : Int 
expectedPos6_3 = 5
                                                                                
usedCats6_3 : Array (Cell Kitten)                                               
usedCats6_3 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls6_3 : Array (Cell (Maybe BallOfWool))                                  
usedBalls6_3 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]


-- 6_4 - Double the word, put blue ball between the word and its copy

basketsNumb6_4 : Int
basketsNumb6_4 = nineBaskets

machine6_4 : Machine BallOfWool Kitten
machine6_4 =
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))
  , initHeadPosForDraw = 0
  , initHeadPosForMach = 1
  , startState  = White 
  , acceptState = Orange 
  , rejectState = Violet 
  } 

transTable6_4 : UserTransTable BallOfWool Kitten
transTable6_4 =
  fromList
    [ { key   = (White, Just Green) 
      , value = { state = StableCell (White)
                , symb  = StableCell (Just Green)
                , dir   = StableCell (MoveRight)
                } 
      , clickNum = 0 
      }
    , { key   = (White, Nothing)
      , value = { state = StableCell (LightGrey)
                , symb  = StableCell (Just Blue) 
                , dir   = StableCell (MoveLeft)
                } 
      , clickNum = 0 
      }
    , { key   = (LightGrey, Just Red)                                             
      , value = { state = EmptyCell                                     
                , symb  = EmptyCell                               
                , dir   = StableCell (MoveRight)                                
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
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Blue)                                
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      } 
    , { key   = (LightGrey, Nothing) 
      , value = { state = StableCell (Grey) 
                , symb  = StableCell (Nothing)
                , dir   = StableCell (MoveRight) 
                }
      , clickNum = 0
      }
    , { key   = (Grey, Just Green)
      , value = { state = EmptyCell
                , symb  = StableCell (Just Red)
                , dir   = StableCell (MoveRight) 
                }
      , clickNum = 0 
      }
    , { key   = (Grey, Just Blue) 
      , value = { state = StableCell (Orange)
                , symb  = StableCell (Just Blue)
                , dir   = StableCell (MoveRight)
                }
      , clickNum = 0 
      } 
    , { key   = (Brown, Just Green) 
      , value = { state = StableCell (Brown) 
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveRight)
                } 
      , clickNum = 0 
      }  
    , { key   = (Brown, Just Blue) 
      , value = { state = StableCell (Brown) 
                , symb  = StableCell (Just Blue)
                , dir   = StableCell (MoveRight)
                } 
      , clickNum = 0
      }
    , { key   = (Brown, Nothing)
      , value = { state = EmptyCell
                , symb  = StableCell (Just Green) 
                , dir   = StableCell (MoveLeft)
                } 
      , clickNum = 0
      }
    ]

input6_4 : List (Maybe BallOfWool)                                              
input6_4 =                                                                      
  [Nothing, Just Green, Just Green, Just Green, Nothing, Nothing, Nothing, 
   Nothing, Nothing]
                                                                                
expectedResult6_4 : List (Maybe BallOfWool)                                     
expectedResult6_4 = 
  [Nothing, Just Green, Just Green, Just Green, Just Blue, Just Green, 
   Just Green, Just Green, Nothing]  

expectedPos6_4 : Int
expectedPos6_4 = 5
                                                                                
usedCats6_4 : Array (Cell Kitten)
usedCats6_4 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,
                        UserCell Brown, UserCell DarkBrown]
                                                                                
usedBalls6_4 : Array (Cell (Maybe BallOfWool))
usedBalls6_4 = fromList [UserCell (Just Red), UserCell (Just Yellow),
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]
-------------------------------------------------------------------------------

module GameBase.Data.LevelsData.TasksBlock1Data exposing                                     
            (basketsNumb1_1, machine1_1, input1_1, transTable1_1, 
             expectedResult1_1, expectedPos1_1, usedCats1_1, usedBalls1_1, 
             basketsNumb1_2, machine1_2, input1_2, transTable1_2, 
             expectedResult1_2, expectedPos1_2, usedCats1_2, usedBalls1_2, 
             basketsNumb1_3, machine1_3, input1_3, transTable1_3, 
             expectedResult1_3, expectedPos1_3, usedCats1_3, usedBalls1_3)                
                                                                                
import TuringMachine.TuringTypes exposing ( Direction(..), Machine,             
                                            UserTransTable, Cell(..))           
import TuringMachine.RunTuring exposing (transFunc)                             
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))            
import GameBase.UI.MainObjects.Basket exposing (fourBaskets, sevenBaskets) 

import Array exposing (Array, fromList) 


--BLOCK 1 : Head movement and addition balls at the end of input word----------


-- 1_1 - put blue ball at the right end of word                                 

basketsNumb1_1 : Int            
basketsNumb1_1 = fourBaskets 

machine1_1 : Machine BallOfWool Kitten                                          
machine1_1 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 0                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable1_1 : UserTransTable BallOfWool Kitten                                
transTable1_1 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]   

input1_1 : List (Maybe BallOfWool)                                              
input1_1 =                                                                      
  [Just Red]                                                                    
                                                                                
expectedResult1_1 : List (Maybe BallOfWool)                                     
expectedResult1_1 =                                                             
  [Just Red, Just Blue]                                                         
                                                                                
expectedPos1_1 : Int                                                            
expectedPos1_1 = 1
                                                                                
usedCats1_1 : Array (Cell Kitten)                                               
usedCats1_1 = fromList [UserCell White, UserCell LightGrey, UserCell Grey, 
                        UserCell Brown]      
                                                                                
usedBalls1_1 : Array (Cell (Maybe BallOfWool))                                  
usedBalls1_1 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                 UserCell (Just Green), UserCell (Just Blue), UserCell Nothing] 


-- 1_2 - put blue ball at the left end of word

basketsNumb1_2 : Int                                                            
basketsNumb1_2 = fourBaskets 

machine1_2 : Machine BallOfWool Kitten                                          
machine1_2 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1 
  , initHeadPosForMach = 1                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable1_2 : UserTransTable BallOfWool Kitten                                
transTable1_2 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                 
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = StableCell (Orange)                                   
                , symb  = EmptyCell                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]                                                                           
                                                                                
input1_2 : List (Maybe BallOfWool)                                              
input1_2 =                                                                      
  [Nothing, Just Red]                                                           
                                                                               
expectedResult1_2 : List (Maybe BallOfWool)                                     
expectedResult1_2 =                                                             
  [Just Blue, Just Red]                                                         
                                                                                
expectedPos1_2 : Int                                                            
expectedPos1_2 = 2
                                                                                
usedCats1_2 : Array (Cell Kitten)                                               
usedCats1_2 = fromList [UserCell White, UserCell LightGrey, UserCell Grey, 
                        UserCell Brown]
                                                                                
usedBalls1_2 : Array (Cell (Maybe BallOfWool))                                  
usedBalls1_2 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  


-- 1_3 - put blue balls at the both ends of word                                

basketsNumb1_3 : Int                                                            
basketsNumb1_3 = sevenBaskets

machine1_3 : Machine BallOfWool Kitten                                          
machine1_3 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1
  , initHeadPosForMach = 1                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable1_3 : UserTransTable BallOfWool Kitten                                
transTable1_3 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Red)                                 
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Just Yellow)                                              
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Yellow)                              
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Just Green)                                               
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Nothing)                                                  
      , value = { state = EmptyCell                                             
                , symb  = EmptyCell                                             
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Just Red)                                             
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Red)                                 
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }       
    , { key = (LightGrey, Just Yellow)                                          
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Yellow)                              
                , dir   = EmptyCell                                             
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Just Green)                                           
      , value = { state = EmptyCell                                             
                , symb  = StableCell (Just Green)                               
                , dir   = EmptyCell                                             
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

input1_3 : List (Maybe BallOfWool)                                              
input1_3 =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green]                                  
                                                                                
expectedResult1_3 : List (Maybe BallOfWool)                                     
expectedResult1_3 =                                                             
  [Just Blue, Just Red, Just Yellow, Just Green, Just Blue]                     
                                                                                
expectedPos1_3 : Int                                                            
expectedPos1_3 = 2                                                              
                                                                                
usedCats1_3 : Array (Cell Kitten)                                               
usedCats1_3 = fromList [UserCell White, UserCell LightGrey, UserCell Grey, 
                        UserCell Brown]
                                                                                
usedBalls1_3 : Array (Cell (Maybe BallOfWool))                                  
usedBalls1_3 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  
------------------------------------------------------------------------------- 

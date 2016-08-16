 3_10 - Replace every red ball by two green balls                               
                                                                                
basketsNumb3_10 : Int                                                            
basketsNumb3_10 = nineBaskets                                                   
                                                                                
machine3_10 : Machine BallOfWool Kitten                                          
machine3_10 =                                                                    
  { transition = (transFunc (fromList []) (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 1                                                      
  , startState = White                                                          
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }                                                                             
                                                                                
transTable3_10 : UserTransTable BallOfWool Kitten                                
transTable3_10 =                                                                 
  fromList                                                                      
    [ { key = (White, Just Red)                                                 
      , value = { state = StableCell (LightGrey)                                   
                , symb  = StableCell (Just Green)                                             
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (White, Just Yellow)                                                  
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Yellow)                                  
                , dir   = StableCell (MoveRight)                                
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
      , value = { state = StableCell (Orange)                                   
                , symb  = StableCell (Nothing)                                  
                , dir   = StableCell (MoveLeft)                                 
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Just Red)                                             
      , value = { state = StableCell (Grey)                                     
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                  
    , { key = (LightGrey, Just Yellow)                                                  
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                                  
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Just Green)                                           
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (LightGrey, Nothing)                                              
      , value = { state = StableCell (White)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Yellow)                                               
      , value = { state = StableCell (LightGrey)                                    
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Just Green)                                                
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    , { key = (Grey, Nothing)                                                   
      , value = { state = StableCell (LightGrey)                                
                , symb  = StableCell (Just Green)                               
                , dir   = StableCell (MoveRight)                                
                }                                                               
      , clickNum = 0                                                            
      }                                                                         
    ]    

input3_10 : List (Maybe BallOfWool)                                              
input3_10 =                                                                      
  [Nothing, Just Red, Just Yellow, Just Red, Just Red]                          
                                                                                
expectedResult3_10 : List (Maybe BallOfWool)                                     
expectedResult3_10 =                                                             
  [Nothing, Just Green, Just Green, Just Yellow, Just Green, Just Green,        
   Just Green, Just Green, Nothing]                                             
                                                                                
expectedPos3_10 : Int                                                            
expectedPos3_10 = 7                                                              
                                                                                
usedCats3_10 : Array (Cell Kitten)                                               
usedCats3_10 = fromList [UserCell White, UserCell LightGrey, UserCell Grey,      
                        UserCell Brown]                                         
                                                                                
usedBalls3_10 : Array (Cell (Maybe BallOfWool))                                  
usedBalls3_10 = fromList [UserCell (Just Red), UserCell (Just Yellow),           
                UserCell (Just Green), UserCell (Just Blue), UserCell Nothing]  


-- 5_7 - Replace each red ball by two green balls (delete red ball, insert two  
-- green balls) 

module TasksBlock5Tests.TwoGreenInsteadRed exposing (tests)

import TuringMachine.TuringTypes exposing
        (Machine, MachineCfg, TapeCfg, Direction(..), TransTable)
import TuringMachine.RunTuring   exposing (runMachine, transFunc)
import TuringMachine.InitUpdate  exposing (initMachineCfg)

import Array   exposing (fromList, empty)
import ElmTest exposing (Test, suite, test, assertEqual)                                                    
import List    exposing (head, tail, reverse, length, drop, take)


--TEST DATA--------------------------------------------------------------------
type BallOfWool = Red | Yellow | Green | Blue                                   
type Kitten = White | LightGrey | Grey | Brown | DarkBrown | Orange | Violet


machine : Machine BallOfWool Kitten                                          
machine =                                                                    
  { transition = (transFunc transTable (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 3                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }  


transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList  
    [ { key   = (White, Just Red)                                               
      , value = (LightGrey, Just Green, MoveLeft)
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Grey, Just Green, MoveLeft)
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveLeft)      
      } 
    , { key   = (LightGrey, Nothing)                                            
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveLeft)                                 
      }      
    , { key   = (Grey, Just Green)                                              
      , value = (LightGrey, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input = 
  [Nothing, Nothing, Nothing, Just Yellow, Just Red, Just Yellow, Just Red,     
   Nothing]
-------------------------------------------------------------------------------


--TEST FUNCTIONS---------------------------------------------------------------  
runMach : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Int -> 
          List (MachineCfg BallOfWool Kitten)    
runMach m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)
  in                                                                            
    (runMachine m init [init])     
-----------------------

fstTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten)
fstTestedCfg m inp hpos =                                                         
  (runMach m inp hpos)
  |> head                                                    


fstCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
fstCorrectMaybeCfg = Just fstCorrectCfg 


fstCorrectCfg : MachineCfg BallOfWool Kitten         
fstCorrectCfg =                                                                
  { currState = White
  , currDir   = Stay
  , tapeCfg   = 
      { leftSyms  = fromList [Nothing, Nothing, Nothing]                                                        
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Red, Just Yellow, Just Red, Nothing]                                     
      }  
  }                                                                             
-----------------------

thirdTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
                 Int -> Maybe (MachineCfg BallOfWool Kitten)
thirdTestedCfg m inp hpos =  
  (runMach m inp hpos)
  |> drop 2
  |> head                                                      
                                     

thirdCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
thirdCorrectMaybeCfg = Just thirdCorrectCfg                                     


thirdCorrectCfg : MachineCfg BallOfWool Kitten         
thirdCorrectCfg =                                                                
  { currState = LightGrey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Nothing, Nothing]
      , currSym   = Just Yellow                                                                
      , rightSyms = fromList [Just Green, Just Yellow, Just Red, Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
fourthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fourthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 3                                                                     
  |> head                                                                       
                                                                                
                                                                                
fourthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
fourthCorrectMaybeCfg = Just fourthCorrectCfg                                     
                                                                                
                                                                                
fourthCorrectCfg : MachineCfg BallOfWool Kitten                                  
fourthCorrectCfg =                                                               
  { currState = Grey                                                       
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing]                        
      , currSym   = Nothing                                                 
      , rightSyms = fromList [Just Green, Just Green, Just Yellow, Just Red, 
                              Nothing]       
      }                                                                         
  }
-----------------------                                                         
                                                                                
fifthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fifthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> drop 4                                                                     
  |> head                                                                       
                                                                                
                                                                                
fifthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
fifthCorrectMaybeCfg = Just fifthCorrectCfg                                   
                                                                                
                                                                                
fifthCorrectCfg : MachineCfg BallOfWool Kitten                                 
fifthCorrectCfg =                                                              
  { currState = White                                                            
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Just Yellow]                                 
      , currSym   = Just Green                     
      , rightSyms = fromList [Just Green, Just Yellow, Just Red, Nothing]                                          
      }                                                                         
  }
-----------------------                                                         
                                                                                
twentiethTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                     Int -> Maybe (MachineCfg BallOfWool Kitten)                    
twentiethTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
twentiethCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
twentiethCorrectMaybeCfg = Just twentiethCorrectCfg     
                                                                                
                                                                                
twentiethCorrectCfg : MachineCfg BallOfWool Kitten                                  
twentiethCorrectCfg =                                                               
  { currState = Orange
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Yellow, Just Green, Just Green, 
                              Just Yellow, Just Green]
      , currSym   = Just Green                                                  
      , rightSyms = fromList [Nothing]       
      }                                                                         
  }
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "third cfg"                                                                    
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach) 
                     thirdCorrectMaybeCfg 
    , test "fourth cfg"                                                          
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)  
                     fourthCorrectMaybeCfg  
    , test "fifth cfg"                                                         
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg   
    , test "last cfg"                                                          
      <| assertEqual (twentiethTestedCfg machine input 
                                         machine.initHeadPosForMach)  
                     twentiethCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                    20 
    ]
-------------------------------------------------------------------------------

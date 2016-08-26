-- 5_6 - Put yellow ball after the first occurence of green ball, if it         
-- exists, else dont change the word 

module TasksBlock5Tests.YellowAfterFirstGreen exposing (tests)

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
  , initHeadPosForDraw = 1                                                      
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }


transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList
    [ { key   = (White, Just Red)                                               
      , value = (White, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (LightGrey, Just Yellow, MoveLeft)                                             
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (White, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (Grey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (Brown, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Just Green, MoveRight)                                
      }                                 
    , { key   = (Grey, Just Red)                                                
      , value = (Grey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (LightGrey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Brown, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Red)                                               
      , value = (Grey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (LightGrey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (Orange, Just Blue, MoveRight)                                
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input = 
  [Nothing, Just Red, Just Blue, Just Blue, Just Green, Just Red, Nothing]  
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
      { leftSyms  = fromList [Nothing]                                                           
      , currSym   = Just Red
      , rightSyms = fromList [Just Blue, Just Blue, Just Green, Just Red, 
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
  { currState = LightGrey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Just Red, Just Blue]
      , currSym   = Just Blue                                                               
      , rightSyms = fromList [Just Yellow, Just Red, Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
sixthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
sixthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 5                                                                     
  |> head                                                                       
                                                                                
                                                                                
sixthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
sixthCorrectMaybeCfg = Just sixthCorrectCfg                                     
                                                                                
                                                                                
sixthCorrectCfg : MachineCfg BallOfWool Kitten                                  
sixthCorrectCfg =                                                               
  { currState = Brown                                                       
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red]                     
      , currSym   = Just Blue                                                   
      , rightSyms = fromList [Just Green, Just Yellow, Just Red, Nothing]                   
      }                                                                         
  } 
-----------------------                                                         
                                                                                
eighthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                    
eighthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 7                                                                     
  |> head                                                                       
                                                                                
                                                                                
eighthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
eighthCorrectMaybeCfg = Just eighthCorrectCfg                                     
                                                                                
                                                                                
eighthCorrectCfg : MachineCfg BallOfWool Kitten                                  
eighthCorrectCfg =                                                               
  { currState = Grey                                                           
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = empty                                
      , currSym   = Nothing                                                   
      , rightSyms = fromList [Just Blue, Just Blue, Just Green, Just Yellow, 
                              Just Red, Nothing]       
      }                                                                         
  }    
-----------------------                                                         
                                                                                
ninthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                   
ninthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
ninthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
ninthCorrectMaybeCfg = Just ninthCorrectCfg                                   
                                                                                
                                                                                
ninthCorrectCfg : MachineCfg BallOfWool Kitten                                 
ninthCorrectCfg =                                                              
  { currState = Orange                                                            
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red]                                                       
      , currSym   = Just Blue                                                     
      , rightSyms = fromList [Just Blue, Just Green, Just Yellow, Just Red, 
                              Nothing]                                
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
    , test "fifth cfg"                                                                    
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg 
    , test "sixth cfg"                                                          
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg 
    , test "eighth cfg"                                                          
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach)  
                     eighthCorrectMaybeCfg     
    , test "last cfg"                                                         
      <| assertEqual (ninthTestedCfg machine input machine.initHeadPosForMach) 
                     ninthCorrectMaybeCfg   
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                      9
    ]
-------------------------------------------------------------------------------

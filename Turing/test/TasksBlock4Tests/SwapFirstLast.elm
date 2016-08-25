-- 4_4 - Swap the first and the last balls

module TasksBlock4Tests.SwapFirstLast exposing (tests)

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
  , initHeadPosForMach = 1                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }


transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList
    [ { key   = (White, Just Yellow)                                            
      , value = (LightGrey, Nothing, MoveRight)
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveRight)
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Just Blue, MoveRight) 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Nothing, MoveLeft)
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Brown, Just Yellow, MoveLeft)
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveLeft)
      }                                           
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveLeft)
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
  [Nothing, Just Yellow, Just Green, Just Blue, Nothing]
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
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Green, Just Blue, Nothing]                                     
      }  
  }                                                                             
-----------------------

sndTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten) 
sndTestedCfg m inp hpos =   
  (runMach m inp hpos)   
  |> drop 1
  |> head                                              


sndCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
sndCorrectMaybeCfg = Just sndCorrectCfg


sndCorrectCfg : MachineCfg BallOfWool Kitten             
sndCorrectCfg =                                                                    
  { currState = LightGrey 
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Nothing, Nothing]                                 
      , currSym   = Just Green                                     
      , rightSyms = fromList [Just Blue, Nothing]                                
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
  { currState = Grey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Nothing, Just Green]
      , currSym   = Just Blue                                                                
      , rightSyms = fromList [Nothing]          
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
      { leftSyms  = fromList [Nothing, Nothing]                     
      , currSym   = Just Green                                                   
      , rightSyms = fromList [Just Yellow, Nothing]                                          
      }                                                                         
  }   
-----------------------                                                         
                                                                                
eighthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                      
eighthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
eighthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
eighthCorrectMaybeCfg = Just eighthCorrectCfg                                     
                                                                                
                                                                                
eighthCorrectCfg : MachineCfg BallOfWool Kitten                                  
eighthCorrectCfg =                                                               
  { currState = Orange                                                           
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Blue]                                 
      , currSym   = Just Green                                                  
      , rightSyms = fromList [Just Yellow, Nothing]                             
      }                                                                         
  }  


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "fifth cfg"                                                         
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach)    
                     fifthCorrectMaybeCfg 
    , test "sixth cfg"                                                          
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg   
    , test "last cfg"                                                                    
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach) 
                     eighthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     8
    ]
-------------------------------------------------------------------------------

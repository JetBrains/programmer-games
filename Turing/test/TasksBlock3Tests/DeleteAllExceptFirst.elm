-- 3_5 - Delete all balls except the first ball (dont change empty word)

module TasksBlock3Tests.DeleteAllExceptFirst exposing (tests)

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
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }


transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Red)                                               
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (LightGrey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Nothing, MoveRight)                                
      }  
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    ]

                      
input1 : List (Maybe BallOfWool)                                              
input1 =                                                                      
  [Just Red, Just Yellow, Just Green, Just Blue, Nothing]

input2 : List (Maybe BallOfWool)  
input2 = 
  [Nothing]
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
      { leftSyms  = empty
      , currSym   = Just Red 
      , rightSyms = fromList [Just Yellow, Just Green, Just Blue, Nothing]                                     
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
      { leftSyms  = fromList [Just Red]                                
      , currSym   = Just Yellow
      , rightSyms = fromList [Just Green, Just Blue, Nothing]                                
      }
  } 
-----------------------

sixthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
sixthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                   
  |> head                                                                       
                                                                                
                                                                                
sixthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
sixthCorrectMaybeCfg = Just sixthCorrectCfg                                     
                                                                                
                                                                                
sixthCorrectCfg : MachineCfg BallOfWool Kitten                                  
sixthCorrectCfg =                                                               
  { currState = Orange                                                          
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Nothing, Nothing]                            
      , currSym   = Nothing
      , rightSyms = fromList [Nothing]                                                     
      }                                                                         
  }
-----------------------                                                         
                                                                                
emptyWordLastTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool)
                         -> Int -> Maybe (MachineCfg BallOfWool Kitten)                    
emptyWordLastTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                    
  |> head                                                                       
                                                                                
                                                                                
emptyWordLastCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
emptyWordLastCorrectMaybeCfg = Just emptyWordLastCorrectCfg                                     
                                                                                
                                                                                
emptyWordLastCorrectCfg : MachineCfg BallOfWool Kitten                                  
emptyWordLastCorrectCfg =                                                               
  { currState = Orange                                                          
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                      
      , currSym   = Nothing                                                     
      , rightSyms = empty                                          
      }                                                                         
  }  
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.DeleteAllExceptFirst "                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input1 machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input1 machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "last cfg"                                                           
      <| assertEqual (sixthTestedCfg machine input1 machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg 
    , test "last cfg when empty word"                                                           
      <| assertEqual (emptyWordLastTestedCfg machine input2 
                                             machine.initHeadPosForMach) 
                     emptyWordLastCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input1 machine.initHeadPosForMach) 
                      |> length)
                     6
    ]
-------------------------------------------------------------------------------

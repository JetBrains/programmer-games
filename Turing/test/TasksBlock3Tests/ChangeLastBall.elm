-- 3_2 - Change one last ball

module TasksBlock3Tests.ChangeLastBall exposing (tests)

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
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }

                                                                                
transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList                                                                      
    [ { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Orange, Just Blue, MoveLeft)                                 
      }                                                                         
    ]

                                                                                
input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Yellow, Just Yellow, Just Yellow, Nothing]
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
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Yellow, Just Yellow, Nothing]                                     
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
  { currState = White
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Just Yellow]                                
      , currSym   = Just Yellow            
      , rightSyms = fromList [Just Yellow, Nothing]                                
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
      { leftSyms  = fromList [Just Yellow, Just Yellow]                            
      , currSym   = Just Yellow    
      , rightSyms = fromList [Nothing]                                                     
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
      { leftSyms  = fromList [Just Yellow]                
      , currSym   = Just Yellow
      , rightSyms = fromList [Just Blue, Nothing]                                                       
      }                                                                         
  }
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.ChangeLastBall"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "fifth cfg"                                                           
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach)  
                     fifthCorrectMaybeCfg 
    , test "last cfg"                                                           
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     6
    ]
-------------------------------------------------------------------------------

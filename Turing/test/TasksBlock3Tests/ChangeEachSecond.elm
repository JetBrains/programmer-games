-- 3_9 - Change each second ball to yellow ball

module TasksBlock3Tests.ChangeEachSecond exposing (tests)

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
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Red, Just Yellow, Just Green, Just Blue, Just Red, Just Green,          
   Just Blue, Just Red, Nothing]
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
      , rightSyms = fromList [Just Yellow, Just Green, Just Blue, Just Red, 
                              Just Green, Just Blue, Just Red, Nothing]
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
      , rightSyms = fromList [Just Green, Just Blue, Just Red, Just Green, 
                              Just Blue, Just Red, Nothing]
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
  { currState = White                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Just Yellow]                                       
      , currSym   = Just Green           
      , rightSyms = fromList [Just Blue, Just Red, Just Green, Just Blue, 
                              Just Red, Nothing]
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
      { leftSyms  = fromList [Just Red, Just Yellow, Just Green, Just Yellow]  
      , currSym   = Just Red
      , rightSyms = fromList [Just Green, Just Blue, Just Red, Nothing]                           
      }                                                                         
  }   
-----------------------                                                         
                                                                                
tenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
tenthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
tenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
tenthCorrectMaybeCfg = Just tenthCorrectCfg                                     
                                                                                
                                                                                
tenthCorrectCfg : MachineCfg BallOfWool Kitten                                  
tenthCorrectCfg =                                                               
  { currState = Orange                                                        
  , currDir   = MoveLeft                                                   
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Just Yellow, Just Green, Just Yellow, 
                              Just Red, Just Yellow, Just Blue]   
      , currSym   = Just Yellow
      , rightSyms = fromList [Nothing]         
      }                                                                         
  } 
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.ChangeEachSecond"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "third cfg"                                                         
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach)    
                     thirdCorrectMaybeCfg     
    , test "fifth cfg"                                                         
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach)    
                     fifthCorrectMaybeCfg    
    , test "last cfg"                                                          
      <| assertEqual (tenthTestedCfg machine input machine.initHeadPosForMach)  
                     tenthCorrectMaybeCfg   
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     10
    ]
-------------------------------------------------------------------------------

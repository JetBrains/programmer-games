-- 2_2 - Put balls on empty tape with a space after each one

module TasksBlock2Tests.BallsWithSpaceAfterEachOne exposing (tests)

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
    [ { key   = (White, Nothing)                                                
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key    = (LightGrey, Nothing)                                           
       , value = (Grey, Nothing, MoveRight)                               
      }                                                                         
    , { key    = (Grey, Nothing)                                                
       , value = (LightGrey, Just Green, MoveRight)                               
      }                                                                         
    , { key    = (Grey, Just Blue)                                              
       , value = (Orange, Just Blue, MoveLeft)                                
      }                                                                         
    ]

                                                                
input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Just Blue]             
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
      , currSym   = Nothing                                                        
      , rightSyms = fromList [Nothing, Nothing, Nothing, Nothing, Nothing, 
                              Just Blue]                                    
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
      , currSym   = Nothing                                    
      , rightSyms = fromList [Nothing, Nothing, Nothing, Nothing, Just Blue]
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
  { currState = Grey                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Nothing]                                         
      , currSym   = Nothing                                                     
      , rightSyms = fromList [Nothing, Nothing, Nothing, Just Blue]    
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
  { currState = LightGrey                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Nothing, Just Green]                                
      , currSym   = Nothing                                                     
      , rightSyms = fromList [Nothing, Nothing, Just Blue]             
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
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Nothing, Just Green, Nothing]                    
      , currSym   = Nothing                                                     
      , rightSyms = fromList [Nothing, Just Blue]                      
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
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red,Nothing,Just Green,Nothing,Just Green]           
      , currSym   = Nothing                                                
      , rightSyms = fromList [Just Blue]                               
      }                                                                         
  }
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock2Tests.BallsWithSpaceAfterEachOne"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
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
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach) 
                     eighthCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     8
    ]
-------------------------------------------------------------------------------

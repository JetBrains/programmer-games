-- 3_7 - Put green balls on the left side, yellow balls on the right side

module TasksBlock3Tests.GreenOnLeftYellowOnRight exposing (tests)

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
    [ { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (Grey, Just Yellow, MoveLeft)
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Brown, Just Green, MoveRight)
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Brown, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (LightGrey, Just Green, MoveRight)                                             
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Green, Just Yellow, Just Green, Just Yellow, Just Green, Just Yellow]
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
      , currSym   = Just Green 
      , rightSyms = fromList [Just Yellow, Just Green, Just Yellow, Just Green, 
                              Just Yellow]
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
      { leftSyms  = fromList [Just Green]                                                       
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Just Green, Just Yellow, Just Green, Just Yellow]                                      
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
  , currDir   = MoveRight                                                      
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Yellow]                            
      , currSym   = Just Green
      , rightSyms = fromList [Just Yellow, Just Green, Just Yellow]                                                     
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
      { leftSyms  = fromList [Just Green]                          
      , currSym   = Just Yellow                                                  
      , rightSyms = fromList [Just Yellow,Just Yellow,Just Green,Just Yellow]             
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
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green]                                       
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Just Yellow,Just Yellow,Just Green,Just Yellow]
      }                                                                         
  } 
-----------------------                                                         
                                                                                
seventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                   Int -> Maybe (MachineCfg BallOfWool Kitten)                    
seventhTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 6                                                                     
  |> head                                                                       
                                                                                
                                                                                
seventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
seventhCorrectMaybeCfg = Just seventhCorrectCfg                                     
                                                                                
                                                                                
seventhCorrectCfg : MachineCfg BallOfWool Kitten                                  
seventhCorrectCfg =                                                               
  { currState = LightGrey                                                           
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Green]                                       
      , currSym   = Just Yellow
      , rightSyms = fromList [Just Yellow,Just Green,Just Yellow]   
      }                                                                         
  } 
-----------------------                                                         
                                                                                
eighteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->      
                      Int -> Maybe (MachineCfg BallOfWool Kitten)                  
eighteenthTestedCfg m inp hpos =                                                   
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
eighteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                   
eighteenthCorrectMaybeCfg = Just eighteenthCorrectCfg                                 
                                                                                
                                                                                
eighteenthCorrectCfg : MachineCfg BallOfWool Kitten                                
eighteenthCorrectCfg =                                                             
  { currState = Orange                                                       
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Green, Just Green, Just Yellow, 
                              Just Yellow]       
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Nothing]               
      }                                                                         
  }
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.GreenOnLeftYellowOnRight"                                                        
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
    , test "sixth cfg"                                                         
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach) 
                     sixthCorrectMaybeCfg   
    , test "seventh cfg"                                                          
      <| assertEqual (seventhTestedCfg machine input 
                                       machine.initHeadPosForMach)  
                     seventhCorrectMaybeCfg  
    , test "last cfg"                                                        
      <| assertEqual (eighteenthTestedCfg machine input                            
                                          machine.initHeadPosForMach)              
                     eighteenthCorrectMaybeCfg    
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     18
    ]
-------------------------------------------------------------------------------

-- 6_3 - Turn the word (abb -> bba)

module TasksBlock6Tests.TurnWord exposing (tests)

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
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (DarkBrown, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Brown, Just Red, MoveLeft)                                 
      }   
    , { key   = (Grey, Just Red)                                                
      , value = (Grey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Grey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Brown, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Red)                                               
      , value = (Brown, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (White, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (DarkBrown, Just Red)                                           
      , value = (Orange, Just Red, MoveRight)                                
      }
    , { key   = (DarkBrown, Just Yellow)                                        
      , value = (DarkBrown, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (DarkBrown, Just Green)                                         
      , value = (Orange, Just Green, MoveRight)                                
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =  
  [Nothing, Just Red, Just Green, Just Green, Nothing, Nothing, Nothing,        
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
      { leftSyms  = fromList [Nothing, Just Red, Just Green]                                                           
      , currSym   = Just Green                                                       
      , rightSyms = fromList [Nothing, Nothing, Nothing, Nothing]
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
  { currState = Grey 
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Nothing, Just Red, Just Green, Just Yellow]                                
      , currSym   = Nothing
      , rightSyms = fromList [Nothing, Nothing, Nothing]                                
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
  { currState = Brown                                                            
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red, Just Green]       
      , currSym   = Just Yellow                                        
      , rightSyms = fromList [Just Green, Nothing, Nothing, Nothing]                        
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
  { currState = White                                                           
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red]                    
      , currSym   = Just Green                                      
      , rightSyms = fromList [Just Yellow,Just Green,Nothing,Nothing,Nothing]            
      }                                                                         
  }    
-----------------------
                                                                               
twelfthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                   Int -> Maybe (MachineCfg BallOfWool Kitten)                   
twelfthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> drop 11                                                                     
  |> head 

                                                                         
twelfthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
twelfthCorrectMaybeCfg = Just twelfthCorrectCfg                                   
                                                                                
                                                                                
twelfthCorrectCfg : MachineCfg BallOfWool Kitten                                 
twelfthCorrectCfg =                                                              
  { currState = LightGrey                                                       
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Yellow]                                
      , currSym   = Just Yellow                                                  
      , rightSyms = fromList [Just Yellow, Just Green, Just Green, Nothing, 
                              Nothing]   
      }                                                                         
  }  
-----------------------                                                         
                                                                                
twentyThirdTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->      
                       Int -> Maybe (MachineCfg BallOfWool Kitten)                  
twentyThirdTestedCfg m inp hpos =                                                   
  (runMach m inp hpos)                                                          
  |> drop 22                                                                    
  |> head                                                                       
                                                                                
                                                                                
twentyThirdCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                   
twentyThirdCorrectMaybeCfg = Just twentyThirdCorrectCfg                                 
                                                                                
                                                                                
twentyThirdCorrectCfg : MachineCfg BallOfWool Kitten                                
twentyThirdCorrectCfg =                                                             
  { currState = DarkBrown                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                             
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Just Yellow, Just Yellow, Just Green, Just Green, 
                              Just Red, Nothing]                                          
      }                                                                         
  }  
-----------------------                                                         
                                                                                
twentySeventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->  
                         Int -> Maybe (MachineCfg BallOfWool Kitten)              
twentySeventhTestedCfg m inp hpos =                                               
  (runMach m inp hpos)                                                          
  |> reverse                                                                    
  |> head                                                                       
                                                                                
                                                                                
twentySeventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)               
twentySeventhCorrectMaybeCfg = Just twentySeventhCorrectCfg                         
                                                                                
                                                                                
twentySeventhCorrectCfg : MachineCfg BallOfWool Kitten                            
twentySeventhCorrectCfg =                                                         
  { currState = Orange
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Nothing, Just Green]                                          
      , currSym   = Just Green 
      , rightSyms = fromList [Just Red, Nothing]                                
      }                                                                         
  }  
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock6Tests.TurnWord"                                                        
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
    , test "twelfth cfg"                                                         
      <| assertEqual (twelfthTestedCfg machine input 
                                       machine.initHeadPosForMach) 
                     twelfthCorrectMaybeCfg
    , test "twenty third cfg"                                                        
      <| assertEqual (twentyThirdTestedCfg machine input 
                                           machine.initHeadPosForMach)
                     twentyThirdCorrectMaybeCfg   
    , test "last cfg"                                                   
      <| assertEqual (twentySeventhTestedCfg machine input                        
                                             machine.initHeadPosForMach)          
                     twentySeventhCorrectMaybeCfg        
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     27
    ]
-------------------------------------------------------------------------------

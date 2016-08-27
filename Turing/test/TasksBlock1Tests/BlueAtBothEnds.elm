-- 1_3 - Put blue balls at the both ends of word

module TasksBlock1Tests.BlueAtBothEnds exposing (tests)

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
    , { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveRight)
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Just Blue, MoveLeft)                                             
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Just Red, MoveLeft)                                             
      }   
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveLeft)
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveLeft)
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Just Blue, MoveRight)                                
      }                                                                         
    ]                                                                           
                                                                                
input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green]
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
      , rightSyms = fromList [Just Yellow, Just Green]                                     
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
      { leftSyms  = fromList [Nothing, Just Red]                                 
      , currSym   = Just Yellow                    
      , rightSyms = fromList [Just Green]                                
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
      { leftSyms  = fromList [Nothing, Just Red, Just Yellow]
      , currSym   = Just Green
      , rightSyms = fromList [Just Blue]          
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
  { currState = LightGrey                                                       
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red]                   
      , currSym   = Just Yellow                                      
      , rightSyms = fromList [Just Green, Just Blue]                                        
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
      { leftSyms  = fromList [Just Blue]                                
      , currSym   = Just Red                                         
      , rightSyms = fromList [Just Yellow, Just Green, Just Blue]                            
      }                                                                         
  } 
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock1Tests.BlueAtBothEnds"                                                        
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
      <| assertEqual (ninthTestedCfg machine input machine.initHeadPosForMach)  
                     ninthCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     9
    ]
-------------------------------------------------------------------------------

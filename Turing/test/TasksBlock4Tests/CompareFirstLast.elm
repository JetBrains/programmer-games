-- 4_5 - If the first and the last balls are the same, then dont change the     
-- word, else change it to the empty word

module TasksBlock4Tests.CompareFirstLast exposing (tests)

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
    [ { key   = (White, Just Red)                                               
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)                                
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
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (Orange, Just Red, MoveLeft)                                 
      }                               
    , { key   = (Grey, Just Yellow)                                             
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Red)                                               
      , value = (Brown, Nothing, MoveLeft)
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    ]  

input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green, Just Blue, Just Yellow, Nothing] 
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
      , rightSyms = fromList [Just Yellow, Just Green, Just Blue, Just Yellow, 
                              Nothing]
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
      { leftSyms  = fromList [Nothing, Just Red]                                 
      , currSym   = Just Yellow                                     
      , rightSyms = fromList [Just Green, Just Blue, Just Yellow, Nothing]                                
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
  { currState = Grey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Just Red, Just Yellow, Just Green, 
                              Just Blue]
      , currSym   = Just Yellow                                   
      , rightSyms = fromList [Nothing]          
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
  { currState = Brown                                                            
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red, Just Yellow, Just Green]
      , currSym   = Just Blue
      , rightSyms = fromList [Nothing, Nothing]
      }                                                                         
  } 
-----------------------                                                         
                                                                                
thirteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                      Int -> Maybe (MachineCfg BallOfWool Kitten)                  
thirteenthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> reverse
  |> head                                                                       
                                                                                
                                                                                
thirteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
thirteenthCorrectMaybeCfg = Just thirteenthCorrectCfg
                                                                                
                                                                                
thirteenthCorrectCfg : MachineCfg BallOfWool Kitten                                 
thirteenthCorrectCfg =                                                              
  { currState = Orange                                                           
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]       
      , currSym   = Nothing                                                   
      , rightSyms = fromList [Nothing, Nothing, Nothing, Nothing, Nothing]                                 
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
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "seventh cfg"                                                                    
      <| assertEqual (seventhTestedCfg machine input 
                                       machine.initHeadPosForMach)
                     seventhCorrectMaybeCfg 
    , test "eighth cfg"                                                        
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach)              
                     eighthCorrectMaybeCfg   
    , test "last cfg"                                                         
      <| assertEqual (thirteenthTestedCfg machine input 
                                          machine.initHeadPosForMach) 
                     thirteenthCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     13
    ]
-------------------------------------------------------------------------------

-- 4_2 - Move the first (Yellow, Green, Blue) ball to the end of word (for      
-- empty word, one symb word, few symb word)  

module TasksBlock4Tests.FirstToEnd exposing (tests)

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
    [ { key   = (White, Just Yellow)                                            
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (Brown, Nothing, MoveRight)
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
      , value = (Orange, Just Yellow, MoveLeft)                                 
      }      
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveRight) 
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Grey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Grey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (Orange, Just Blue, MoveLeft)
      }                                                                         
    ] 

input : List (Maybe BallOfWool)                                              
input =  
  [Just Yellow, Just Green, Just Blue, Nothing] 
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
      { leftSyms  = fromList [Nothing]                                 
      , currSym   = Just Green                                     
      , rightSyms = fromList [Just Blue, Nothing]                                
      }
  } 
-----------------------

fifthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten)
fifthTestedCfg m inp hpos =  
  (runMach m inp hpos)
  |> reverse
  |> head                                                      
                                     

fifthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
fifthCorrectMaybeCfg = Just fifthCorrectCfg                                     


fifthCorrectCfg : MachineCfg BallOfWool Kitten         
fifthCorrectCfg =                                                                
  { currState = Orange   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Just Green]
      , currSym   = Just Blue                                                                
      , rightSyms = fromList [Just Yellow]          
      }
  }     
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock4Tests.FirstToEnd"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "last cfg"                                                                    
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     5
    ]
-------------------------------------------------------------------------------

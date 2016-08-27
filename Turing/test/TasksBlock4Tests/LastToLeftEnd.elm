-- 4_3 - Move the last (Yellow, Green) ball to the left end of word (for empty  
-- word, one symb word, few symb word)

module TasksBlock4Tests.LastToLeftEnd exposing (tests)

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
      , value = (White, Just Yellow, MoveRight)
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (Brown, Nothing, MoveLeft) 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Nothing, MoveRight)
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveLeft)
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Grey, Just Green, MoveLeft) 
      }                                            
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Just Yellow, MoveRight) 
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveLeft)
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveLeft)
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (Orange, Just Green, MoveRight)
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =  
  [Nothing, Just Yellow, Just Green, Nothing]
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
      , rightSyms = fromList [Just Green, Nothing]                                     
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
  , currDir   = MoveLeft
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Nothing, Just Yellow]                                 
      , currSym   = Just Green                                     
      , rightSyms = fromList [Nothing]                                
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
  { currState = Brown                                                          
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                              
      , currSym   = Just Yellow                                                   
      , rightSyms = fromList [Nothing, Nothing]                                      
      }                                                                         
  }                                                                             
------------------------------------------------------------------------------- 

seventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten)
seventhTestedCfg m inp hpos =  
  (runMach m inp hpos)
  |> reverse
  |> head                                                      
                                     

seventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
seventhCorrectMaybeCfg = Just seventhCorrectCfg        


seventhCorrectCfg : MachineCfg BallOfWool Kitten         
seventhCorrectCfg =                                                                
  { currState = Orange   
  , currDir   = MoveRight
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Just Green]
      , currSym   = Just Yellow                                                                
      , rightSyms = fromList [Nothing, Nothing]          
      }
  }     
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock4Tests.LastToLeftEnd"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "fourth cfg"                                                          
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)  
                     fourthCorrectMaybeCfg          
    , test "fifth cfg"                                                                    
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg 
    , test "last cfg"                                                          
      <| assertEqual (seventhTestedCfg machine input 
                                       machine.initHeadPosForMach)  
                     seventhCorrectMaybeCfg     
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                      7
    ]
-------------------------------------------------------------------------------

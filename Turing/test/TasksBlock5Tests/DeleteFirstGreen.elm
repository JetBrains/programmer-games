-- 5_2 - Delete first occurrence of green ball, if it exists, else change the   
-- word to empty word  

module TasksBlock5Tests.DeleteFirstGreen exposing (tests)

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
    , { key   = (White, Just Blue)                                              
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (Orange, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Just Yellow, MoveRight)                                
      }       
    , { key   = (Grey, Just Yellow)                                             
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Grey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Orange, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Just Blue, MoveRight)                                
      }                                                                         
    ]  


input : List (Maybe BallOfWool)                                              
input = 
  [Just Blue, Just Yellow, Just Green, Just Green, Nothing]  
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
      , currSym   = Just Blue                                                        
      , rightSyms = fromList [Just Yellow, Just Green, Just Green, Nothing]                                     
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
      { leftSyms  = fromList [Nothing]                                 
      , currSym   = Just Yellow                                     
      , rightSyms = fromList [Just Green, Just Green, Nothing]                                
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
      { leftSyms  = fromList [Nothing, Just Blue]
      , currSym   = Just Green                                                                
      , rightSyms = fromList [Just Green, Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
fourthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fourthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
fourthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
fourthCorrectMaybeCfg = Just fourthCorrectCfg                                     
                                                                                
                                                                                
fourthCorrectCfg : MachineCfg BallOfWool Kitten                                  
fourthCorrectCfg =                                                               
  { currState = Orange                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Blue, Just Yellow]                               
      , currSym   = Just Green                                                  
      , rightSyms = fromList [Nothing]                              
      }                                                                         
  }  
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock5Tests.DeleteFirstGreen"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "third cfg"                                                                    
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach) 
                     thirdCorrectMaybeCfg 
    , test "last cfg"                                                          
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)  
                     fourthCorrectMaybeCfg    
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     4
    ]
-------------------------------------------------------------------------------

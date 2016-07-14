-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing   

module CatWoolTest exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)                                         
import Turing exposing ( Machine, MachineCfg, TapeCfg, Direction(..),            
                         TransTable, run, initMachineCfg, transFunc)      
                                                                                
                                                                                
--TEST DATA-------------------------------------------------------------------  
 
-- Test Data for tests (types, machine, transition table and input word)
                                                                                
type BallOfWool = Red | Orange | Yellow | Green | Blue                          
type Kitten = Grey | Black | Brown | LightGrey 

testMachine : Machine BallOfWool Kitten                                         
testMachine =                                                                   
  { transition = (transFunc transTable (Black, Nothing, MoveLeft))              
  , startState = LightGrey                                                      
  , acceptState = Brown                                                         
  , rejectState = Black                                                         
  }                                                                             
                                                                                
                                                                                
transTable : TransTable BallOfWool Kitten                                       
transTable =                                                                    
  [ { key = (LightGrey, Just Red), 
      value = (LightGrey, Just Red, MoveRight)
    }    
  , { key = (LightGrey, Nothing), 
      value = (Grey, Just Red, MoveLeft)
    }           
  , { key = (Grey, Just Red), 
      value = (Grey, Just Red, MoveLeft)
    }               
  , { key = (Grey, Nothing), 
      value = (Brown, Just Blue, MoveRight)
    }             
  ]                                                                             
                                                                                
                                                                                
input : List (Maybe BallOfWool)                                                 
input =                                                                         
  [Just Red, Nothing, Just Orange, Just Yellow, Just Green, Just Blue]

------------------------------------------------------------------------------  
------------------------------------------------------------------------------  

                                                                                
--HELPERS---------------------------------------------------------------------  
                                                                                
-- Common run function for all tests                                            
                                                                                
runRes : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> List (MachineCfg BallOfWool Kitten)    
runRes m inp =                                                                  
  let                                                                           
    init = (initMachineCfg m inp)                                               
  in                                                                            
    (run m init [init])                                                         
                                                                                
                                                                                
--HEAD------------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs                         

headCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
headCfgForCheck m inp =                                                         
  (List.head (runRes m inp))                                                    
                                                                                
headCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                        
headTCfg inp =                                                                  
  { leftSyms =  Array.empty                                                     
  , currSym =                                                                   
      case (List.head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms =                                                                 
      case (List.tail inp) of                                                   
        Just t -> (Array.fromList t)                                            
        Nothing -> Array.empty                                                  
                                                                                
  }     

------------------------------------------------------------------------------   
------------------------------------------------------------------------------  
                                                                                
                                                                                
--MIDDLE----------------------------------------------------------------------  
 
-- Check if go to state Grey when read first Nothing                            
-- (only one transition for middle check here)                                  
                                                                                
fstTransCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
fstTransCfgForCheck m inp =                                                     
  (List.head (List.drop 2 (runRes m inp))) -- 3 config in list                                                
                                                                                
fstTransCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                            
                                                                                
fstTransMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten
fstTransMCfg m inp =                                                            
  { currState = Grey                                                            
  , tapeCfg = (fstTransTCfg inp)                                                
  }                                                                             
                                                                                
fstTransTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                    
fstTransTCfg inp =                                                              
  { leftSyms = Array.empty                                                      
  , currSym =                                                                   
      case (List.head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = Array.fromList ([Just Red] ++ (List.drop 2 inp))                
  } 

------------------------------------------------------------------------------   
------------------------------------------------------------------------------  
                                                                                
                                                                                
--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs               

lastCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgForCheck m inp =                                                         
  (List.head (List.reverse (runRes m inp)))                                     
                                                                                
lastCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgCorrect m inp = Just (lastMCfg m inp)                                    
                                                                                
lastMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten
lastMCfg m inp =                                                                
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             
                                                                                
lastTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                        
lastTCfg inp =                                                                  
  { leftSyms = (Array.fromList [Just Blue])                                     
  , currSym =                                                                   
      case (List.head inp) of                                                   
        Just c -> c                                                             
        Nothing -> Nothing                                                      
  , rightSyms = ( Array.fromList ([Just Red] ++ (List.drop 2 inp)) )            
  } 

------------------------------------------------------------------------------   
------------------------------------------------------------------------------

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual ( headCfgForCheck testMachine input ) 
                   ( headCfgCorrect testMachine input )
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual ( fstTransCfgForCheck testMachine input )                       
                   ( fstTransCfgCorrect testMachine input )  
  , test "last"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine input ) 
                   ( lastCfgCorrect testMachine input )
  , test "count"
    <| assertEqual (List.length (runRes testMachine input)) 5
  ] 



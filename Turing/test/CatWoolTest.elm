-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing   

module CatWoolTest exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)   
import List exposing (head, tail, reverse, length, drop, take)

import TuringTypes exposing ( Machine, MachineCfg, TapeCfg, Direction(..), 
                              TransTable )
import RunTuring exposing (runMachine, transFunc)                                      
import InitUpdate exposing (initMachineCfg)  

                                                                                
--TEST DATA-------------------------------------------------------------------  
 
-- Test Data for tests (types, machine, transition table and input word)
                                                                                
type BallOfWool = Red | Orange | Yellow | Green | Blue                          
type Kitten = Grey | Black | Brown | LightGrey 

testMachine : Machine BallOfWool Kitten                                         
testMachine =                                                                   
  { transition = (transFunc transTable (Black, Nothing, MoveLeft))              
  , initHeadPosForDraw = 3                                                      
  , initHeadPosForMach = 0 
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
                                                                                
runRes : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Int
         -> List (MachineCfg BallOfWool Kitten)    
runRes m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)                                               
  in                                                                            
    (runMachine m init [init])                                                         
                                                                                
                                                                                
--HEAD------------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs                         

headCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Int 
                  -> Maybe (MachineCfg BallOfWool Kitten)
headCfgForCheck m inp hpos =                                                         
  (head (runRes m inp hpos))                                                    
                                                                                
headCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
                 -> Maybe (MachineCfg BallOfWool Kitten)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
           -> MachineCfg BallOfWool Kitten
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                        
headTCfg inp =                                                                  
  { leftSyms = empty                                                     
  , currSym =                                                                   
      case (head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms =                                                                 
      case (tail inp) of                                                   
        Just t -> (fromList t)                                            
        Nothing -> empty                                                  
                                                                                
  }     

------------------------------------------------------------------------------   
------------------------------------------------------------------------------  
                                                                                
                                                                                
--MIDDLE----------------------------------------------------------------------  
 
-- Check if go to state Grey when read first Nothing                            
-- (only one transition for middle check here)                                  
                                                                                
fstTransCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
                      -> Int -> Maybe (MachineCfg BallOfWool Kitten)
fstTransCfgForCheck m inp hpos =                                                     
  (head (drop 2 (runRes m inp hpos))) -- 3 config in list                                                
                                                                                
fstTransCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
                     -> Maybe (MachineCfg BallOfWool Kitten)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                            
                                                                                
fstTransMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
               -> MachineCfg BallOfWool Kitten
fstTransMCfg m inp =                                                            
  { currState = Grey                                                            
  , tapeCfg = (fstTransTCfg inp)                                                
  }                                                                             
                                                                                
fstTransTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                    
fstTransTCfg inp =                                                              
  { leftSyms = empty                                                      
  , currSym =                                                                   
      case (head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = fromList ([Just Red] ++ (drop 2 inp))                
  } 

------------------------------------------------------------------------------   
------------------------------------------------------------------------------  
                                                                                
                                                                                
--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs               

lastCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Int 
                  -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgForCheck m inp hpos =                                                         
  (head (reverse (runRes m inp hpos)))                                     
                                                                                
lastCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
                 -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgCorrect m inp = Just (lastMCfg m inp)                                    
                                                                                
lastMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) 
           -> MachineCfg BallOfWool Kitten
lastMCfg m inp =                                                                
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             
                                                                                
lastTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                        
lastTCfg inp =                                                                  
  { leftSyms = (fromList [Just Blue])                                     
  , currSym =                                                                   
      case (head inp) of                                                   
        Just c -> c                                                             
        Nothing -> Nothing                                                      
  , rightSyms = ( fromList ([Just Red] ++ (drop 2 inp)) )            
  } 

------------------------------------------------------------------------------   
------------------------------------------------------------------------------

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual (headCfgForCheck testMachine input   
                                    testMachine.initHeadPosForMach) 
                   (headCfgCorrect testMachine input)
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual (fstTransCfgForCheck testMachine input 
                                        testMachine.initHeadPosForMach)                       
                   (fstTransCfgCorrect testMachine input)  
  , test "last"                                                                    
    <| assertEqual (lastCfgForCheck testMachine input 
                                    testMachine.initHeadPosForMach) 
                   (lastCfgCorrect testMachine input)
  , test "count"
    <| assertEqual (length (runRes testMachine input 
                                   testMachine.initHeadPosForMach)) 5
  ] 

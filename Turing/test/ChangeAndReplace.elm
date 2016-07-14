-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)                                         
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

module ChangeAndReplace exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)
import Turing exposing ( Machine, MachineCfg, TapeCfg, Direction(..),            
                         TransTable, run, initMachineCfg, transFunc)      


--TEST DATA-------------------------------------------------------------------

-- Test data for all tests (machine, transition table, input) 

testMachine : Machine Char Int                                                  
testMachine =                                                                   
  { transition = (transFunc transTable (4, Nothing, MoveLeft))                  
  , startState = 0                                                              
  , acceptState = 3                                                             
  , rejectState = 4                                                             
  }                                                                             
                                                                                
                                                                                
transTable : TransTable Char Int                                                
transTable =                                                                    
  [ { key = (0, Nothing),  value = (0, Nothing, MoveRight)}                     
  , { key = (0, Just '0'), value = (1, Just '1', MoveRight)}                    
  , { key = (0, Just '1'), value = (1, Just '0', MoveRight)}                    
  , { key = (1, Nothing),  value = (2, Nothing, MoveLeft)}                      
  , { key = (1, Just '0'), value = (1, Just '1', MoveRight)}                    
  , { key = (1, Just '1'), value = (1, Just '0', MoveRight)}                    
  , { key = (2, Just '0'), value = (2, Just '0', MoveLeft)}                     
  , { key = (2, Just '1'), value = (2, Just '1', MoveLeft)}                     
  , { key = (2, Nothing), value = (3, Nothing, MoveRight)}                      
  ]                                                                             
                                                                                
                                                                                
inputForCh : List (Maybe Char)                                                  
inputForCh =                                                                    
  [Nothing, Nothing, Just '0', Nothing, Nothing]                                
                                                                                
                                                                                
inputForRepl : List (Maybe Char)                                                
inputForRepl =                                                                  
  [Nothing, Nothing, Just '0', Just '1', Nothing]   

------------------------------------------------------------------------------  
------------------------------------------------------------------------------  


--HELPERS---------------------------------------------------------------------  
                                                                                
-- Common run function for all tests                                            
                                                                                
runRes : Machine Char Int -> List (Maybe Char) -> List (MachineCfg Char Int)    
runRes m inp =                                                                  
  let                                                                           
    init = (initMachineCfg m inp)                                               
  in                                                                            
    (run m init [init])                                                         

                                                                                
--HEAD----------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs  

headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgForCheck m inp =                                                         
  (List.head (runRes m inp))                                                    
                                                                                
headCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe Char) -> TapeCfg Char                                    
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

-- Check if do transition in two important cases:                               
-- 1) go to state 1 when read first Just c                                      
--    (check that  first "Just c" change to the opposite value)                 
-- 2) go to state 2 from state 1 when read first Nothing after Just c           
--    (check that all of "Just c" change to the opposite values) 

fstTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckC m inp =                                                    
  (List.head (List.drop 3 (runRes m inp))) -- 4 config in list                                               
                                                                                
fstTransCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectC m inp = Just (fstTransMCfgC m inp)                          
                                                                                
fstTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
fstTransMCfgC m inp =                                                           
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfgC inp)                                               
  }                                                                             
                                                                                
fstTransTCfgC : List (Maybe Char) -> TapeCfg Char                               
fstTransTCfgC inp =                                                             
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                 
  , currSym = Nothing                                                           
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  

sndTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckC m inp =                                                    
  (List.head (List.drop 4 (runRes m inp))) -- 5 config in list                  
                                                                                
sndTransCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectC m inp = Just (sndTransMCfgC m inp)                          
                                                                                
sndTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
sndTransMCfgC m inp =                                                           
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgC inp)                                               
  }                                                                             
                                                                                
sndTransTCfgC : List (Maybe Char) -> TapeCfg Char                               
sndTransTCfgC inp =                                                             
  { leftSyms = Array.fromList (List.take 2 inp)                                 
  , currSym = Just '1'                                                          
  , rightSyms = Array.fromList (List.drop 3 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  

fstTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckR m inp =                                                    
  (List.head (List.drop 3 (runRes m inp))) -- 4 config in list                                               
                                                                                
fstTransCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectR m inp = Just (fstTransMCfgR m inp)                          
                                                                                
fstTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
fstTransMCfgR m inp =                                                           
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfgR inp)                                               
  }                                                                             
                                                                                
fstTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
fstTransTCfgR inp =                                                             
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                 
  , currSym = Just '1'                                                          
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  
                                                                                
sndTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckR m inp =                                                    
  (List.head (List.drop 5 (runRes m inp))) -- 6 config in list                  
                                                                                
sndTransCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectR m inp = Just (sndTransMCfgR m inp)                          
                                                                                
sndTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
sndTransMCfgR m inp =                                                           
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgR inp)                                               
  }                                                                             
                                                                                
sndTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
sndTransTCfgR inp =                                                             
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                 
  , currSym = Just '0'                                                          
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  }

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 


--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs 

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp =                                                         
  (List.head (List.reverse (runRes m inp)))                                     
                                                                                
lastCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrectC m inp = Just (lastMCfgC m inp)                                  
                                                                                
lastMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int        
lastMCfgC m inp =                                                               
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfgC inp)                                                   
  }                                                                             
                                                                                
lastTCfgC : List (Maybe Char) -> TapeCfg Char                                   
lastTCfgC inp =                                                                 
    { leftSyms = (Array.fromList (List.take 2 inp))                             
    , currSym = Just '1'                                                        
    , rightSyms = (Array.fromList (List.drop 3 inp))                            
    }                                                                           
                                                                                
------------------------------------------------------------------------------  

lastCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrectR m inp = Just (lastMCfgR m inp)                                  
                                                                                
lastMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int        
lastMCfgR m inp =                                                               
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfgR inp)                                                   
  }                                                                             
                                                                                
lastTCfgR : List (Maybe Char) -> TapeCfg Char                                   
lastTCfgR inp =                                                                 
  { leftSyms = (Array.fromList (List.take 2 inp))                               
  , currSym = Just '1'                                                          
  , rightSyms = (Array.fromList ([Just '0'] ++ (List.drop 4 inp)) )             
  }                                                                             
     
------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head for change test"     
    <| assertEqual ( headCfgForCheck testMachine inputForCh ) 
                   ( headCfgCorrect testMachine inputForCh )
  , test "head for replace test"                                                  
    <| assertEqual ( headCfgForCheck testMachine inputForRepl )                        
                   ( headCfgCorrect testMachine inputForRepl )  

  , test "first transition (see in the middle block) for change"                                                                 
    <| assertEqual ( fstTransCfgForCheckC testMachine inputForCh )                       
                   ( fstTransCfgCorrectC testMachine inputForCh )  
  , test "second transition (see in the middle block) for change"                           
    <| assertEqual ( sndTransCfgForCheckC testMachine inputForCh )                    
                   ( sndTransCfgCorrectC testMachine inputForCh )  
  , test "first transition (see in the middle block) for replace"                           
    <| assertEqual ( fstTransCfgForCheckR testMachine inputForRepl )                    
                   ( fstTransCfgCorrectR testMachine inputForRepl )                     
  , test "second transition (see in the middle block) for replace"                          
    <| assertEqual ( sndTransCfgForCheckR testMachine inputForRepl )                    
                   ( sndTransCfgCorrectR testMachine inputForRepl )  

  , test "last for change"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine inputForCh ) 
                   ( lastCfgCorrectC testMachine inputForCh )
  , test "last for replace"                                                         
    <| assertEqual ( lastCfgForCheck testMachine inputForRepl )                        
                   ( lastCfgCorrectR testMachine inputForRepl )  
  , test "count for change"
    <| assertEqual (List.length (runRes testMachine inputForCh)) 7
  , test "count for replace"                                                     
    <| assertEqual (List.length (runRes testMachine inputForRepl)) 9
  ] 

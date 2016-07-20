-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)                                         
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

module ChangeAndReplace exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)
import List exposing (head, tail, reverse, length, drop, take)

import TuringTypes exposing ( Machine, MachineCfg, TapeCfg, Direction(..), 
                              TransTable )
import RunTuring exposing (runMachine, transFunc)                                      
import InitUpdate exposing (initMachineCfg) 


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

initHeadPos : Int                                                               
initHeadPos = 0    

------------------------------------------------------------------------------  
------------------------------------------------------------------------------  


--HELPERS---------------------------------------------------------------------  
                                                                                
-- Common run function for all tests                                            
                                                                                
runRes : Machine Char Int -> List (Maybe Char) -> Int 
         -> List (MachineCfg Char Int)    
runRes m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)                                               
  in                                                                            
    (runMachine m init [init])                                                         

                                                                                
--HEAD----------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs  

headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                  -> Maybe (MachineCfg Char Int)
headCfgForCheck m inp hpos =                                                         
  (head (runRes m inp hpos))                                                    
                                                                                
headCfgCorrect : Machine Char Int -> List (Maybe Char) 
                 -> Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe Char) -> TapeCfg Char                                    
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

-- Check if do transition in two important cases:                               
-- 1) go to state 1 when read first Just c                                      
--    (check that  first "Just c" change to the opposite value)                 
-- 2) go to state 2 from state 1 when read first Nothing after Just c           
--    (check that all of "Just c" change to the opposite values) 

fstTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Int 
                       -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckC m inp hpos =                                                    
  (head (drop 3 (runRes m inp hpos))) -- 4 config in list                                               
                                                                                
fstTransCfgCorrectC : Machine Char Int -> List (Maybe Char) 
                      -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectC m inp = Just (fstTransMCfgC m inp)                          
                                                                                
fstTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
fstTransMCfgC m inp =                                                           
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfgC inp)                                               
  }                                                                             
                                                                                
fstTransTCfgC : List (Maybe Char) -> TapeCfg Char                               
fstTransTCfgC inp =                                                             
  { leftSyms = fromList ((take 2 inp) ++ [Just '1'])                 
  , currSym = Nothing                                                           
  , rightSyms = fromList (drop 4 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  

sndTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Int 
                       -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckC m inp hpos =                                                    
  (head (drop 4 (runRes m inp hpos))) -- 5 config in list                  
                                                                                
sndTransCfgCorrectC : Machine Char Int -> List (Maybe Char) 
                      -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectC m inp = Just (sndTransMCfgC m inp)                          
                                                                                
sndTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
sndTransMCfgC m inp =                                                           
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgC inp)                                               
  }                                                                             
                                                                                
sndTransTCfgC : List (Maybe Char) -> TapeCfg Char                               
sndTransTCfgC inp =                                                             
  { leftSyms = fromList (take 2 inp)                                 
  , currSym = Just '1'                                                          
  , rightSyms = fromList (drop 3 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  

fstTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Int 
                       -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckR m inp hpos =                                                    
  (head (drop 3 (runRes m inp hpos))) -- 4 config in list                                               
                                                                                
fstTransCfgCorrectR : Machine Char Int -> List (Maybe Char) 
                      -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectR m inp = Just (fstTransMCfgR m inp)                          
                                                                                
fstTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
fstTransMCfgR m inp =                                                           
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfgR inp)                                               
  }                                                                             
                                                                                
fstTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
fstTransTCfgR inp =                                                             
  { leftSyms = fromList ((take 2 inp) ++ [Just '1'])                 
  , currSym = Just '1'                                                          
  , rightSyms = fromList (drop 4 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  
                                                                                
sndTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Int 
                       -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckR m inp hpos =                                                    
  (head (drop 5 (runRes m inp hpos))) -- 6 config in list                  
                                                                                
sndTransCfgCorrectR : Machine Char Int -> List (Maybe Char) 
                      -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectR m inp = Just (sndTransMCfgR m inp)                          
                                                                                
sndTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
sndTransMCfgR m inp =                                                           
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgR inp)                                               
  }                                                                             
                                                                                
sndTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
sndTransTCfgR inp =                                                             
  { leftSyms = fromList ((take 2 inp) ++ [Just '1'])                 
  , currSym = Just '0'                                                          
  , rightSyms = fromList (drop 4 inp)                                
  }

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 


--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs 

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                  -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp hpos =                                                         
  (head (reverse (runRes m inp hpos)))                                     
                                                                                
lastCfgCorrectC : Machine Char Int -> List (Maybe Char) 
                  -> Maybe (MachineCfg Char Int)
lastCfgCorrectC m inp = Just (lastMCfgC m inp)                                  
                                                                                
lastMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int        
lastMCfgC m inp =                                                               
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfgC inp)                                                   
  }                                                                             
                                                                                
lastTCfgC : List (Maybe Char) -> TapeCfg Char                                   
lastTCfgC inp =                                                                 
    { leftSyms = (fromList (take 2 inp))                             
    , currSym = Just '1'                                                        
    , rightSyms = (fromList (drop 3 inp))                            
    }                                                                           
                                                                                
------------------------------------------------------------------------------  

lastCfgCorrectR : Machine Char Int -> List (Maybe Char) 
                  -> Maybe (MachineCfg Char Int)
lastCfgCorrectR m inp = Just (lastMCfgR m inp)                                  
                                                                                
lastMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int        
lastMCfgR m inp =                                                               
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfgR inp)                                                   
  }                                                                             
                                                                                
lastTCfgR : List (Maybe Char) -> TapeCfg Char                                   
lastTCfgR inp =                                                                 
  { leftSyms = (fromList (take 2 inp))                               
  , currSym = Just '1'                                                          
  , rightSyms = (fromList ([Just '0'] ++ (drop 4 inp)) )             
  }                                                                             
     
------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head for change test"     
    <| assertEqual (headCfgForCheck testMachine inputForCh initHeadPos) 
                   (headCfgCorrect testMachine inputForCh)
  , test "head for replace test"                                                  
    <| assertEqual (headCfgForCheck testMachine inputForRepl initHeadPos)                        
                   (headCfgCorrect testMachine inputForRepl)  
  , test "first transition (see in the middle block) for change"  
    <| assertEqual (fstTransCfgForCheckC testMachine inputForCh initHeadPos)
                   (fstTransCfgCorrectC testMachine inputForCh)  
  , test "second transition (see in the middle block) for change"                           
    <| assertEqual (sndTransCfgForCheckC testMachine inputForCh initHeadPos)                    
                   (sndTransCfgCorrectC testMachine inputForCh)  
  , test "first transition (see in the middle block) for replace"                           
    <| assertEqual (fstTransCfgForCheckR testMachine inputForRepl initHeadPos)                    
                   (fstTransCfgCorrectR testMachine inputForRepl)                     
  , test "second transition (see in the middle block) for replace"                          
    <| assertEqual (sndTransCfgForCheckR testMachine inputForRepl initHeadPos)                    
                   (sndTransCfgCorrectR testMachine inputForRepl)  
  , test "last for change"                                                                    
    <| assertEqual (lastCfgForCheck testMachine inputForCh initHeadPos) 
                   (lastCfgCorrectC testMachine inputForCh)
  , test "last for replace"                                                         
    <| assertEqual (lastCfgForCheck testMachine inputForRepl initHeadPos)                        
                   (lastCfgCorrectR testMachine inputForRepl)  
  , test "count for change"
    <| assertEqual (length (runRes testMachine inputForCh initHeadPos)) 7
  , test "count for replace"                                                     
    <| assertEqual (length (runRes testMachine inputForRepl initHeadPos)) 9
  ] 

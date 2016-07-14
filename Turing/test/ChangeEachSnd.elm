-- Change each second item (_ 0 0 0 0 _ -> _ 0 1 0 1 _)

module ChangeEachSnd exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)
import List exposing (head, tail, reverse, length)

import TuringTypes exposing (Machine, MachineCfg, TapeCfg, Direction(..), TransTable)
import RunTuring exposing (run, transFunc)
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
  [ { key = (0, Nothing), value = (0, Nothing, MoveRight)}                     
  , { key = (0, Just '0'), value = (1, Just '0', MoveRight)}                    
  , { key = (1, Just '0'), value = (2, Just '1', MoveRight)}
  , { key = (1, Nothing), value = (3, Nothing, MoveLeft)}
  , { key = (2, Just '0'), value = (1, Just '0', MoveRight)}
  , { key = (2, Nothing), value = (3, Nothing, MoveLeft)}
  ]                                                                             
                                                                                 
input : List (Maybe Char)                                                       
input =                                                                         
  [Nothing, Just '0', Just '0', Just '0', Just '0', Nothing]                                

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
  (head (runRes m inp))                                                    

headCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
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

-----------------------------------------------------------------------------
------------------------------------------------------------------------------


--LAST------------------------------------------------------------------------ 

-- Check the last MachineConfig in the list of configs 

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp =                                                         
  (head (reverse (runRes m inp)))                                     
  
lastCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrect m inp = Just (lastMCfg m inp)                                    

lastMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
lastMCfg m inp =                                                                
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             


lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =                                                                  
  { leftSyms = (fromList [Nothing, Just '0', Just '1', Just '0'])                            
  , currSym = Just '1'
  , rightSyms = (fromList [Nothing])
  }     

------------------------------------------------------------------------------
------------------------------------------------------------------------------

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual ( headCfgForCheck testMachine input ) 
                   ( headCfgCorrect testMachine input )
  , test "last"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine input ) 
                   ( lastCfgCorrect testMachine input )
  , test "count"
    <| assertEqual (length (runRes testMachine input)) 7
  ] 

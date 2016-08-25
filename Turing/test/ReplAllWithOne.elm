-- Replace all items with one item type (_ 1 2 3 _ -> _ 0 0 0 _)

module ReplAllWithOne exposing (tests)

import TuringMachine.TuringTypes exposing                                       
      (Machine, MachineCfg, TapeCfg, Direction(..), TransTable)                 
import TuringMachine.RunTuring exposing (runMachine, transFunc)                 
import TuringMachine.InitUpdate exposing (initMachineCfg) 

import ElmTest exposing (..)
import Array exposing (fromList, empty)
import List exposing (head, tail, reverse, length)


--TEST DATA------------------------------------------------------------------- 
testMachine : Machine Char Int                                                  
testMachine =                                                                   
  { transition = (transFunc transTable (3, Nothing, MoveLeft))                  
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0
  , startState  = 0                                                              
  , acceptState = 2 
  , rejectState = 3                                                             
  }                                                                             


transTable : TransTable Char Int                                                
transTable =                     
  fromList                                               
    [ { key = (0, Nothing), value  = (0, Nothing, MoveRight)}                     
    , { key = (0, Just '1'), value = (0, Just '0', MoveRight)}        
    , { key = (0, Just '2'), value = (0, Just '0', MoveRight)}
    , { key = (0, Just '3'), value = (1, Just '0', MoveRight)}
    , { key = (1, Nothing), value  = (2, Nothing, MoveLeft)} 
    ]                                                                             


input : List (Maybe Char)                                                       
input =                                                                         
  [Nothing, Just '1', Just '2', Just '3', Nothing]                                
------------------------------------------------------------------------------


--HELPERS---------------------------------------------------------------------  
                                                                                 
-- Common run function for all tests  
runRes : Machine Char Int -> List (Maybe Char) -> Int -> 
         List (MachineCfg Char Int)    
runRes m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)                                               
  in                                                                            
    (runMachine m init [init])                                                         

                                                                                 
--HEAD----------------------------------------------------------------------   

-- Check the first MachineConfig in the list of configs

headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int -> 
                  Maybe (MachineCfg Char Int)
headCfgForCheck m inp hpos =                                                         
  (head (runRes m inp hpos))                                                    


headCfgCorrect : Machine Char Int -> List (Maybe Char) -> 
                 Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)                                    


headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
headMCfg m inp =                                                                
  { currState = m.startState              
  , currDir   =  
  , tapeCfg   = (headTCfg inp)                                                    
  }                                                                             


headTCfg : List (Maybe Char) -> TapeCfg Char                                    
headTCfg inp =                                                                  
  { leftSyms = empty                                                     
  , currSym  =                                                                   
      case (head inp) of                                                   
        Just h  -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms =                                                                 
      case (tail inp) of                                                   
        Just t  -> (fromList t)                                            
        Nothing -> empty                                                  
  } 
-----------------------------------------------------------------------------


--LAST------------------------------------------------------------------------ 

-- Check the last MachineConfig in the list of configs 

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int -> 
                  Maybe (MachineCfg Char Int)
lastCfgForCheck m inp hpos =                                                         
  (head (reverse (runRes m inp hpos)))                                     


lastCfgCorrect : Machine Char Int -> List (Maybe Char) -> 
                 Maybe (MachineCfg Char Int)
lastCfgCorrect m inp = Just (lastMCfg m inp)                                    


lastMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
lastMCfg m inp =                                                                
  { currState = m.acceptState
  , currDir   =  
  , tapeCfg   = (lastTCfg inp)                                                    
  }                                                                             


lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =                                                                  
  { leftSyms  = (fromList [Nothing, Just '0', Just '0'])                            
  , currSym   = Just '0'
  , rightSyms = (fromList [Nothing])
  }     
------------------------------------------------------------------------------

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
    [ test "head"     
      <| assertEqual (headCfgForCheck testMachine input
                                         testMachine.initHeadPosForMach)   
                     (headCfgCorrect testMachine input)
    , test "last"                                                                    
      <| assertEqual (lastCfgForCheck testMachine input 
                                         testMachine.initHeadPosForMach)  
                     (lastCfgCorrect testMachine input)
    , test "count"
      <| assertEqual (length (runRes testMachine input 
                                      testMachine.initHeadPosForMach)) 6
    ] 

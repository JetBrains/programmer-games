-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)                                         
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

-- Check the last MachineConfig in the list of configs 

module LastTestCAR exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)

------------------------------------------------------------------------------

-- Just machine config (for check) 
lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp =   
  (List.head (List.reverse (runRes m inp)))


-- Just machine config (correct) 
lastCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrectC m inp = Just (lastMCfgC m inp)  


-- last machine config                                                          
lastMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
lastMCfgC m inp =                                                                
  { currState = m.acceptState
  , tapeCfg = (lastTCfgC inp)                                                    
  }                                                                             


-- last tape config                                                             
lastTCfgC : List (Maybe Char) -> TapeCfg Char                                    
lastTCfgC inp =          
    { leftSyms = (Array.fromList (List.take 2 inp))
    , currSym = Just '1'
    , rightSyms = (Array.fromList (List.drop 3 inp))
    }

------------------------------------------------------------------------------

-- Just machine config (correct)                                                
lastCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrectR m inp = Just (lastMCfgR m inp)                                   
                                                                                 
                                                                                 
-- last machine config                                                          
lastMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int        
lastMCfgR m inp =                                                               
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfgR inp)                                                    
  }                                                                             
                                                 

-- last tape config                                                             
lastTCfgR : List (Maybe Char) -> TapeCfg Char                                   
lastTCfgR inp =                                                                 
  { leftSyms = (Array.fromList (List.take 2 inp))                             
  , currSym = Just '1'
  , rightSyms = (Array.fromList ([Just '0'] ++ (List.drop 4 inp)) )          
  } 



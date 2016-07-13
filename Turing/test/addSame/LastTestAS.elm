-- Add same item (_ _ 0 _ _ -> _ _ 0 0 _)
-- Check the last MachineConfig in the list of configs 

module LastTestAS exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)


-- Just machine config (for check) 
lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp =   
  (List.head (List.reverse (runRes m inp)))


-- Just machine config (correct) 
lastCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrect m inp = Just (lastMCfg m inp)  


-- last machine config                                                          
lastMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
lastMCfg m inp =                                                                
  { currState = m.acceptState
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             


-- last tape config                                                             
lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =          
    { leftSyms = (Array.fromList (List.take 2 inp))
    , currSym = 
       case (List.head (List.drop 2 inp)) of
          Just c -> c
          Nothing -> Nothing
    , rightSyms = ( Array.fromList ([Just '0'] ++ (List.drop 4 inp)) )
    }

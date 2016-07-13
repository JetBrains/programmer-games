-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing 

-- Check the last MachineConfig in the list of configs 

module LastTestCW exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)
import CommonTypes exposing (..) 


-- Just machine config (for check) 
lastCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgForCheck m inp =   
  (List.head (List.reverse (runRes m inp)))


-- Just machine config (correct) 
lastCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
lastCfgCorrect m inp = Just (lastMCfg m inp)  


-- last machine config                                                          
lastMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten        
lastMCfg m inp =                                                                
  { currState = m.acceptState
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             


-- last tape config                                                             
lastTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                                    
lastTCfg inp =          
  { leftSyms = (Array.fromList [Just Blue])
  , currSym = 
      case (List.head inp) of
        Just c -> c
        Nothing -> Nothing
  , rightSyms = ( Array.fromList ([Just Red] ++ (List.drop 2 inp)) )
  }

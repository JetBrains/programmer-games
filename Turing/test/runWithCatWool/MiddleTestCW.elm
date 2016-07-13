-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing 

-- Check if go to state Grey when read first Nothing 
-- (only one transition for middle check here)

module MiddleTestCW exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)
import CommonTypes exposing (..) 


-- Just machine config (for check) 
fstTransCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)  
fstTransCfgForCheck m inp =                                                          
  (List.head (List.drop 2 (runRes m inp))) -- 3 config in list                                                
                                                                                  

-- Just machine config (correct) 
fstTransCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                                    
                                                                                 
                                                                                 
-- machine config                                                          
fstTransMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten
fstTransMCfg m inp =                                                                
  { currState = Grey                                                    
  , tapeCfg = (fstTransTCfg inp)                                                    
  }                                                                             
                                                                                 
                                                                                 
-- tape config                                                             
fstTransTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool                                    
fstTransTCfg inp =                                                                  
  { leftSyms = Array.empty                                             
  , currSym = 
      case (List.head inp) of                                                       
        Just h -> h                                                               
        Nothing -> Nothing                                                        
  , rightSyms = Array.fromList ([Just Red] ++ (List.drop 2 inp))
  }


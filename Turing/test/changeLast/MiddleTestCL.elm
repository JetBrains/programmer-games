-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)

-- Check if do transition in two important cases:                               
-- 1) go to state 1 when read first Just char                                   
-- 2) go to state 2 from state 1 when read Nothing   

module MiddleTestCL exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)


-- Just machine config (for check) 
fstTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheck m inp =                                                          
  (List.head (List.drop 2 (runRes m inp)))                                                 
                                                                                  

-- Just machine config (correct) 
fstTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                                    
                                                                                 
                                                                                 
-- middle machine config                                                          
fstTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
fstTransMCfg m inp =                                                                
  { currState = 1                                                    
  , tapeCfg = (fstTransTCfg inp)                                                    
  }                                                                             
                                                                                 
                                                                                 
-- middle tape config                                                             
fstTransTCfg : List (Maybe Char) -> TapeCfg Char                                    
fstTransTCfg inp =                                                                  
  { leftSyms = Array.fromList (List.take 2 inp)                                               
  , currSym = 
      case (List.head (List.drop 2 inp)) of                                                       
        Just h -> h                                                               
        Nothing -> Nothing                                                        
  , rightSyms = Array.fromList (List.drop 3 inp)
  }

------------------------------------------------------------------------------

-- Just machine config (for check)                                              
sndTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheck m inp =                                                   
  (List.head (List.drop 5 (runRes m inp))) -- 6 config in list
                                                                                 
                                                                                 
-- Just machine config (correct)                                                
sndTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrect m inp = Just (sndTransMCfg m inp)                        
                                                                                 
                                                                                 
-- middle machine config                                                          
sndTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int   
sndTransMCfg m inp =                                                          
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfg inp)                                              
  }                                                                             
                                                                                 
                                                                                 
-- middle tape config                                                             
sndTransTCfg : List (Maybe Char) -> TapeCfg Char                              
sndTransTCfg inp =                                                            
  { leftSyms = Array.fromList (List.take 3 inp)                                 
  , currSym =                                                                   
      case (List.head (List.drop 3 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  }     

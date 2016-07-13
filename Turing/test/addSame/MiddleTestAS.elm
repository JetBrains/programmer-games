-- Add same item (_ _ 0 _ _ -> _ _ 0 0 _) 

-- Check if do transition in two important cases: 
-- 1) go to state 1 when read Just '0'
-- 2) go to state 2 from state 1 when read Nothing after Just '0' 
--    (and replace this Nothing with new Just '0')


module MiddleTestAS exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)


-- Just machine config (for check) 
fstTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheck m inp =                                                          
  (List.head (List.drop 3 (runRes m inp))) -- 4 config in list                                               
                                                                                  

-- Just machine config (correct) 
fstTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                                    
                                                                                 
                                                                                 
-- machine config                                                          
fstTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
fstTransMCfg m inp =                                                                
  { currState = 1                                                    
  , tapeCfg = (fstTransTCfg inp)                                                    
  }                                                                             
                                                                                 
                                                                                 
-- tape config                                                             
fstTransTCfg : List (Maybe Char) -> TapeCfg Char                                    
fstTransTCfg inp =                                                                  
  { leftSyms = Array.fromList (List.take 3 inp)                                               
  , currSym = 
      case (List.head (List.drop 3 inp)) of                                                       
        Just h -> h                                                               
        Nothing -> Nothing                                                        
  , rightSyms = Array.fromList (List.drop 4 inp)
  }

------------------------------------------------------------------------------

-- Just machine config (for check)                                              
sndTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheck m inp =                                                   
  (List.head (List.drop 4 (runRes m inp))) -- 5 config in list
                                                                                 
                                                                                 
-- Just machine config (correct)                                                
sndTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrect m inp = Just (sndTransMCfg m inp)                        
                                                                                 
                                                                                 
-- machine config                                                          
sndTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int   
sndTransMCfg m inp =                                                          
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfg inp)                                              
  }                                                                             
                                                                                 
                                                                                 
-- tape config                                                             
sndTransTCfg : List (Maybe Char) -> TapeCfg Char                              
sndTransTCfg inp =                                                            
  { leftSyms = Array.fromList (List.take 2 inp)                                 
  , currSym =                                                                   
      case (List.head (List.drop 2 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = Array.fromList ([Just '0'] ++ (List.drop 4 inp))                       
  }     

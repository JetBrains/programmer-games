-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

-- Check if do transition in two important cases: 
-- 1) go to state 1 when read first Just c
--    (check that  first "Just c" change to the opposite value)
-- 2) go to state 2 from state 1 when read first Nothing after Just c 
--    (check that all of "Just c" change to the opposite values)

module MiddleTestCAR exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)

------------------------------------------------------------------------------

-- Just machine config (for check) 
fstTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckC m inp =                                                          
  (List.head (List.drop 3 (runRes m inp))) -- 4 config in list                                               
                                                                                  

-- Just machine config (correct) 
fstTransCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectC m inp = Just (fstTransMCfgC m inp)                                    
                                                                                 
                                                                                 
-- machine config                                                          
fstTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
fstTransMCfgC m inp =                                                                
  { currState = 1                                                    
  , tapeCfg = (fstTransTCfgC inp)                                                    
  }                                                                             
                                                                               

-- tape config                                                             
fstTransTCfgC : List (Maybe Char) -> TapeCfg Char                                    
fstTransTCfgC inp =                                                                  
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                                             
  , currSym = Nothing
  , rightSyms = Array.fromList (List.drop 4 inp)
  }

------------------------------------------------------------------------------

-- Just machine config (for check)                                              
sndTransCfgForCheckC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckC m inp =                                                   
  (List.head (List.drop 4 (runRes m inp))) -- 5 config in list
                                                                                 
                                                                                 
-- Just machine config (correct)                                                
sndTransCfgCorrectC : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectC m inp = Just (sndTransMCfgC m inp)                        
                                                                                 
                                                                                 
-- machine config                                                          
sndTransMCfgC : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int   
sndTransMCfgC m inp =                                                          
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgC inp)                                              
  }                                                                             
                                                                                 

-- tape config                                                             
sndTransTCfgC : List (Maybe Char) -> TapeCfg Char                              
sndTransTCfgC inp =                                                            
  { leftSyms = Array.fromList (List.take 2 inp)                                 
  , currSym = Just '1'
  , rightSyms = Array.fromList (List.drop 3 inp)                       
  }     

------------------------------------------------------------------------------  

-- Just machine config (for check)                                              
fstTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheckR m inp =                                                    
  (List.head (List.drop 3 (runRes m inp))) -- 4 config in list                                               
                                                                                 
                                                                                 
-- Just machine config (correct)                                                
fstTransCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrectR m inp = Just (fstTransMCfgR m inp)                           


-- machine config                                                               
fstTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
fstTransMCfgR m inp =                                                           
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfgR inp)                                                
  }                                                                             
                                                                                 

-- tape config                                                                  
fstTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
fstTransTCfgR inp =                                                             
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                 
  , currSym = Just '1'                                                           
  , rightSyms = Array.fromList (List.drop 4 inp)
  }                                                                              
                                                                                 
------------------------------------------------------------------------------  

------------------------------------------------------------------------------

-- Just machine config (for check)                                            
sndTransCfgForCheckR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheckR m inp =                                                    
  (List.head (List.drop 5 (runRes m inp))) -- 6 config in list                  

                                                                                 
-- Just machine config (correct)                                                
sndTransCfgCorrectR : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrectR m inp = Just (sndTransMCfgR m inp)                           


-- machine config                                                               
sndTransMCfgR : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int    
sndTransMCfgR m inp =                                                           
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfgR inp)                                                
  }                                                                             
                                                                                 
                                                                                 
-- tape config                                                                  
sndTransTCfgR : List (Maybe Char) -> TapeCfg Char                               
sndTransTCfgR inp =                                                             
  { leftSyms = Array.fromList ((List.take 2 inp) ++ [Just '1'])                         
  , currSym = Just '0'                                                          
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  } 

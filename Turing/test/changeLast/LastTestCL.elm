-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)
-- Check the last MachineConfig in the list of configs 

module LastTestCL exposing (..)

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


{-
-- get index for last number (start with 1)
indexOfLastNum : List (Maybe Char) -> Int -> Int
indexOfLastNum inp ind = 
  case (List.head inp) of
    Just h -> 
      case h of
        Just c ->
          case (List.head (List.drop 1 inp)) of
            Just hh -> 
              case hh of
                Just ch -> (indexOfLastNum (List.drop 1 inp) (ind+1))
                Nothing -> ind
            Nothing -> -1
        Nothing -> (indexOfLastNum (List.drop 1 inp) (ind+1))
    Nothing -> -1
-}


-- last tape config                                                             
lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =          
    { leftSyms = (Array.fromList (List.take 2 inp))
    , currSym = 
       case (List.head (List.drop 2 inp)) of
          Just sec -> sec
          Nothing -> Nothing
    , rightSyms = ( Array.fromList ([Just '1'] ++ (List.drop 4 inp)) )
    }

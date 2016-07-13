-- Add same item (_ _ 0 _ _ -> _ _ 0 0 _) 
-- Check the first MachineConfig in the list of configs
                                   
module HeadTestAS exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)


-- Just machine config (for check)
headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgForCheck m inp =
  (List.head (runRes m inp))


-- Just machine config (correct)
headCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)


-- head machine config
headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int
headMCfg m inp =
  { currState = m.startState 
  , tapeCfg = (headTCfg inp)
  }


-- head tape config
headTCfg : List (Maybe Char) -> TapeCfg Char
headTCfg inp =
  { leftSyms =  Array.empty
  , currSym = 
      case (List.head inp) of
        Just h -> h
        Nothing -> Nothing
  , rightSyms = 
      case (List.tail inp) of
        Just t -> (Array.fromList t)
        Nothing -> Array.empty

  }

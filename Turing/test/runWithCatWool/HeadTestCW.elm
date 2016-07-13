-- Check if module works with cat and wools correctly.
-- Must write Just Blue at the begin., Just Red instead of first Nothing

-- Check the first MachineConfig in the list of configs
                                   
module HeadTestCW exposing (..)

import Turing exposing (..)
import Array exposing (..)
import CommonRun exposing (..)
import CommonTypes exposing (..)


-- Just machine config (for check)
headCfgForCheck : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten)
headCfgForCheck m inp =
  (List.head (runRes m inp))


-- Just machine config (correct)
headCfgCorrect : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Maybe (MachineCfg BallOfWool Kitten) 
headCfgCorrect m inp = Just (headMCfg m inp)


-- head machine config
headMCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> MachineCfg BallOfWool Kitten 
headMCfg m inp =
  { currState = m.startState 
  , tapeCfg = (headTCfg inp)
  }


-- head tape config
headTCfg : List (Maybe BallOfWool) -> TapeCfg BallOfWool
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

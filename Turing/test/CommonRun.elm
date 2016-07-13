-- Common run function for all tests

module CommonRun exposing (..)
import Turing exposing (..)

runRes : Machine a b -> List (Maybe a) -> List (MachineCfg a b)
runRes m inp =
  let 
    init = (initMachineCfg m inp)
  in
    (run m init [init]) 

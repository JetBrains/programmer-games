module TuringMachine.RunTuring exposing (debugRun, runMachine, transFunc)
                               
import List exposing (map, take) 
import Array exposing (get, slice, length)
import String exposing (join)

import TuringMachine.TuringTypes exposing 
                           (Direction(..), Machine, MachineCfg, TransTable)
import TuringMachine.PrintTuring exposing (printMachineCfg) 
import TuringMachine.InitUpdate exposing (initMachineCfg, updateMachineCfg)                   


-- | Return true if the machine is in a final state.                            
machineCfgFinal : Machine a b -> MachineCfg a b -> Bool                         
machineCfgFinal m mcfg =                                                        
  mcfg.currState == m.acceptState || mcfg.currState == m.rejectState            


upLimitForCyclicalRun : Int
upLimitForCyclicalRun = 100


takeCfgsWhenCycle : Int
takeCfgsWhenCycle = 10


-- run machine while not in final state.                                        
runMachine : Machine a b -> MachineCfg a b -> List (MachineCfg a b) 
             -> List (MachineCfg a b)
runMachine m mcfg res =                                                                
  let                                                                           
    upd = (updateMachineCfg m mcfg)                                           
  in                                                                            
    if (machineCfgFinal m mcfg) 
       then res
    else 
      if (List.length res) > upLimitForCyclicalRun
         then (take takeCfgsWhenCycle res) 
      else (runMachine m upd (res ++ [upd]))                                          
                                                                                
                                                                                
-- | A transition function                                                      
transFunc : TransTable a b -> (b, Maybe a, Direction) -> (b, Maybe a) 
            -> (b, Maybe a, Direction)
transFunc tt def key =                                                          
  case (get 0 tt) of                                                             
    Just h -> if h.key == key                                                   
                 then h.value                                                   
              else                                                              
                 (transFunc (slice 1 (length tt) tt) def key)
    Nothing -> def                                                              
                                                                                
                                                                                
-- | Return all machine configs for given input word until final state.         
debugRun : Machine a b -> List (Maybe a) -> Int -> String                            
debugRun m w hpos =                                                                
  let                                                                           
    init = (initMachineCfg m w hpos)                                                 
  in                                                                            
    join " /// " (map printMachineCfg (runMachine m init [init]))             

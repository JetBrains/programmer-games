module RunTuring exposing (runMachine, run, transFunc)
                               
import List exposing (map, head, tail) 
import String exposing (join)
import TuringTypes exposing (Direction(..), Machine, MachineCfg, TransTable)
import PrintTuring exposing (printMachineCfg) 
import InitUpdate exposing (initMachineCfg, updateMachineCfg)                   


-- | Return true if the machine is in a final state.                            
machineCfgFinal : Machine a b -> MachineCfg a b -> Bool                         
machineCfgFinal m mcfg =                                                        
  mcfg.currState == m.acceptState || mcfg.currState == m.rejectState            
                                                                                
                                                                                
-- run machine while not in final state.                                        
run : Machine a b -> MachineCfg a b -> List (MachineCfg a b) -> List (MachineCfg a b)
run m mcfg res =                                                                
  let                                                                           
      upd = (updateMachineCfg m mcfg)                                           
  in                                                                            
     if (machineCfgFinal m mcfg) then res                                       
     else ( run m upd (res ++ [upd]) )                                          
                                                                                
                                                                                
-- | A transition function                                                      
transFunc : TransTable a b -> (b, Maybe a, Direction) -> (b, Maybe a) -> (b, Maybe a, Direction)
transFunc tt def key =                                                          
  case (head tt) of                                                             
    Just h -> if h.key == key                                                   
                 then h.value                                                   
              else                                                              
                case (tail tt) of                                               
                  Just t -> (transFunc t def key)                               
                  Nothing -> def                                                
    Nothing -> def                                                              
                                                                                
                                                                                
-- | Return all machine configs for given input word until final state.         
runMachine : Machine a b -> List (Maybe a) -> String                            
runMachine m w =                                                                
  let                                                                           
    init = (initMachineCfg m w)                                                 
  in                                                                            
    join " /// " (map printMachineCfg (run m init [init]))             
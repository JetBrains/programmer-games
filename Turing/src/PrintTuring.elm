module PrintTuring exposing (printMachineCfg)                   
                                                                                
import Array exposing (map, toList)
import String exposing (concat, join)
import TuringTypes exposing (TapeCfg, MachineCfg)


-- print tape --> return tape as a list of strings                              
printTapeCfg : TapeCfg a -> List String                                         
printTapeCfg {leftSyms, currSym, rightSyms} =                                   
  (["("] ++ toList (map toString leftSyms)) ++ [")"] ++                 
  ["["] ++ [toString currSym] ++ ["]"] ++                                       
  ["("] ++ (toList (map toString rightSyms) ++ [")"])                   
                                                                                
                                                                                
-- print machine --> return it as a string with the tape and the last state     
printMachineCfg : MachineCfg a b -> String                                      
printMachineCfg {currState, tapeCfg} =                                          
  concat [ join "  " (printTapeCfg tapeCfg),                             
           " <", (toString currState), ">"                                  
         ]                                                                  

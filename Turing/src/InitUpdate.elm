module InitUpdate exposing (initMachineCfg, updateMachineCfg)

import Array exposing (empty, fromList)
import TuringTypes exposing (Direction(..), Machine, TapeCfg, MachineCfg)
import UpdHelpers exposing (moveLeft, moveRight, getNewLeft, getNewRight, doTrans)       


-- | Replace symbol under tape head with new symbol, then move tape head.       
updateTapeCfg : TapeCfg a -> Maybe a -> Direction -> TapeCfg a                  
updateTapeCfg tcfg newSym dir =                                                 
  case dir of                                                                   
    MoveLeft ->                                                                 
      ( moveLeft tcfg.leftSyms (getNewRight tcfg.rightSyms newSym) )            
    MoveRight ->                                                                
      ( moveRight tcfg.rightSyms (getNewLeft tcfg.leftSyms newSym) )            
                                                                                
                                                                                
-- | Execute one transition step for given machine and config.                  
updateMachineCfg : Machine a b -> MachineCfg a b -> MachineCfg a b              
updateMachineCfg m mcfg =                                                       
  let                                                                           
    (newState, newSym, dir) = (doTrans m mcfg)                                  
  in                                                                            
    (updateTapeCfg mcfg.tapeCfg newSym dir)                                     
    |> MachineCfg newState                                                      
                                                                                
                                                                                
-- | Initialise tape with input word.                                           
initTapeCfg : List (Maybe a) -> TapeCfg a                                       
initTapeCfg w =                                                                 
  case w of                                                                     
    [] -> TapeCfg empty Nothing empty                                   
    (x::xs) -> TapeCfg empty x (fromList xs)                            
                                                                                
                                                                                
-- | Initialise machine config with input word.                                 
initMachineCfg : Machine a b -> List (Maybe a) -> MachineCfg a b                
initMachineCfg m input = MachineCfg (m.startState) (initTapeCfg input)          

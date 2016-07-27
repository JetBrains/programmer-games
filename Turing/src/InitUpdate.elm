module InitUpdate exposing (initMachineCfg, updateMachineCfg)

import Array exposing (empty, fromList)
import List exposing (take, drop, head)
import TuringTypes exposing (Direction(..), Machine, TapeCfg, MachineCfg)
import UpdHelpers exposing ( moveLeft, moveRight, getNewLeft, getNewRight
                           , doTrans)       


-- | Replace symbol under tape head with new symbol, then move tape head.       
updateTapeCfg : TapeCfg a -> Maybe a -> Direction -> TapeCfg a                  
updateTapeCfg tcfg newSym dir =                                                 
  case dir of                                                                   
    MoveLeft ->                                                                 
      ( moveLeft tcfg.leftSyms (getNewRight tcfg.rightSyms newSym) )            
    MoveRight ->                                                                
      ( moveRight tcfg.rightSyms (getNewLeft tcfg.leftSyms newSym) )            
    Stay -> 
      tcfg


-- | Execute one transition step for given machine and config.                  
updateMachineCfg : Machine a b -> MachineCfg a b -> MachineCfg a b              
updateMachineCfg m mcfg =                                                       
  let                                                                           
    (newState, newSym, dir) = (doTrans m mcfg)                                  
  in                                                                            
    (updateTapeCfg mcfg.tapeCfg newSym dir)                                     
    |> MachineCfg newState (dir)                                                     


getCurrSym : List (Maybe a) -> Int -> Maybe a
getCurrSym w hpos =
  case (head (drop hpos w)) of
    Nothing -> Nothing
    Just sym -> sym

-- | Initialise tape with input word.                                           
initTapeCfg : List (Maybe a) -> Int -> TapeCfg a                                       
initTapeCfg w hpos =                                                                 
  case w of                                                                     
    [] -> 
      TapeCfg empty Nothing empty                                   
    (x::xs) -> 
      if hpos == 0 then TapeCfg empty x (fromList xs)  
      else TapeCfg (fromList (take hpos w)) 
                   (getCurrSym w hpos) 
                   (fromList (drop (hpos+1) w)) 
                                                                                
                                                                                
-- | Initialise machine config with input word.    
initMachineCfg : Machine a b -> List (Maybe a) -> Int -> MachineCfg a b                
initMachineCfg m input hpos = MachineCfg (m.startState) 
                                         (Stay) 
                                         (initTapeCfg input hpos)          

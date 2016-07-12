module Turing exposing (..)

import Array as Arr exposing (..)
import String as Str exposing (..)
import List exposing (..) 


--TYPES BLOCK------------------------------------------------------------------
-------------------------------------------------------------------------------

-- | Tape head movement direction.
type Direction = MoveLeft | MoveRight


type alias Machine a b = 
  {
  transition    : (b, Maybe a) -> (b, Maybe a, Direction)
  , startState  : b
  , acceptState : b 
  , rejectState : b
  }


type alias TapeCfg a =                                                          
  { leftSyms  : Array (Maybe a)   --  symbols to the left of tape head                   
  , currSym   : Maybe a           --  symbol under the tape head                          
  , rightSyms : Array (Maybe a)   --  symbols to the right of tape head                 
  } 


type alias MachineCfg a b =
  { currState : b         -- current state of machine
  , tapeCfg   : TapeCfg a
  }


type alias KeyValue a b =
  { key : (b, Maybe a)
  , value : (b, Maybe a, Direction)
  }


type alias TransTable a b = 
  List (KeyValue a b)
------------------------------------------------------------------------------


--HELPERS TO UPDATE BLOCK-----------------------------------------------------
------------------------------------------------------------------------------

empty : Array (Maybe a)
empty = Arr.fromList [Nothing] 


getNewRight : Array (Maybe a) -> Maybe a -> Array (Maybe a)
getNewRight right sym =
  (Arr.append (Arr.repeat 1 sym) right)


getNewLeft : Array (Maybe a) -> Maybe a -> Array (Maybe a) 
getNewLeft left sym = 
  (Arr.push sym left)  


getLast : Array (Maybe a) -> Maybe a
getLast leftS =
  case (Arr.get ((Arr.length leftS)-1) leftS) of
    Just x -> x
    Nothing -> Nothing


withoutLast : Array (Maybe a) -> Array (Maybe a)
withoutLast leftS = 
  (Arr.slice 0 -1 leftS)


moveLeft : Array (Maybe a) -> Array (Maybe a) -> TapeCfg a 
moveLeft leftSyms right =
  if leftSyms == empty then (TapeCfg empty Nothing right)    
  else (TapeCfg (withoutLast leftSyms) (getLast leftSyms) right) 


getTail : Array (Maybe a) -> Array (Maybe a)
getTail rightS =
  (Arr.slice 1 (Arr.length rightS) rightS)


getFirst : Array (Maybe a) -> Maybe a
getFirst rightS =
  case (Arr.get 0 rightS) of
    Just x -> x
    Nothing -> Nothing


moveRight : Array (Maybe a) -> Array (Maybe a) -> TapeCfg a 
moveRight rightSyms left =
  if rightSyms == empty then (TapeCfg left Nothing empty)    
  else (TapeCfg left (getFirst rightSyms) (getTail rightSyms))    


doTrans : Machine a b -> MachineCfg a b -> (b, Maybe a, Direction)              
doTrans m {currState, tapeCfg} =                                                
  m.transition (currState, tapeCfg.currSym)  
------------------------------------------------------------------------------


--UPDATE BLOCK----------------------------------------------------------------
------------------------------------------------------------------------------

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


------------------------------------------------------------------------------


--INIT BLOCK------------------------------------------------------------------
------------------------------------------------------------------------------

-- | Initialise tape with input word.
initTapeCfg : List (Maybe a) -> TapeCfg a
initTapeCfg w = 
  case w of 
    -- if not Array.empty but Array.fromList [Nothing]
    -- [] -> TapeCfg Array.empty Nothing Array.empty 
    -- (x::xs) -> TapeCfg Array.empty x (Array.fromList xs)  
    [] -> TapeCfg empty Nothing empty
    (x::xs) -> TapeCfg empty x (Arr.fromList xs)


-- | Initialise machine config with input word.
initMachineCfg : Machine a b -> List (Maybe a) -> MachineCfg a b
initMachineCfg m input = MachineCfg (m.startState) (initTapeCfg input)
------------------------------------------------------------------------------


--PRINT BLOCK-----------------------------------------------------------------
------------------------------------------------------------------------------

-- print tape --> return tape as a list of strings                              
printTapeCfg : TapeCfg a -> List String                                         
printTapeCfg {leftSyms, currSym, rightSyms} =                                   
  (Arr.toList (Arr.map toString leftSyms)) ++ [toString currSym] ++       
  (Arr.toList (Arr.map toString rightSyms))                  


-- print machine --> return it as a string with the tape and the last state     
printMachineCfg : MachineCfg a b -> String                                      
printMachineCfg {currState, tapeCfg} =                                          
  Str.concat [ Str.join " " (printTapeCfg tapeCfg), 
               " current_machine_state: ", (toString currState),           
               " current_symbol_on_the_tape: ", (toString tapeCfg.currSym)]              
------------------------------------------------------------------------------


--RUN BLOCK-------------------------------------------------------------------
------------------------------------------------------------------------------

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
    Str.join " /// " (List.map printMachineCfg (run m init [init]))
------------------------------------------------------------------------------

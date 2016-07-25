module UpdHelpers exposing (moveLeft, moveRight, getNewLeft, getNewRight, doTrans)

import Array exposing (Array, append, repeat, push, get, length, slice, empty)
import TuringTypes exposing (Direction(..), Machine, TapeCfg, MachineCfg)


{-
( ) [ Just Red ] ( Just Yellow Just Green Just Blue ) <White> 
( Just Red ) [ Just Yellow ] ( Just Green Just Blue ) <White> 
( Just Red Just Yellow ) [ Just Green ] ( Just Blue ) <White> 
( Just Red Just Yellow Just Green ) [ Just Blue ] ( ) <White> 
( Just Red Just Yellow Just Green Just Blue ) [ Nothing ] ( ) <White> 
( Just Red Just Yellow Just Green ) [ Just Blue ] ( Just Red ) <LightGrey> 
( Just Red Just Yellow ) [ Just Green ] ( Just Blue Just Red ) <LightGrey> 
( Just Red ) [ Just Yellow ] ( Just Green Just Blue Just Red ) <LightGrey> 
( ) [ Just Red ] ( Just Yellow Just Green Just Blue Just Red ) <LightGrey> 
( ) [ Nothing ] ( Just Red Just Yellow Just Green Just Blue Just Red ) <LightGrey> 
( Just Blue ) [ Just Red ] ( Just Yellow Just Green Just Blue Just Red ) <Orange>
-}


getNewRight : Array (Maybe a) -> Maybe a -> Array (Maybe a)
getNewRight right sym =
  (append (repeat 1 sym) right)


getNewLeft : Array (Maybe a) -> Maybe a -> Array (Maybe a) 
getNewLeft left sym = 
  (push sym left)  


getLast : Array (Maybe a) -> Maybe a
getLast leftS =
  case (get ((length leftS)-1) leftS) of
    Just x -> x
    Nothing -> Nothing


withoutLast : Array (Maybe a) -> Array (Maybe a)
withoutLast leftS = 
  (slice 0 ((length leftS)-1) leftS)


moveLeft : Array (Maybe a) -> Array (Maybe a) -> TapeCfg a 
moveLeft leftSyms right =
  if leftSyms == empty then (TapeCfg empty Nothing right)    
  else (TapeCfg (withoutLast leftSyms) (getLast leftSyms) right) 


getTail : Array (Maybe a) -> Array (Maybe a)
getTail rightS =
  (slice 1 (length rightS) rightS)


getFirst : Array (Maybe a) -> Maybe a
getFirst rightS =
  case (get 0 rightS) of
    Just x -> x
    Nothing -> Nothing


moveRight : Array (Maybe a) -> Array (Maybe a) -> TapeCfg a 
moveRight rightSyms left =
  if rightSyms == empty then (TapeCfg left Nothing empty)    
  else (TapeCfg left (getFirst rightSyms) (getTail rightSyms))    


doTrans : Machine a b -> MachineCfg a b -> (b, Maybe a, Direction)              
doTrans m {currState, tapeCfg} =                                                
  m.transition (currState, tapeCfg.currSym)  

module TuringMachine.TuringTypes exposing (Direction(..), Machine, 
                                           TapeCfg, MachineCfg, 
                                           TransTable, UserTransTable, 
                                           UserKeyValue, KeyValue, 
                                           UserValue, Cell(..))

import Array exposing (Array)


-- | Tape head movement direction.                                              
type Direction = MoveLeft | MoveRight | Stay
                                                                                
                                                                                
type alias Machine a b =                                                        
  { transition  : (b, Maybe a) -> (b, Maybe a, Direction)                       
  , initHeadPosForDraw : Int
  , initHeadPosForMach : Int
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
  , currDir   : Direction
  , tapeCfg   : TapeCfg a                                                       
  }                                                                             
                                                                                

---------------------------------------------------------------
type alias KeyValue a b =                                                       
  { key : (b, Maybe a)                                                          
  , value : (b, Maybe a, Direction) 
  }                                                                             
                                                                                
                                                                                
type alias TransTable a b =                                                     
  Array (KeyValue a b)
---------------------------------------------------------------

---------------------------------------------------------------
--EmptyCell = ?, UserCell = user input, StableCell 
type Cell c = EmptyCell | UserCell c | StableCell c

type alias UserValue a b =
  { state : Cell (b)
  , symb : Cell (Maybe a)
  , dir : Cell (Direction) 
  }

type alias UserKeyValue a b =
  { key : (b, Maybe a)  
  , value : UserValue a b
  }

type alias UserTransTable a b = 
  Array (UserKeyValue a b)
---------------------------------------------------------------

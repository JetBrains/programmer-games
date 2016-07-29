module GameBase.Proccessing.TranslateTables exposing (getTrTFromUserTrT)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..), Model)
import TuringMachine.TuringTypes exposing ( Direction(..), Machine, TransTable, 
                                            TransTable, UserTransTable, 
                                            KeyValue, UserKeyValue, Cell(..))
import TuringMachine.RunTuring exposing (transFunc)

import Array exposing (push, get, slice, empty, length, isEmpty)


ifSymbNotEmpty : Cell (Maybe BallOfWool) -> Bool
ifSymbNotEmpty symb =
  case symb of
    EmptyCell -> False
    _ -> True


ifStateNotEmpty : Cell Kitten -> Bool
ifStateNotEmpty state =
  case state of                                                                  
    EmptyCell -> False   
    _ -> True


ifDirNotEmpty : Cell Direction -> Bool
ifDirNotEmpty dir =
  case dir of                                                                 
    EmptyCell -> False                                                       
    _ -> True  


ifValueNotEmpty : Maybe (UserKeyValue BallOfWool Kitten) -> Bool
ifValueNotEmpty maybeUserKV =
  case maybeUserKV of 
    Just userKV ->
      if (ifSymbNotEmpty userKV.value.symb) && 
         (ifStateNotEmpty userKV.value.state) && 
         (ifDirNotEmpty userKV.value.dir)  
         then True
      else False
    Nothing -> False


-- at the beginning res is True
checkIfTableFull : UserTransTable BallOfWool Kitten -> Bool -> Bool
checkIfTableFull userTable res = 
  let
    updRes = res && (ifValueNotEmpty (get 0 userTable))
    len = (length userTable)
  in
    if (isEmpty userTable) == False 
      then (checkIfTableFull (slice 1 len userTable) updRes) 
    else res


fromCellToSymb : Cell (Maybe BallOfWool) -> Maybe BallOfWool
fromCellToSymb cell =
 case cell of
   StableCell sym -> sym
   UserCell sym -> sym
   EmptyCell -> Nothing -- smth goes wrong


fromCellToState : Cell Kitten -> Kitten
fromCellToState cell =
  case cell of                                                                   
    StableCell st -> st                                                            
    UserCell st -> st
    EmptyCell -> Violet --smth does wrong


fromCellToDir : Cell Direction -> Direction
fromCellToDir cell =
  case cell of                                                                  
    StableCell dir -> dir  
    UserCell dir -> dir 
    EmptyCell -> MoveLeft -- smth goes wrong


translateOneValue : Maybe (UserKeyValue BallOfWool Kitten) -> 
                    KeyValue BallOfWool Kitten
translateOneValue maybeUserKV =
  case maybeUserKV of
    Just userKV -> { key = userKV.key                                                    
                   , value = ( (fromCellToState userKV.value.state)      
                             , (fromCellToSymb userKV.value.symb)    
                             , (fromCellToDir userKV.value.dir))                         
                   }
    Nothing -> { key = (Violet, Nothing) -- when we cant get head
               , value = (Violet, Nothing, MoveLeft)
               } 


-- at the beginning res is Array.empty
translateFullTable : UserTransTable BallOfWool Kitten -> 
                   TransTable BallOfWool Kitten -> TransTable BallOfWool Kitten                    
translateFullTable userTable res =
  let 
    updRes = push (translateOneValue (get 0 userTable)) res
    len = (length userTable)
  in
    if (isEmpty userTable) == False 
      then (translateFullTable (slice 1 len userTable) updRes)
    else res


getTrTFromUserTrT : Model -> Model
getTrTFromUserTrT m =
  let
    userTable = m.trTableUser
    transTable = (translateFullTable userTable empty)
  in
    if (checkIfTableFull userTable True) == True
      then { m | machine = { transition = (transFunc transTable 
                                                   (Violet, Nothing, MoveLeft)) 
                           , initHeadPosForDraw = m.machine.initHeadPosForDraw
                           , initHeadPosForMach = m.machine.initHeadPosForMach
                           , startState = m.machine.startState
                           , acceptState = m.machine.acceptState
                           , rejectState = m.machine.rejectState
                           } 
               , ifTableFull = True
           }
    else { m | ifTableFull = False } -- userTable is not full 

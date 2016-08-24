module GameBase.Proccessing.TranslateTables exposing (getTrTFromUserTrT, 
                                                      checkIfTableFull)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..), Model)
import TuringMachine.TuringTypes exposing (Direction(..), Machine, TransTable, 
                                           TransTable, UserTransTable, 
                                           KeyValue, UserKeyValue, Cell(..))
import TuringMachine.RunTuring exposing (transFunc)

import Array exposing (push, get, slice, empty, length, isEmpty)


ifSymbNotEmpty : Cell (Maybe BallOfWool) -> Bool
ifSymbNotEmpty symb =
  case symb of
    EmptyCell -> False
    _         -> True


ifStateNotEmpty : Cell Kitten -> Bool
ifStateNotEmpty state =
  case state of                                                                  
    EmptyCell -> False   
    _         -> True


ifDirNotEmpty : Cell Direction -> Bool
ifDirNotEmpty dir =
  case dir of                                                                 
    EmptyCell -> False                                                       
    _         -> True  


ifValueNotEmpty : Maybe (UserKeyValue BallOfWool Kitten) -> Bool
ifValueNotEmpty maybeUserKV =
  case maybeUserKV of 
    Just userKV ->
      if (ifSymbNotEmpty userKV.value.symb)   == True && 
         (ifStateNotEmpty userKV.value.state) == True && 
         (ifDirNotEmpty userKV.value.dir)     == True 
         then True
      else False
    Nothing -> False


-- at the beginning res is True
checkIfTableFull : UserTransTable BallOfWool Kitten -> Bool -> Bool
checkIfTableFull userTable res = 
  let
    updRes = res && (ifValueNotEmpty (get 0 userTable))
    len    = (length userTable)
  in
    if (isEmpty userTable) == False 
      then (checkIfTableFull (slice 1 len userTable) updRes) 
    else res


fromCellToSymb : Cell (Maybe BallOfWool) -> Maybe BallOfWool
fromCellToSymb cell =
 case cell of
   StableCell sym -> sym
   UserCell sym   -> sym
   EmptyCell      -> Nothing -- smth goes wrong


fromCellToState : Cell Kitten -> Kitten
fromCellToState cell =
  case cell of                                                                   
    StableCell st -> st                                                            
    UserCell st   -> st
    EmptyCell     -> Violet -- smth does wrong


fromCellToDir : Cell Direction -> Direction
fromCellToDir cell =
  case cell of                                                                  
    StableCell dir -> dir  
    UserCell dir   -> dir 
    EmptyCell      -> MoveLeft -- smth goes wrong


translateOneValue : Maybe (UserKeyValue BallOfWool Kitten) -> 
                    KeyValue BallOfWool Kitten
translateOneValue maybeUserKV =
  case maybeUserKV of
    Just userKV -> { key   = userKV.key                                                    
                   , value = ( (fromCellToState userKV.value.state)      
                             , (fromCellToSymb userKV.value.symb)    
                             , (fromCellToDir userKV.value.dir))                         
                   }
    Nothing     -> { key = (Violet, Nothing) -- when we cant get head
                   , value = (Violet, Nothing, MoveLeft)
                   } 


-- at the beginning res is Array.empty
translateFullTable : UserTransTable BallOfWool Kitten -> 
                 TransTable BallOfWool Kitten -> TransTable BallOfWool Kitten                    
translateFullTable userTable res =
  let 
    updRes = push (translateOneValue (get 0 userTable)) res
    len    = (length userTable)
  in
    if (isEmpty userTable) == False 
      then (translateFullTable (slice 1 len userTable) updRes)
    else res


getTrTFromUserTrT : Model -> Model
getTrTFromUserTrT m =
  let
    userTable  = m.transTables.trTableUser
    transTable = (translateFullTable userTable empty)
  in
    if (checkIfTableFull userTable True) == True
      then { m 
              | modelMachine = 
                  { input   = m.modelMachine.input                                    
                  , machine = 
                      { transition = (transFunc transTable                  
                                      (Violet, Nothing, MoveLeft))              
                      , initHeadPosForDraw =                                
                          m.modelMachine.machine.initHeadPosForDraw         
                      , initHeadPosForMach =                                
                          m.modelMachine.machine.initHeadPosForMach         
                      , startState  =                                        
                          m.modelMachine.machine.startState                 
                      , acceptState =                                       
                          m.modelMachine.machine.acceptState                
                      , rejectState =                                       
                          m.modelMachine.machine.rejectState                
                      }    
                  , machineCfgs = m.modelMachine.machineCfgs  
                  }
              , flags = 
                  { ifPushRun   = m.flags.ifPushRun
                  , ifStart     = m.flags.ifStart
                  , ifPlay      = m.flags.ifPlay
                  , ifHistory   = m.flags.ifHistory
                  , ifRules     = m.flags.ifRules                                         
                  , ifAuthors   = m.flags.ifAuthors                                               
                  , ifEnd       = m.flags.ifEnd                                  
                  , ifCatLooks  = m.flags.ifCatLooks                                                
                  , ifTableFull = True 
                  }
          }
      else { m 
              | flags = 
                  { ifPushRun   = m.flags.ifPushRun                               
                  , ifStart     = m.flags.ifStart                                   
                  , ifPlay      = m.flags.ifPlay                                     
                  , ifHistory   = m.flags.ifHistory 
                  , ifRules     = m.flags.ifRules                                       
                  , ifAuthors   = m.flags.ifAuthors                                   
                  , ifEnd       = m.flags.ifEnd                                       
                  , ifCatLooks  = m.flags.ifCatLooks                                 
                  , ifTableFull = False
                  } 
            }

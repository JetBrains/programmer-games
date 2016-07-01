module Turing exposing (..)

import Html exposing (Html, div, button, text)
import Html.Events exposing (onClick)
import Html.App
import Array exposing (..)
import String exposing (..)
import List exposing (..)

-- output must be S10011E q2 

type alias State = Int 
type alias Symbol = Char

-- | Tape head movement direction.
type Direction = MoveLeft | MoveRight

type alias Machine = 
        {
        -- | @'transition' s smb@ defines the behaviour of the machine when it
        -- is in state @s@ and @smb@ is the symbol under the tape head.
        -- The function returns the next state of the machine,
        -- the symbol overwriting the current symbol under the tape head,
        -- and the direction in which the tape head should move after overwriting.

        -- If @smb@ is @'Just' x@, that signifies that @x@ is the current symbol
        -- under the tape head.
        -- If @smb@ is 'Nothing', that means that the tape head is over a
        -- tape position that has not been initialised yet.

        transition    : State -> Maybe Symbol -> (State, Symbol, Direction)
        , startState  : State
        , acceptState : State  -- принимающее состояние
        , rejectState : State  -- состояние отклонения
        }


type alias TapeCfg = 
        { leftSyms  : Array Symbol  -- ^ symbols to the left of tape head
        , currSym   : Maybe Symbol  -- ^ symbol under the tape head
        , rightSyms : Array Symbol  -- ^ symbols to the right of tape head
        }       


type alias MachineCfg =
        { currState : State     -- current state of machine
        , tapeCfg   : TapeCfg
        }


-- | Replace symbol under tape head with new symbol, then move tape head.
updateTapeCfg : TapeCfg -> Symbol -> Direction -> TapeCfg
updateTapeCfg {leftSyms, currSym, rightSyms} newSym dir =
        case dir of     
                MoveLeft ->
                        let 
                            right = (Array.append (Array.repeat 1 newSym) rightSyms) --Add an element to the left end of a sequence to the right of tape head 
                        in
                            if (Array.isEmpty leftSyms) then (TapeCfg empty Nothing right) 
                            else (TapeCfg (Array.slice 0 -1 leftSyms) (Array.get ((Array.length leftSyms)-1) leftSyms) right)
                MoveRight ->
                        let
                            left = (Array.push newSym leftSyms) -- Add an element to the right end of a sequence to the left of tape head
                        in
                             if (Array.isEmpty rightSyms) then (TapeCfg left Nothing empty)
                             else (TapeCfg left (Array.get 0 rightSyms) (Array.slice 1 (Array.length rightSyms) rightSyms))


-- | Execute one transition step for given machine and config.
updateMachineCfg : Machine -> MachineCfg -> MachineCfg
updateMachineCfg m {currState, tapeCfg} =
        let (state', newSym, dir) = m.transition currState tapeCfg.currSym
        in (updateTapeCfg tapeCfg newSym dir)
           |> MachineCfg state' -- f <| x == f x  x |> f == f x


-- | Initialise tape with input word.
initTapeCfg : List Symbol -> TapeCfg
initTapeCfg w = 
        case w of 
                [] -> TapeCfg Array.empty Nothing Array.empty
                (x::xs) -> TapeCfg Array.empty (Just x) (Array.fromList xs)  

{-
initTapeCfg w = 
        case (tail w) of 
                Just l -> TapeCfg empty (head w) (fromList l) -- tail return Maybe list
                Nothing -> TapeCfg empty (head w) empty
-}


-- | Initialise machine config with input word.
initMachineCfg : Machine -> List Symbol -> MachineCfg
-- init tape with input word and define current state of machine with start state
initMachineCfg m input = MachineCfg (m.startState) (initTapeCfg input)


-- | Return true if the machine is in a final state.
machineCfgFinal : Machine -> MachineCfg -> Bool
machineCfgFinal m mcfg =
        mcfg.currState == m.acceptState || mcfg.currState == m.rejectState


{-
-- variant of run with List MachineCfg return value (all configs)
run : Machine -> MachineCfg -> List MachineCfg -> List MachineCfg
run m mcfg res = 
        if (machineCfgFinal m mcfg) then res
        else (run m (updateMachineCfg m mcfg) (res ++ mcfg::[]))  
-}


-- run machine while not in final state. return the last machine configuration (state+tape)
run : Machine -> MachineCfg -> MachineCfg
run m mcfg = 
        if (machineCfgFinal m mcfg) then mcfg
        else (run m (updateMachineCfg m mcfg))  


-- print tape --> return tape as a list of symbols
printTapeCfg : TapeCfg -> List Symbol
printTapeCfg {leftSyms, currSym, rightSyms} = 
        case currSym of
                Just c -> (Array.toList leftSyms) ++ [c] ++ (Array.toList rightSyms) ++ [' ']
                Nothing -> (Array.toList leftSyms) ++ [' '] ++ (Array.toList rightSyms) ++ [' ']


-- print machine ---> return it as a string with the tape and the last state
printMachineCfg : MachineCfg -> String 
printMachineCfg {currState, tapeCfg} =
        String.concat [String.fromList (printTapeCfg tapeCfg), "q", (toString currState), " "]


{-
-- init and run the machine ---> get list of strings-machine configurations
runMachine : Machine -> List Symbol -> List String
runMachine m w =
        List.map printMachineCfg (run m (initMachineCfg m w) []) -- map : (a -> b) -> List a -> List b
-}


-- | Return the last machine config for given input word until final state.
runMachine : Machine -> List Symbol -> String
runMachine m w = 
        printMachineCfg (run m (initMachineCfg m w)) 


-- | A test Turing machine 
testMachine : Machine
testMachine =
        { transition = t
        , startState = 0
        , acceptState = 2
        , rejectState = 3
        } 


t : State -> Maybe Symbol -> (State, Symbol, Direction)
t st sm = 
        case st of
                0 -> case sm of
                        Just x -> (0, x, MoveRight)
                        Nothing -> (1, 'E', MoveLeft)
                1 -> case sm of
                        Just x -> (1, x, MoveLeft)
                        Nothing -> (2, 'S', MoveRight)
                _ -> (3, 'R', MoveLeft)
                                                                                                                                            

{-
-- for variant with print all configurations
main = 
        text (String.concat (runMachine testMachine ['1', '0', '0', '1', '1']))
-}

main = 
        text (runMachine testMachine ['1', '0', '0', '1', '1'])


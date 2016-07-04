import Html exposing (Html, div, button, text)
import Turing exposing(..)


-- Symbol is a ball of wool with special color
-- Tape head is a kitten
type BallOfWool = Red | Orange | Yellow | Green | Blue 
type Kitten = Grey | Black | Brown | LightGrey


-- | A test Turing machine 
testMachine =
        { transition = t
        , startState = LightGrey -- = 0 
        , acceptState = Brown  -- = 2
        , rejectState = Black  -- = 3
        }
 
t : Kitten -> Maybe BallOfWool -> (Kitten, BallOfWool, Turing.Direction)
t st sm =
        case st of
                LightGrey -> case sm of
                        Just x -> (LightGrey, x, MoveRight)
                        Nothing -> (Grey, Red, Turing.MoveLeft)
                Grey -> case sm of
                        Just x -> (Grey, x, Turing.MoveLeft)
                        Nothing -> (Brown, Blue, Turing.MoveRight)
                _ -> (Black, Yellow, Turing.MoveLeft)

main =
        text (runMachine testMachine [Red, Orange, Yellow, Green, Blue])

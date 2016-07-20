import Html exposing (Html, div, button, text)
import Turing exposing(..)

type BallOfWool = Red | Orange | Yellow | Green | Blue | DarkGrey
type Kitten = Grey | Black | Brown | LightGrey

testMachine =
  { transition = (transFunc transTable (Black, DarkGrey, MoveLeft))
  , startState = LightGrey 
  , acceptState = Brown 
  , rejectState = Black
  }

transTable = 
  [ { key = (LightGrey, Just Red), value = (LightGrey, Red, MoveRight)}
  , { key = (LightGrey, Just Orange), value = (LightGrey, Orange, MoveRight)}
  , { key = (LightGrey, Just Yellow), value = (LightGrey, Yellow, MoveRight)}
  , { key = (LightGrey, Just Green), value = (LightGrey, Green, MoveRight)}
  , { key = (LightGrey, Just Blue), value = (LightGrey, Blue, MoveRight)}
  , { key = (LightGrey, Nothing), value = (Grey, Red, MoveLeft)}
  , { key = (Grey, Just Red), value = (Grey, Red, MoveLeft)}
  , { key = (Grey, Just Orange), value = (Grey, Orange, MoveLeft)}
  , { key = (Grey, Just Yellow), value = (Grey, Yellow, MoveLeft)}
  , { key = (Grey, Just Green), value = (Grey, Green, MoveLeft)}
  , { key = (Grey, Just Blue), value = (Grey, Blue, MoveLeft)}
  , { key = (Grey, Nothing), value = (Brown, Blue, MoveRight)}
  ] 

main =
  text (runMachine testMachine [Red, Orange, Yellow, Green, Blue])

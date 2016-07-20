import Html exposing (text)
import TuringTypes exposing (Direction(..), Machine, TransTable)
import RunTuring exposing (debugRun, transFunc)                          


type BallOfWool = Red | Orange | Yellow | Green | Blue -- a
type Kitten = Grey | Black | Brown | LightGrey -- b


testMachine : Machine BallOfWool Kitten
testMachine =
  { transition = (transFunc transTable (Black, Nothing, MoveLeft))
  , startState = LightGrey 
  , acceptState = Brown 
  , rejectState = Black
  }


transTable : TransTable BallOfWool Kitten
transTable = 
  [ { key = (LightGrey, Just Red), 
      value = (LightGrey, Just Red, MoveRight)}
  , { key = (LightGrey, Just Orange), 
      value = (LightGrey, Just Orange, MoveRight)}
  , { key = (LightGrey, Just Yellow), 
      value = (LightGrey, Just Yellow, MoveRight)}
  , { key = (LightGrey, Just Green), 
      value = (LightGrey, Just Green, MoveRight)}
  , { key = (LightGrey, Just Blue), 
      value = (LightGrey, Just Blue, MoveRight)}
  , { key = (LightGrey, Nothing), 
      value = (Grey, Just Red, MoveLeft)}
  , { key = (Grey, Just Red), 
      value = (Grey, Just Red, MoveLeft)}
  , { key = (Grey, Just Orange), 
      value = (Grey, Just Orange, MoveLeft)}
  , { key = (Grey, Just Yellow), 
      value = (Grey, Just Yellow, MoveLeft)}
  , { key = (Grey, Just Green), 
      value = (Grey, Just Green, MoveLeft)}
  , { key = (Grey, Just Blue), 
      value = (Grey, Just Blue, MoveLeft)}
  , { key = (Grey, Nothing), 
      value = (Brown, Just Blue, MoveRight)}
  ] 


input : List (Maybe BallOfWool)
input = 
  [Just Red, Nothing, Just Orange, Just Yellow, Just Green, Just Blue]


-- initial position of TM head (0 means 'above the first element')
initHeadPos : Int
initHeadPos = 3


main =
  text (debugRun testMachine input initHeadPos)

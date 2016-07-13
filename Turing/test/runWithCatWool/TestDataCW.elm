-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing 

-- Test Data for tests (machine, transition table and input word)

module TestDataCW exposing (..)
import Turing exposing (..)
import CommonTypes exposing (..)


testMachine : Machine BallOfWool Kitten                                         
testMachine =                                                                   
  { transition = (transFunc transTable (Black, Nothing, MoveLeft))              
  , startState = LightGrey                                                      
  , acceptState = Brown                                                         
  , rejectState = Black                                                         
  }                                                                             
  

transTable : TransTable BallOfWool Kitten                                       
transTable =                                                                    
  [ { key = (LightGrey, Just Red), value = (LightGrey, Just Red, MoveRight)}    
  , { key = (LightGrey, Nothing), value = (Grey, Just Red, MoveLeft)}           
  , { key = (Grey, Just Red), value = (Grey, Just Red, MoveLeft)}               
  , { key = (Grey, Nothing), value = (Brown, Just Blue, MoveRight)}             
  ]                                                                             


input : List (Maybe BallOfWool)                                                 
input =                                                                         
  [Just Red, Nothing, Just Orange, Just Yellow, Just Green, Just Blue]              

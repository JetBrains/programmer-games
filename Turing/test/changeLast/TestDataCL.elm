-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)
-- Test Data for tests (machine, transition table and input word)

module TestDataCL exposing (..)
import Turing exposing (..)

testMachine : Machine Char Int
testMachine =                                                                   
  { transition = (transFunc transTable (4, Nothing, MoveLeft))              
  , startState = 0
  , acceptState = 3
  , rejectState = 4
  }                                                                             


transTable : TransTable Char Int
transTable =                                                                    
  [ { key = (0, Nothing),  value = (0, Nothing, MoveRight)} 
  , { key = (0, Just '0'), value = (1, Just '0', MoveRight)}    
  , { key = (1, Nothing),  value = (2, Nothing, MoveLeft)}     
  , { key = (1, Just '0'), value = (1, Just '0', MoveRight)}
  , { key = (2, Just '0'), value = (3, Just '1', MoveLeft)}
  ]   


input : List (Maybe Char)                                                      
input =                                                                        
  [Nothing, Just '0', Just '0', Just '0', Nothing]

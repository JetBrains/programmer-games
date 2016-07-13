-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

-- Test data for all tests (machine, transition table, input)

module TestDataCAR exposing (..)
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
  , { key = (0, Just '0'), value = (1, Just '1', MoveRight)}                    
  , { key = (0, Just '1'), value = (1, Just '0', MoveRight)}                    
  , { key = (1, Nothing),  value = (2, Nothing, MoveLeft)}                      
  , { key = (1, Just '0'), value = (1, Just '1', MoveRight)}                    
  , { key = (1, Just '1'), value = (1, Just '0', MoveRight)}                    
  , { key = (2, Just '0'), value = (2, Just '0', MoveLeft)}                     
  , { key = (2, Just '1'), value = (2, Just '1', MoveLeft)}                     
  , { key = (2, Nothing), value = (3, Nothing, MoveRight)}                      
  ]                                                                             


inputForCh : List (Maybe Char)                                                      
inputForCh =                                                                        
  [Nothing, Nothing, Just '0', Nothing, Nothing]                                


inputForRepl : List (Maybe Char)                                                      
inputForRepl =                                                                        
  [Nothing, Nothing, Just '0', Just '1', Nothing] 

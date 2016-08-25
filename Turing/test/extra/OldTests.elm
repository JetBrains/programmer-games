module OldTests exposing (..)

import ElmTest exposing (..)
import Turing exposing (..)


testMachine3 =                                                                  
  { transition = t3                                                          
  , startState = 0                                                              
  , acceptState = 0                                                             
  , rejectState = 1                                                             
  }  

t3 : Int -> Maybe Char -> (Int, Char, Direction)                             
t3 st sm =                                                                   
    case st of                                                                  
      0 ->                                                                      
        case sm of                                                              
          Just '0' -> (0, '1', MoveRight)                                       
          Just '1' -> (0, '0', MoveRight)                                       
          Nothing  -> (1, ' ', MoveLeft)                                        
          Just _ -> (1, '_', MoveLeft) 
      _ -> (1, '_', MoveLeft)


testMachine4 =                                                                  
  { transition = t4                                                             
  , startState = 0                                                              
  , acceptState = 1                                                             
  , rejectState = 2                                                             
  }                                                                             


t4 : Int -> Maybe Char -> (Int, Char, Direction)                                
t4 st sm =                                                                      
  case st of                                                                  
    0 ->                                                                      
      case sm of                                                              
        Nothing -> (1, '*', MoveLeft)                                        
        Just '0' -> (1, '0', MoveLeft)
        Just y -> (2, y, MoveLeft)
    _ -> (2, 'E', MoveLeft)


testMachine5 =                                                                  
  { transition = t5                                                             
  , startState = 0                                                              
  , acceptState = 5                                                           
  , rejectState = 6                                                             
  }                                                                             


t5 : Int -> Maybe Char -> (Int, Char, Direction)                                
t5 st sm =                                                                      
  case st of                                                                  
    0 ->                                                                      
      case sm of                                                              
        Just '0' -> (0, '0', MoveRight)                                       
        Just '1' -> (1, '0', MoveRight)                                       
        Nothing  -> (5, '_', MoveLeft)                                        
        Just '_' -> (5, '_', MoveLeft)
        Just y   -> (6, '!', MoveLeft)                                          
    1 ->                                                                      
      case sm of                                                              
        Just '0' -> (1, '0', MoveRight)                                        
        Just '1' -> (1, '1', MoveRight)                                        
        Nothing  -> (2, '_' , MoveRight)
        Just '_' -> (2, '_' , MoveRight)
        Just y   -> (6, '!', MoveLeft)
    2 ->
      case sm of
        Just '1' -> (2, '1', MoveRight)                                          
        Nothing  -> (3, '1' , MoveLeft)  
        Just '_' -> (3, '1' , MoveLeft)
        Just y   -> (6, '!', MoveLeft)
    3 -> 
      case sm of
        Just '1' -> (3, '1', MoveLeft)                                         
        Just '_' -> (4, '_' , MoveLeft)
        Just y   -> (6, '!', MoveLeft)
        Nothing  -> (6, '!' , MoveLeft)
    4 ->
      case sm of
        Just '0' -> (4, '0', MoveLeft)
        Just '1' -> (4, '1', MoveLeft)                                         
        Nothing  -> (0, '_' , MoveRight)                                         
        Just '_' -> (0, '_', MoveRight)
        Just y   -> (6, '!', MoveLeft)
    _ -> (6, '!' , MoveLeft)


testMachine6 =                                                                  
  { transition = t6                                                             
  , startState = 0                                                              
  , acceptState = 2                                                             
  , rejectState = 3                                                             
  }                                                                             


t6 : Int -> Maybe Char -> (Int, Char, Direction)                                
t6 st sm =                                                                      
  case st of                                                                    
    0 ->                                                                        
      case sm of 
        Just y -> (0, y, MoveRight)
        Nothing -> (1, ' ', MoveLeft)
    1 ->
      case sm of
        Just '0' -> (2, '1', MoveLeft)
        Just '1' -> (2, '2', MoveLeft)
        Just '2' -> (2, '3', MoveLeft)
        Just '3' -> (2, '4', MoveLeft)
        Just '4' -> (2, '5', MoveLeft)
        Just '5' -> (2, '6', MoveLeft)
        Just '6' -> (2, '7', MoveLeft)
        Just '7' -> (2, '8', MoveLeft)
        Just '8' -> (2, '9', MoveLeft)
        Just '9' -> (1, '0', MoveLeft)
        Nothing  -> (2, '1', MoveLeft)
        Just x   -> (3, '!', MoveLeft)
    _ -> (3, '!', MoveLeft) 


tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [
    -- Check if startState = acceptState
     test "3"
    <| assertEqual (runMachine testMachine3 ['0', '0', '1', '1', '0', '1', '1']) 
    "'0''0''1''1''0''1''1' q0"
    -- Check if work with empty input, write 1 symb and end
    , test "4"
    <| assertEqual (runMachine testMachine4 []) "'*' q1"
    -- Check if work with one-symbol input
    -- in initTapeCfg line (x::xs) -> ...(Array.fromList xs) when xs is empty
    , test "5"
    <| assertEqual (runMachine testMachine4 ['0']) "'0' q1" 
    -- Check if stop when get rejuct state
    , test "6"
    <| assertEqual (runMachine testMachine4 ['1']) "'1' q2"
    -- Check get 000000000000 1111111 from 001101011101
    , test "8"
    <| assertEqual (runMachine testMachine5 ['0','0','1','1','0','1','0','1',
    '1','1','0','1']) "'_''0''0''0''0''0''0''0''0''0''0''0''0''_''1''1''1''1''1''1''1' q5"
    -- Check if machine can do +1 to the number on the tape
    , test "9"
    <| assertEqual (runMachine testMachine6 ['1','2','3']) "'1''2''4'' ' q2"
    -- Check the previous example with another input word
    , test "10"
    <| assertEqual (runMachine testMachine6 ['9','9','9']) "'1''0''0''0'' ' q2"
  ] 






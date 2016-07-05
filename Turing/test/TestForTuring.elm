module TestForTuring exposing (..)

import Html exposing (Html, div, button, text)
import ElmTest exposing (..)
import Turing exposing (..)


-- Symbol is a ball of wool with special color                                  
-- Tape head is a kitten                                                        
type BallOfWool = Red | Orange | Yellow | Green | Blue                          
type Kitten = Grey | Black | Brown | LightGrey                                  

testMachine1 =                                                                   
  { transition = t1                                                        
  , startState = LightGrey                                          
  , acceptState = Brown                                            
  , rejectState = Black                                          
  }                                                                       


t1 : Kitten -> Maybe BallOfWool -> (Kitten, BallOfWool, Turing.Direction)       
t1 st sm =                                                                       
  case st of                                                              
    LightGrey -> case sm of                                         
      Just x -> (LightGrey, x, MoveRight)                     
      Nothing -> (Grey, Red, Turing.MoveLeft)                
    Grey -> case sm of                                              
      Just x -> (Grey, x, Turing.MoveLeft)                   
      Nothing -> (Brown, Blue, Turing.MoveRight)             
    _ -> (Black, Yellow, Turing.MoveLeft)                          

                                                                              
testMachine2 =                                                                   
  { transition = t2                                                    
  , startState = 0                                                        
  , acceptState = 2                                                       
  , rejectState = 3                                                       
  }                                                                       


t2 : Int -> Maybe Char -> (Int, Char, Direction)                     
t2 st sm =                                                                   
    case st of                                                               
      0 ->                                                                  
        case sm of                                                         
          Just '0' -> (0, '1', MoveRight)                               
          Just '1' -> (0, '0', MoveRight)                               
          Nothing  -> (1, '*', MoveLeft)                                
          Just _ -> (1, '_', MoveLeft)                                  
      1 ->                                                                  
        case sm of                                                         
          Just '0' -> (1, '0', MoveLeft)                                
          Just '1' -> (1, '1', MoveLeft)                                
          Nothing  -> (2, '+' , MoveRight)                              
          Just _ -> (2, '_', MoveRight)                                 
      _ -> (3, '_' , MoveRight) 


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
        Just y -> (1, y, MoveLeft)                                          
    _ -> (2, 'E', MoveLeft)


tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ -- Check if work with colors. Must write Blue at the begin., Red at the end
    test "1"                                                       
    <| assertEqual (runMachine testMachine1 [Red, Orange, Yellow, Green, Blue]) 
    "BlueRedOrangeYellowGreenBlueRed qBrown" 
    -- Check with example on change numbers
    ,test "2"
    <| assertEqual (runMachine testMachine2 ['0', '0', '1', '1', '0', '1', '1']) 
    "'+''1''1''0''0''1''0''0''*' q2"
    -- Check if startState = acceptState
    , test "3"
    <| assertEqual (runMachine testMachine3 ['0', '0', '1', '1', '0', '1', '1']) 
    "'0''0''1''1''0''1''1' q0"
    -- Check if work with empty input, write 1 symb and end
    , test "4"
    <| assertEqual (runMachine testMachine4 [])
    "'*' q1"
  ] 






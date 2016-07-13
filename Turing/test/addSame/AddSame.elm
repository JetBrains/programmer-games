-- Add same item (_ _ 0 _ _ -> _ _ 0 0 _) 
-- Test suite that contains all tests (head, middle, last, count)

module AddSame exposing (..)

import ElmTest exposing (..)

import TestDataAS exposing (..)
import CommonRun exposing (..)
import HeadTestAS exposing (..)
import MiddleTestAS exposing (..)
import LastTestAS exposing (..)


tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual ( headCfgForCheck testMachine input ) 
                   ( headCfgCorrect testMachine input )
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual ( fstTransCfgForCheck testMachine input )                       
                   ( fstTransCfgCorrect testMachine input )  
  , test "second transition (see in the middle block)"                           
    <| assertEqual ( sndTransCfgForCheck testMachine input )                    
                   ( sndTransCfgCorrect testMachine input )  
  , test "last"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine input ) 
                   ( lastCfgCorrect testMachine input )
  , test "count"
    <| assertEqual (List.length (runRes testMachine input)) 7
  ] 

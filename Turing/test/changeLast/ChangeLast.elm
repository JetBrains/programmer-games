-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)
-- Test suite that contains all tests (head, middle, last, count)

module ChangeLast exposing (..)

import ElmTest exposing (..)

import TestDataCL exposing (..)
import CommonRun exposing (..)
import HeadTestCL exposing (..)
import MiddleTestCL exposing (..)
import LastTestCL exposing (..)


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



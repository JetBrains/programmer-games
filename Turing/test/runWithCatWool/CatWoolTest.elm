-- Check if module works with cat and wools correctly.                          
-- Must write Just Blue at the begin., Just Red instead of first Nothing   

-- Test suite that contains all tests (head, middle, last, count)

module CatWoolTest exposing (..)

import ElmTest exposing (..)
import TestDataCW exposing (..)
import CommonRun exposing (..)
import HeadTestCW exposing (..)
import MiddleTestCW exposing (..)
import LastTestCW exposing (..)

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual ( headCfgForCheck testMachine input ) 
                   ( headCfgCorrect testMachine input )
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual ( fstTransCfgForCheck testMachine input )                       
                   ( fstTransCfgCorrect testMachine input )  
  , test "last"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine input ) 
                   ( lastCfgCorrect testMachine input )
  , test "count"
    <| assertEqual (List.length (runRes testMachine input)) 5
  ] 



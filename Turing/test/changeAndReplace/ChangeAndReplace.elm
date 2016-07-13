-- Change item (_ _ 0 _ _ -> _ _ 1 _ _)                                         
-- Replace items (_ _ 0 1 _ -> _ _ 1 0 _)

-- Test suite that contains all tests (head, middle, last, count)

module ChangeAndReplace exposing (..)

import ElmTest exposing (..)
import TestDataCAR exposing (..)
import CommonRun exposing (..)
import HeadTestCAR exposing (..)
import MiddleTestCAR exposing (..)
import LastTestCAR exposing (..)


tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head for change test"     
    <| assertEqual ( headCfgForCheck testMachine inputForCh ) 
                   ( headCfgCorrect testMachine inputForCh )
  , test "head for replace test"                                                  
    <| assertEqual ( headCfgForCheck testMachine inputForRepl )                        
                   ( headCfgCorrect testMachine inputForRepl )  

  , test "first transition (see in the middle block) for change"                                                                 
    <| assertEqual ( fstTransCfgForCheckC testMachine inputForCh )                       
                   ( fstTransCfgCorrectC testMachine inputForCh )  
  , test "second transition (see in the middle block) for change"                           
    <| assertEqual ( sndTransCfgForCheckC testMachine inputForCh )                    
                   ( sndTransCfgCorrectC testMachine inputForCh )  
  , test "first transition (see in the middle block) for replace"                           
    <| assertEqual ( fstTransCfgForCheckR testMachine inputForRepl )                    
                   ( fstTransCfgCorrectR testMachine inputForRepl )                     
  , test "second transition (see in the middle block) for replace"                          
    <| assertEqual ( sndTransCfgForCheckR testMachine inputForRepl )                    
                   ( sndTransCfgCorrectR testMachine inputForRepl )  

  , test "last for change"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine inputForCh ) 
                   ( lastCfgCorrectC testMachine inputForCh )
  , test "last for replace"                                                         
    <| assertEqual ( lastCfgForCheck testMachine inputForRepl )                        
                   ( lastCfgCorrectR testMachine inputForRepl )  
  , test "count for change"
    <| assertEqual (List.length (runRes testMachine inputForCh)) 7
  , test "count for replace"                                                     
    <| assertEqual (List.length (runRes testMachine inputForRepl)) 9
  ] 

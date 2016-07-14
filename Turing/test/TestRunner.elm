module Main exposing (..)

import ElmTest exposing (..)

import AddSame  
import CatWoolTest  
import ChangeAndReplace  
import ChangeLast
import ChangeEachSnd
import ReplAllWithOne

tests : Test
tests = 
  suite "A Test Suite"                                                          
  [ ChangeLast.tests
  , AddSame.tests
  , CatWoolTest.tests
  , ChangeAndReplace.tests
  , ChangeEachSnd.tests
  , ReplAllWithOne.tests
  ] 


main : Program Never
main =
  runSuite tests

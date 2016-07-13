module Main exposing (..)

import ElmTest exposing (..)
import AddSame  
import CatWoolTest  
import ChangeAndReplace  
import ChangeLast

tests : Test
tests = 
  suite "A Test Suite"                                                          
  [ ChangeLast.tests
  , AddSame.tests
  , CatWoolTest.tests
  , ChangeAndReplace.tests
  , ChangeLast.tests
  ] 


main : Program Never
main =
  runSuite tests

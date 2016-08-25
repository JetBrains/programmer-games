import TasksBlock4Tests.AddSame  
import TasksBlock4Tests.FirstToEnd
import TasksBlock4Tests.LastToLeftEnd
import TasksBlock4Tests.SwapFirstLast
import TasksBlock4Tests.CompareFirstLast
import TasksBlock4Tests.BallsEvenNumber
import TasksBlock4Tests.DeleteRightHalf
import TasksBlock5Tests.DeleteSecondBall
--import TasksBlock4Tests.CatWoolTest 
--import TasksBlock4Tests.ChangeAndReplace  
--import TasksBlock4Tests.ChangeLast
--import TasksBlock4Tests.ChangeEachSnd
--import TasksBlock4Tests.ReplAllWithOne

import ElmTest exposing (Test, suite, runSuite)

tests : Test
tests = 
  suite "A Test Suite"                                                          
    [ TasksBlock4Tests.AddSame.tests
    , TasksBlock4Tests.FirstToEnd.tests 
    , TasksBlock4Tests.LastToLeftEnd.tests
    , TasksBlock4Tests.SwapFirstLast.tests
    , TasksBlock4Tests.CompareFirstLast.tests
    , TasksBlock4Tests.BallsEvenNumber.tests
    , TasksBlock4Tests.DeleteRightHalf.tests
    , TasksBlock5Tests.DeleteSecondBall.tests
--    , TasksBlock4Tests.ChangeLast.tests
--    , TasksBlock4Tests.CatWoolTest.tests
--    , TasksBlock4Tests.ChangeAndReplace.tests
--    , TasksBlock4Tests.ChangeEachSnd.tests
--    , TasksBlock4Tests.ReplAllWithOne.tests
    ] 


main : Program Never
main =
  runSuite tests

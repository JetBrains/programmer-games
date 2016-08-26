import TasksBlock4Tests.AddSame  
import TasksBlock4Tests.FirstToEnd
import TasksBlock4Tests.LastToLeftEnd
import TasksBlock4Tests.SwapFirstLast
import TasksBlock4Tests.CompareFirstLast
import TasksBlock4Tests.BallsEvenNumber
import TasksBlock4Tests.DeleteRightHalf
import TasksBlock5Tests.DeleteSecondBall
import TasksBlock5Tests.DeleteFirstGreen
import TasksBlock5Tests.DeleteAllYellow
import TasksBlock5Tests.DeleteIdenticalBalls
import TasksBlock5Tests.YellowAfterFirst
import TasksBlock5Tests.YellowAfterFirstGreen
import TasksBlock5Tests.TwoGreenInsteadRed
import TasksBlock5Tests.RedInsteadPairYellowBlue
import TasksBlock6Tests.NewWordWithoutYellow
import TasksBlock6Tests.NewRainbowWord
import TasksBlock6Tests.TurnWord
import TasksBlock6Tests.DoubleWord
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
    , TasksBlock5Tests.DeleteFirstGreen.tests
    , TasksBlock5Tests.DeleteAllYellow.tests
    , TasksBlock5Tests.DeleteIdenticalBalls.tests
    , TasksBlock5Tests.YellowAfterFirst.tests
    , TasksBlock5Tests.YellowAfterFirstGreen.tests
    , TasksBlock5Tests.TwoGreenInsteadRed.tests
    , TasksBlock5Tests.RedInsteadPairYellowBlue.tests
    , TasksBlock6Tests.NewWordWithoutYellow.tests
    , TasksBlock6Tests.NewRainbowWord.tests
    , TasksBlock6Tests.TurnWord.tests
    , TasksBlock6Tests.DoubleWord.tests
--    , TasksBlock4Tests.ChangeLast.tests
--    , TasksBlock4Tests.CatWoolTest.tests
--    , TasksBlock4Tests.ChangeAndReplace.tests
--    , TasksBlock4Tests.ChangeEachSnd.tests
--    , TasksBlock4Tests.ReplAllWithOne.tests
    ] 


main : Program Never
main =
  runSuite tests

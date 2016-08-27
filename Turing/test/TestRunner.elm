import TasksBlock1Tests.BlueAtRightEnd
import TasksBlock1Tests.BlueAtLeftEnd
import TasksBlock1Tests.BlueAtBothEnds
import TasksBlock2Tests.RainbowOnEmptyTape
import TasksBlock2Tests.BallsWithSpaceAfterEachOne
import TasksBlock2Tests.SpaceBetweenRedGreenPairs
import TasksBlock3Tests.ChangeInputBall
import TasksBlock3Tests.ChangeLastBall
import TasksBlock3Tests.ReplaceAllWithBlue
import TasksBlock3Tests.SwapTwoBalls
import TasksBlock3Tests.DeleteAllExceptFirst
import TasksBlock3Tests.DeleteAllExceptLast
import TasksBlock3Tests.GreenOnLeftYellowOnRight
import TasksBlock3Tests.YellowToGreenYellowAfterWord
import TasksBlock3Tests.ChangeEachSecond
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
import ElmTest exposing (Test, suite, runSuite)

tests : Test
tests = 
  suite "All tests"                                                          
    [ TasksBlock1Tests.BlueAtRightEnd.tests
    , TasksBlock1Tests.BlueAtLeftEnd.tests
    , TasksBlock1Tests.BlueAtBothEnds.tests
    , TasksBlock2Tests.RainbowOnEmptyTape.tests
    , TasksBlock2Tests.BallsWithSpaceAfterEachOne.tests
    , TasksBlock2Tests.SpaceBetweenRedGreenPairs.tests
    , TasksBlock3Tests.ChangeInputBall.tests
    , TasksBlock3Tests.ChangeLastBall.tests
    , TasksBlock3Tests.ReplaceAllWithBlue.tests
    , TasksBlock3Tests.SwapTwoBalls.tests
    , TasksBlock3Tests.DeleteAllExceptFirst.tests
    , TasksBlock3Tests.DeleteAllExceptLast.tests
    , TasksBlock3Tests.GreenOnLeftYellowOnRight.tests
    , TasksBlock3Tests.YellowToGreenYellowAfterWord.tests
    , TasksBlock3Tests.ChangeEachSecond.tests
    , TasksBlock4Tests.AddSame.tests
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
    ] 


main : Program Never
main =
  runSuite tests

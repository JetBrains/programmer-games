module Main exposing (..)

import ElmTest exposing (..)
import TestForTuring


tests : Test
tests =
      TestForTuring.tests


main : Program Never
main =
  runSuite tests

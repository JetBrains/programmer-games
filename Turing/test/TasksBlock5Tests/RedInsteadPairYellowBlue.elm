-- 5_8 - Replace the first pair of yellow and blue balls by red ball (delete    
-- yellow-blue balls, insert red)

module TasksBlock5Tests.RedInsteadPairYellowBlue exposing (tests)

import TuringMachine.TuringTypes exposing
        (Machine, MachineCfg, TapeCfg, Direction(..), TransTable)
import TuringMachine.RunTuring   exposing (runMachine, transFunc)
import TuringMachine.InitUpdate  exposing (initMachineCfg)

import Array   exposing (fromList, empty)
import ElmTest exposing (Test, suite, test, assertEqual)                                                    
import List    exposing (head, tail, reverse, length, drop, take)


--TEST DATA--------------------------------------------------------------------
type BallOfWool = Red | Yellow | Green | Blue                                   
type Kitten = White | LightGrey | Grey | Brown | DarkBrown | Orange | Violet


machine : Machine BallOfWool Kitten                                          
machine =                                                                    
  { transition = (transFunc transTable (Violet, Nothing, MoveLeft))          
  , initHeadPosForDraw = 0                                                      
  , initHeadPosForMach = 0                                                      
  , startState  = White                                                         
  , acceptState = Orange                                                        
  , rejectState = Violet                                                        
  }


transTable : TransTable BallOfWool Kitten                                
transTable =                                                                 
  fromList  
    [ { key   = (White, Just Yellow)                                            
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft) 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (Orange, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Grey, Just Blue, MoveRight)                                
      }                             
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Just Blue, MoveLeft)                                 
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =  
  [Just Blue, Just Yellow, Just Yellow, Just Yellow, Just Blue, Nothing] 
-------------------------------------------------------------------------------


--TEST FUNCTIONS---------------------------------------------------------------  
runMach : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> Int -> 
          List (MachineCfg BallOfWool Kitten)    
runMach m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)
  in                                                                            
    (runMachine m init [init])     
-----------------------

fstTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten)
fstTestedCfg m inp hpos =                                                         
  (runMach m inp hpos)
  |> head                                                    


fstCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
fstCorrectMaybeCfg = Just fstCorrectCfg 


fstCorrectCfg : MachineCfg BallOfWool Kitten         
fstCorrectCfg =                                                                
  { currState = White
  , currDir   = Stay
  , tapeCfg   = 
      { leftSyms  = empty                                                           
      , currSym   = Just Blue                                                        
      , rightSyms = fromList [Just Yellow, Just Yellow, Just Yellow, Just Blue, 
                              Nothing]
      }  
  }                                                                             
-----------------------

sndTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
               Int -> Maybe (MachineCfg BallOfWool Kitten) 
sndTestedCfg m inp hpos =   
  (runMach m inp hpos)   
  |> drop 1
  |> head                                              


sndCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
sndCorrectMaybeCfg = Just sndCorrectCfg


sndCorrectCfg : MachineCfg BallOfWool Kitten             
sndCorrectCfg =                                                                    
  { currState = Grey 
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Nothing]                                 
      , currSym   = Just Yellow 
      , rightSyms = fromList [Just Yellow, Just Yellow, Just Blue, Nothing]                                
      }
  } 
-----------------------

thirdTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) -> 
                 Int -> Maybe (MachineCfg BallOfWool Kitten)
thirdTestedCfg m inp hpos =  
  (runMach m inp hpos)
  |> drop 2
  |> head                                                      
                                     

thirdCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)
thirdCorrectMaybeCfg = Just thirdCorrectCfg                                     


thirdCorrectCfg : MachineCfg BallOfWool Kitten         
thirdCorrectCfg =                                                                
  { currState = LightGrey  
  , currDir   = MoveRight
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Just Blue]
      , currSym   = Just Yellow                                                                
      , rightSyms = fromList [Just Yellow, Just Blue, Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
sixthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
sixthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
sixthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
sixthCorrectMaybeCfg = Just sixthCorrectCfg                                     
                                                                                
                                                                                
sixthCorrectCfg : MachineCfg BallOfWool Kitten                                  
sixthCorrectCfg =                                                               
  { currState = Orange                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Blue, Just Yellow, Just Yellow, 
                              Just Red]                               
      , currSym   = Nothing                                                 
      , rightSyms = empty                 
      }                                                                         
  }  
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "third cfg"                                                                    
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach) 
                     thirdCorrectMaybeCfg 
    , test "last cfg"                                                          
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     6
    ]
-------------------------------------------------------------------------------

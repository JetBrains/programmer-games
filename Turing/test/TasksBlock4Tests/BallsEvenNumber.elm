-- 4_6 - If input word contains even number of balls then dont change the word, 
-- else add the first ball to the right end

module TasksBlock4Tests.BallsEvenNumber exposing (tests)

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
    [ { key   = (White, Just Red)                                               
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (Grey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (Grey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)
      , value = (Orange, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                
    , { key   = (Grey, Just Green)                                              
      , value = (LightGrey, Just Green, MoveRight)
      }                                                                         
    , { key   = (Grey, Nothing)
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    ] 


input : List (Maybe BallOfWool)                                              
input =
  [Just Red, Just Yellow, Just Green, Nothing] 
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
      , currSym   = Just Red                                                        
      , rightSyms = fromList [Just Yellow, Just Green, Nothing]                                     
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
  { currState = LightGrey 
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Just Red]                                 
      , currSym   = Just Yellow                                     
      , rightSyms = fromList [Just Green, Nothing]                                
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
  { currState = Grey   
  , currDir   = MoveRight
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Just Red, Just Yellow]
      , currSym   = Just Green                                                                
      , rightSyms = fromList [Nothing]          
      }
  } 
-----------------------                                                         
                                                                                
fourthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fourthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 3                                                                     
  |> head                                                                       
                                                                                
                                                                                
fourthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
fourthCorrectMaybeCfg = Just fourthCorrectCfg                                     
                                                                                
                                                                                
fourthCorrectCfg : MachineCfg BallOfWool Kitten                                  
fourthCorrectCfg =                                                               
  { currState = LightGrey                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Just Yellow, Just Green]                            
      , currSym   = Nothing                               
      , rightSyms = empty                                          
      }                                                                         
  }
-----------------------                                                         
                                                                                
fifthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fifthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
fifthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
fifthCorrectMaybeCfg = Just fifthCorrectCfg                                   
                                                                                
                                                                                
fifthCorrectCfg : MachineCfg BallOfWool Kitten                                 
fifthCorrectCfg =                                                              
  { currState = Orange                                                       
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Red, Just Yellow]                
      , currSym   = Just Green                                                     
      , rightSyms = fromList [Just Red]                                                       
      }                                                                         
  } 
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock4Tests.BallsEvenNumber"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "third cfg"                                                                    
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach) 
                     thirdCorrectMaybeCfg 
    , test "fourth cfg"                                                          
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)  
                     fourthCorrectMaybeCfg    
    , test "last cfg"                                                         
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     5
    ]
-------------------------------------------------------------------------------

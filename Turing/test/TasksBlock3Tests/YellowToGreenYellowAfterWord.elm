-- 3_8 - Change yellow balls to green balls (in input word) and put all yellow  
-- balls after input word

module TasksBlock3Tests.YellowToGreenYellowAfterWord exposing (tests)

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
      , value = (LightGrey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (White, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Brown, Just Yellow, MoveLeft)                                             
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                             
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Yellow, Just Blue, Just Yellow, Just Blue]
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
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Blue, Just Yellow, Just Blue]                                     
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
      { leftSyms  = fromList [Just Green]                                
      , currSym   = Just Blue
      , rightSyms = fromList [Just Yellow, Just Blue]                                
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
      { leftSyms  = fromList [Just Green, Just Blue, Just Yellow]                                       
      , currSym   = Just Blue                
      , rightSyms = empty                           
      }                                                                         
  }   
-----------------------                                                         
                                                                                
sixthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->          
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                      
sixthTestedCfg m inp hpos =                                                       
  (runMach m inp hpos)                                                          
  |> drop 5                                                                     
  |> head                                                                       
                                                                                
                                                                                
sixthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                       
sixthCorrectMaybeCfg = Just sixthCorrectCfg                                         
                                                                                
                                                                                
sixthCorrectCfg : MachineCfg BallOfWool Kitten                                    
sixthCorrectCfg =                                                                 
  { currState = Grey                                                       
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Blue, Just Yellow, Just Blue, 
                              Nothing]  
      , currSym   = Nothing     
      , rightSyms = empty                           
      }                                                                         
  }   
-----------------------                                                         
                                                                                
seventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                   Int -> Maybe (MachineCfg BallOfWool Kitten)                    
seventhTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 6                                                                     
  |> head                                                                       
                                                                                
                                                                                
seventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
seventhCorrectMaybeCfg = Just seventhCorrectCfg                                     
                                                                                
                                                                                
seventhCorrectCfg : MachineCfg BallOfWool Kitten                                  
seventhCorrectCfg =                                                               
  { currState = Brown                                                            
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Blue, Just Yellow, Just Blue]                                          
      , currSym   = Nothing                            
      , rightSyms = fromList [Just Yellow]                                                       
      }                                                                         
  }   
-----------------------                                                         
                                                                                
twelfthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->      
                   Int -> Maybe (MachineCfg BallOfWool Kitten)                  
twelfthTestedCfg m inp hpos =                                                   
  (runMach m inp hpos)                                                          
  |> drop 11                                                                     
  |> head                                                                       
                                                                                
                                                                                
twelfthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                   
twelfthCorrectMaybeCfg = Just twelfthCorrectCfg                                 
                                                                                
                                                                                
twelfthCorrectCfg : MachineCfg BallOfWool Kitten                                
twelfthCorrectCfg =                                                             
  { currState = White                                                           
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green]    
      , currSym   = Just Blue                                        
      , rightSyms = fromList [Just Yellow, Just Blue, Nothing, Just Yellow]                                      
      }                                                                         
  }  
-----------------------                                                         
                                                                                
twentyFourthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool)      
                        -> Int -> Maybe (MachineCfg BallOfWool Kitten)                  
twentyFourthTestedCfg m inp hpos =                                                   
  (runMach m inp hpos)                                                          
  |> reverse                                                                   
  |> head                                                                       
                                                                                
                                                                                
twentyFourthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                   
twentyFourthCorrectMaybeCfg = Just twentyFourthCorrectCfg                                 
                                                                                
                                                                                
twentyFourthCorrectCfg : MachineCfg BallOfWool Kitten                                
twentyFourthCorrectCfg =                                                             
  { currState = Orange                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Blue, Just Green]                                       
      , currSym   = Just Blue
      , rightSyms = fromList [Nothing, Just Yellow, Just Yellow]     
      }                                                                         
  } 
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.YellowToGreenYellowAfterWord"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "fourth cfg"                                                         
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)    
                     fourthCorrectMaybeCfg     
    , test "sixth cfg"                                                         
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach)    
                     sixthCorrectMaybeCfg    
    , test "seventh cfg"                                                          
      <| assertEqual (seventhTestedCfg machine input 
                                       machine.initHeadPosForMach)  
                     seventhCorrectMaybeCfg  
    , test "twelfth cfg"                                                        
      <| assertEqual (twelfthTestedCfg machine input 
                                       machine.initHeadPosForMach)
                     twelfthCorrectMaybeCfg 
    , test "last cfg"                                                        
      <| assertEqual (twentyFourthTestedCfg machine input 
                                            machine.initHeadPosForMach)
                     twentyFourthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     24
    ]
-------------------------------------------------------------------------------

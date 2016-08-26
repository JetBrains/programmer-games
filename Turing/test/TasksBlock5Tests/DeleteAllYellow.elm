-- 5_3 - Delete all yellow balls from the word, if they exist, else dont change 
-- the word

module TasksBlock5Tests.DeleteAllYellow exposing (tests)

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
      , value = (LightGrey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (White, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (Orange, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Grey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (Grey, Just Red, MoveLeft)                                                               
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (White, Nothing, MoveRight)                                
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =  
  [Just Red, Just Yellow, Just Red, Just Yellow, Just Red, Nothing, Nothing]
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
      , rightSyms = fromList [Just Yellow, Just Red, Just Yellow, Just Red, 
                              Nothing, Nothing]
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
      { leftSyms  = fromList [Nothing]                                 
      , currSym   = Just Yellow                                      
      , rightSyms = fromList [Just Red, Just Yellow, Just Red, Nothing, 
                              Nothing]                                
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
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = empty
      , currSym   = Nothing                                                                
      , rightSyms = fromList [Just Red, Just Red, Just Yellow, Just Red, 
                              Nothing, Nothing]          
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
  { currState = White                                                            
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                                                       
      , currSym   = Just Red                                                     
      , rightSyms = fromList [Just Red, Just Yellow, Just Red, Nothing, 
                              Nothing]
      }                                                                         
  } 
-----------------------                                                         
                                                                                
thirteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                      Int -> Maybe (MachineCfg BallOfWool Kitten)                   
thirteenthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
thirteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
thirteenthCorrectMaybeCfg = Just thirteenthCorrectCfg                                   
                                                                                
                                                                                
thirteenthCorrectCfg : MachineCfg BallOfWool Kitten                                 
thirteenthCorrectCfg =                                                              
  { currState = Orange                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Just Red]                                          
      , currSym   = Just Red                                                    
      , rightSyms = fromList [Just Red, Nothing]                                          
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
    , test "fourth cfg"                                                          
      <| assertEqual (fourthTestedCfg machine input machine.initHeadPosForMach)  
                     fourthCorrectMaybeCfg 
    , test "last cfg"                                                         
      <| assertEqual (thirteenthTestedCfg machine input 
                                          machine.initHeadPosForMach) 
                     thirteenthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     13
    ]
-------------------------------------------------------------------------------

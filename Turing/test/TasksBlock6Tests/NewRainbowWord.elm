-- 6_2 - Arrange balls in colors of the rainbow by creation new word near input 
-- word. Provide proccessing of the case where there isnt red or yellow ball in 
-- input word

module TasksBlock6Tests.NewRainbowWord exposing (tests)

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
  , initHeadPosForMach = 1                                                      
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
    , { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (White, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (Grey, Just Red, MoveLeft)                                 
      }                             
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Grey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Brown, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Red)                                               
      , value = (Brown, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (DarkBrown, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (DarkBrown, Just Red)                                           
      , value = (DarkBrown, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (DarkBrown, Just Yellow)                                        
      , value = (DarkBrown, Just Yellow, MoveRight)                                
      }                       
    , { key   = (DarkBrown, Just Blue)                                          
      , value = (DarkBrown, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (DarkBrown, Nothing)                                            
      , value = (Orange, Just Yellow, MoveLeft)                                 
      }                                                                         
    ]    


input : List (Maybe BallOfWool)                                              
input =  
  [Nothing, Just Yellow, Just Red, Just Blue, Nothing, Nothing, Nothing]
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
      { leftSyms  = fromList [Nothing]                                                           
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Red, Just Blue, Nothing, Nothing, Nothing]                                     
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
      { leftSyms  = fromList [Nothing, Just Yellow, Just Red]
      , currSym   = Just Blue                              
      , rightSyms = fromList [Nothing, Nothing, Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
fifthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fifthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 4                                                                     
  |> head                                                                       
                                                                                
                                                                                
fifthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
fifthCorrectMaybeCfg = Just fifthCorrectCfg                                     
                                                                                
                                                                                
fifthCorrectCfg : MachineCfg BallOfWool Kitten                                  
fifthCorrectCfg =                                                               
  { currState = Grey                                                       
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Yellow, Just Red]                   
      , currSym   = Just Blue           
      , rightSyms = fromList [Just Red, Nothing, Nothing]                        
      }                                                                         
  } 
-----------------------                                                         
                                                                                
ninthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
ninthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 8                                                                     
  |> head                                                                       
                                                                                
                                                                                
ninthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
ninthCorrectMaybeCfg = Just ninthCorrectCfg                                     
                                                                                
                                                                                
ninthCorrectCfg : MachineCfg BallOfWool Kitten                                  
ninthCorrectCfg =                                                               
  { currState = Brown                                                            
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                   
      , currSym   = Just Yellow                                                   
      , rightSyms = fromList [Just Red, Just Blue, Just Red, Nothing, Nothing]                       
      }                                                                         
  } 
-----------------------                                                         
                                                                                
tenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                 Int -> Maybe (MachineCfg BallOfWool Kitten)                    
tenthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 9                                                                     
  |> head                                                                       
                                                                                
                                                                                
tenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
tenthCorrectMaybeCfg = Just tenthCorrectCfg                                     
                                                                                
                                                                                
tenthCorrectCfg : MachineCfg BallOfWool Kitten                                  
tenthCorrectCfg =                                                               
  { currState = DarkBrown                                                           
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Yellow]                                          
      , currSym   = Just Red                              
      , rightSyms = fromList [Just Blue, Just Red, Nothing, Nothing]  
      }                                                                         
  } 
-----------------------                                                         
                                                                                
fourteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                      Int -> Maybe (MachineCfg BallOfWool Kitten)                    
fourteenthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
fourteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
fourteenthCorrectMaybeCfg = Just fourteenthCorrectCfg                                     
                                                                                
                                                                                
fourteenthCorrectCfg : MachineCfg BallOfWool Kitten                                  
fourteenthCorrectCfg =                                                               
  { currState = Orange                                                       
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Yellow, Just Red, Just Blue]                             
      , currSym   = Just Red                                                    
      , rightSyms = fromList [Just Yellow, Nothing]            
      }                                                                         
  }
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock6Tests.NewRainbowWord"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "third cfg"                                                                    
      <| assertEqual (thirdTestedCfg machine input machine.initHeadPosForMach) 
                     thirdCorrectMaybeCfg 
    , test "fifth cfg"                                                          
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach)  
                     fifthCorrectMaybeCfg   
    , test "ninth cfg"                                                          
      <| assertEqual (ninthTestedCfg machine input machine.initHeadPosForMach)  
                     ninthCorrectMaybeCfg    
    , test "tenth cfg"                                                          
      <| assertEqual (tenthTestedCfg machine input machine.initHeadPosForMach)  
                     tenthCorrectMaybeCfg
    , test "last cfg"                                                          
      <| assertEqual (fourteenthTestedCfg machine input 
                                          machine.initHeadPosForMach)  
                     fourteenthCorrectMaybeCfg 
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     14
    ]
-------------------------------------------------------------------------------

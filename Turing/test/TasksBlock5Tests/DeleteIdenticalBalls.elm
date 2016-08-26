-- 5_4 - Delete each pair of identical balls

module TasksBlock5Tests.DeleteIdenticalBalls exposing (tests)

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
      , value = (Brown, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (Grey, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Brown, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (LightGrey, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Brown, Nothing, MoveLeft)                                 
      }   
    , { key   = (Grey, Nothing)                                                 
      , value = (Brown, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (White, Nothing, MoveRight)                                
      }                                                                         
    ]  


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Blue, Just Yellow, Just Blue, Just Blue, Just Blue, Just Yellow,        
   Nothing, Nothing, Nothing]
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
      , rightSyms = fromList [Just Yellow, Just Blue, Just Blue, Just Blue, 
                              Just Yellow, Nothing, Nothing, Nothing]
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
      , rightSyms = fromList [Just Blue, Just Blue, Just Blue, Just Yellow, 
                              Nothing, Nothing, Nothing]
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
      , currSym   = Just Blue                                                                
      , rightSyms = fromList [Just Blue, Just Blue, Just Yellow, Nothing, 
                              Nothing, Nothing]          
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
  { currState = Brown                                                       
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Blue]                               
      , currSym   = Just Yellow           
      , rightSyms = fromList [Nothing, Just Blue, Just Yellow, Nothing,       
                              Nothing, Nothing]                                 
      }                                                                         
  } 
-----------------------                                                         
                                                                                
eighthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                    
eighthTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 7                                                                     
  |> head                                                                       
                                                                                
                                                                                
eighthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
eighthCorrectMaybeCfg = Just eighthCorrectCfg                                     
                                                                                
                                                                                
eighthCorrectCfg : MachineCfg BallOfWool Kitten                                  
eighthCorrectCfg =                                                               
  { currState = White                                                           
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                               
      , currSym   = Just Blue                                                 
      , rightSyms = fromList [Just Yellow, Nothing, Just Blue, Just Yellow, 
                              Nothing, Nothing, Nothing]
      }                                                                         
  } 
-----------------------                                                         
                                                                                
twentySecondTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                        Int -> Maybe (MachineCfg BallOfWool Kitten)                   
twentySecondTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
twentySecondCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
twentySecondCorrectMaybeCfg = Just twentySecondCorrectCfg
                                                                                
                                                                                
twentySecondCorrectCfg : MachineCfg BallOfWool Kitten                                 
twentySecondCorrectCfg =                                                              
  { currState = Orange                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Just Blue, 
                              Just Yellow, Just Blue, Just Yellow]
      , currSym   = Nothing
      , rightSyms = fromList [Nothing]                        
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
    , test "fifth cfg"                                                          
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach)  
                     fifthCorrectMaybeCfg 
    , test "eighth cfg"                                                          
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach)  
                     eighthCorrectMaybeCfg 
    , test "last cfg"                                                         
      <| assertEqual (twentySecondTestedCfg machine input 
                                            machine.initHeadPosForMach) 
                     twentySecondCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     22
    ]
-------------------------------------------------------------------------------

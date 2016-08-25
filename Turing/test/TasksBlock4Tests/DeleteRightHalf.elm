-- 4_7 - Delete right half of the word

module TasksBlock4Tests.DeleteRightHalf exposing (tests)

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
    [ { key   = (White, Just Blue)                                              
      , value = (LightGrey, Just Green, MoveRight)                                
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
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Brown, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveLeft)                                 
      }       
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (White, Just Blue, MoveRight)                                
      }                                                                         
    ] 


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Just Blue, Just Blue, Just Blue, Just Yellow, Just Yellow, Just Yellow,      
   Nothing]      
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
      , rightSyms = fromList [Just Blue, Just Blue, Just Yellow, Just Yellow, 
                              Just Yellow, Nothing]
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
      , rightSyms = fromList [Just Blue, Just Yellow, Just Yellow, Just Yellow, 
                              Nothing]
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
  { currState = Grey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Just Green, Just Blue, Just Blue, Just Yellow, 
                              Just Yellow]
      , currSym   = Just Yellow                                                                
      , rightSyms = fromList [Nothing]          
      }
  }     
-----------------------                                                         
                                                                                
ninethTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                  Int -> Maybe (MachineCfg BallOfWool Kitten)                   
ninethTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> drop 8                                                                     
  |> head                                                                       
                                                                                
                                                                                
ninethCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
ninethCorrectMaybeCfg = Just ninethCorrectCfg                                   
                                                                                
                                                                                
ninethCorrectCfg : MachineCfg BallOfWool Kitten                                 
ninethCorrectCfg =                                                              
  { currState = Brown                                                            
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Green, Just Blue, Just Blue, Just Yellow]
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Nothing, Nothing]                                          
      }                                                                         
  } 
-----------------------                                                         
                                                                                
fourteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->       
                      Int -> Maybe (MachineCfg BallOfWool Kitten)                   
fourteenthTestedCfg m inp hpos =                                                    
  (runMach m inp hpos)                                                          
  |> drop 13                                                                     
  |> head                                                                       
                                                                                
                                                                                
fourteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                    
fourteenthCorrectMaybeCfg = Just fourteenthCorrectCfg                                   
                                                                                
                                                                                
fourteenthCorrectCfg : MachineCfg BallOfWool Kitten                                 
fourteenthCorrectCfg =                                                              
  { currState = White                                                           
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Blue]    
      , currSym   = Just Blue                                                 
      , rightSyms = fromList [Just Blue, Just Yellow, Just Yellow, Nothing, 
                              Nothing]
      }                                                                         
  }
-----------------------                                                         
                                                                                
twentyNinthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->   
                       Int -> Maybe (MachineCfg BallOfWool Kitten)               
twentyNinthTestedCfg m inp hpos =                                                
  (runMach m inp hpos)                                                          
  |> reverse                                                                    
  |> head                                                                       
                                                                                
                                                                                
twentyNinthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                
twentyNinthCorrectMaybeCfg = Just twentyNinthCorrectCfg
                                                                                
                                                                                
twentyNinthCorrectCfg : MachineCfg BallOfWool Kitten                             
twentyNinthCorrectCfg =                                                          
  { currState = Orange                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Just Blue, Just Blue]                                        
      , currSym   = Just Blue
      , rightSyms = fromList [Nothing, Nothing, Nothing, Nothing]                                          
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
    , test "eighth cfg"                                                                    
      <| assertEqual (eighthTestedCfg machine input machine.initHeadPosForMach) 
                     eighthCorrectMaybeCfg 
    , test "nineth cfg"                                                         
      <| assertEqual (ninethTestedCfg machine input machine.initHeadPosForMach) 
                     ninethCorrectMaybeCfg     
    , test "fourteenth cfg"                                                         
      <| assertEqual (fourteenthTestedCfg machine input 
                                          machine.initHeadPosForMach) 
                     fourteenthCorrectMaybeCfg   
    , test "last cfg"                                                     
      <| assertEqual (twentyNinthTestedCfg machine input                         
                                           machine.initHeadPosForMach)           
                     twentyNinthCorrectMaybeCfg    
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     29 
    ]
-------------------------------------------------------------------------------

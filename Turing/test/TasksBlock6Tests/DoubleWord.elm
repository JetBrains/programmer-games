-- 6_4 - Double the word, put blue ball between the word and its copy

module TasksBlock6Tests.DoubleWord exposing (tests)

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
    [ { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (Grey, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (LightGrey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Brown, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Orange, Just Blue, MoveRight)                                
      }   
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Blue)                                              
      , value = (Brown, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (LightGrey, Just Green, MoveLeft)                                 
      }                                                                         
    ] 


input : List (Maybe BallOfWool)                                              
input =                                                                      
  [Nothing, Just Green, Just Green, Just Green, Nothing, Nothing, Nothing,      
   Nothing, Nothing]    
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
      , currSym   = Just Green                                                  
      , rightSyms = fromList [Just Green, Just Green, Nothing, Nothing, 
                              Nothing, Nothing, Nothing]
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
  { currState = LightGrey   
  , currDir   = MoveLeft
  , tapeCfg   =                                                     
      { leftSyms  = fromList [Nothing, Just Green, Just Green]
      , currSym   = Just Green                                                                
      , rightSyms = fromList [Just Blue, Nothing, Nothing, Nothing, Nothing]          
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
  { currState = Grey                                                       
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                  
      , currSym   = Just Green                                                  
      , rightSyms = fromList [Just Green, Just Green, Just Blue, Nothing, 
                              Nothing, Nothing, Nothing]    
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
  { currState = Brown                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red]                                          
      , currSym   = Just Green                                            
      , rightSyms = fromList [Just Green, Just Blue, Nothing,       
                              Nothing, Nothing, Nothing]                        
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
  { currState = LightGrey                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red, Just Green, Just Green]                                
      , currSym   = Just Blue                                                  
      , rightSyms = fromList [Just Green, Nothing, Nothing, Nothing]                        
      }                                                                         
  } 
-----------------------                                                         
                                                                                
eighteenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->   
                      Int -> Maybe (MachineCfg BallOfWool Kitten)               
eighteenthTestedCfg m inp hpos =                                                
  (runMach m inp hpos)                                                          
  |> drop 17                                                                    
  |> head                                                                       
                                                                                
                                                                                
eighteenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                
eighteenthCorrectMaybeCfg = Just eighteenthCorrectCfg                           
                                                                                
                                                                                
eighteenthCorrectCfg : MachineCfg BallOfWool Kitten                             
eighteenthCorrectCfg =                                                          
  { currState = Grey                                                       
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Green]        
      , currSym   = Just Green                                                   
      , rightSyms = fromList [Just Green, Just Blue, Just Green, Nothing, 
                              Nothing, Nothing]            
      }                                                                         
  }
-----------------------                                                         
                                                                                
thirtySeventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool)    
                         -> Int -> Maybe (MachineCfg BallOfWool Kitten)               
thirtySeventhTestedCfg m inp hpos =                                                
  (runMach m inp hpos)                                                          
  |> reverse                                                                    
  |> head                                                                       
                                                                                
                                                                                
thirtySeventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                
thirtySeventhCorrectMaybeCfg = Just thirtySeventhCorrectCfg                           
                                                                                
                                                                                
thirtySeventhCorrectCfg : MachineCfg BallOfWool Kitten                             
thirtySeventhCorrectCfg =                                                          
  { currState = Orange                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Green, Just Green, Just Green, 
                              Just Blue]                              
      , currSym   = Just Green       
      , rightSyms = fromList [Just Green, Just Green, Nothing]                                 
      }                                                                         
  } 
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock6Tests.DoubleWord"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "fifth cfg"                                                                    
      <| assertEqual (fifthTestedCfg machine input machine.initHeadPosForMach) 
                     fifthCorrectMaybeCfg 
    , test "ninth cfg"                                                          
      <| assertEqual (ninthTestedCfg machine input machine.initHeadPosForMach)  
                     ninthCorrectMaybeCfg 
    , test "tenth cfg"                                                          
      <| assertEqual (tenthTestedCfg machine input machine.initHeadPosForMach)  
                     tenthCorrectMaybeCfg    
    , test "fourteenth cfg"                                                          
      <| assertEqual (fourteenthTestedCfg machine input 
                                          machine.initHeadPosForMach)  
                     fourteenthCorrectMaybeCfg  
    , test "eighteenth cfg"                                                     
      <| assertEqual (eighteenthTestedCfg machine input                         
                                          machine.initHeadPosForMach)           
                     eighteenthCorrectMaybeCfg
    , test "last cfg"                                                     
      <| assertEqual (thirtySeventhTestedCfg machine input                         
                                             machine.initHeadPosForMach)           
                     thirtySeventhCorrectMaybeCfg  
    , test "number of cfgs"
      <| assertEqual ((runMach machine input machine.initHeadPosForMach) 
                      |> length)
                     37 
    ]
-------------------------------------------------------------------------------

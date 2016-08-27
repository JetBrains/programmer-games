-- 3_6 - Delete all balls except the last ball (dont change empty word)

module TasksBlock3Tests.DeleteAllExceptLast exposing (tests)

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
      , value = (White, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Yellow)                                            
      , value = (White, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Green)                                             
      , value = (White, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (White, Just Blue)                                              
      , value = (White, Just Blue, MoveRight)                                
      }                                                                         
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (Grey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (Grey, Just Yellow, MoveLeft)                                 
      }   
    , { key   = (LightGrey, Just Green)                                         
      , value = (Grey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Blue)                                          
      , value = (Grey, Just Blue, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Just Blue)                                               
      , value = (Grey, Nothing, MoveLeft)                                 
      }                                                                         
    , { key   = (Grey, Nothing)                                                 
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    ]


input1 : List (Maybe BallOfWool)                                              
input1 =                                                                      
  [Nothing, Just Red, Just Yellow, Just Green, Just Blue, Nothing]


input2 : List (Maybe BallOfWool)                                                
input2 =                                                                        
  [Nothing]  
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
      , currSym   = Just Red 
      , rightSyms = fromList [Just Yellow, Just Green, Just Blue, Nothing]                                     
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
  { currState = White
  , currDir   = MoveRight
  , tapeCfg   =                                                         
      { leftSyms  = fromList [Nothing, Just Red]                                
      , currSym   = Just Yellow
      , rightSyms = fromList [Just Green, Just Blue, Nothing]                                
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
  { currState = LightGrey                                                          
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red, Just Yellow, Just Green]                            
      , currSym   = Just Blue
      , rightSyms = fromList [Nothing]                                                     
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
  { currState = Grey                                                       
  , currDir   = MoveLeft                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Just Red, Just Yellow]       
      , currSym   = Just Green                                       
      , rightSyms = fromList [Just Blue, Nothing]                                          
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
      { leftSyms  = fromList [Nothing, Just Red]                   
      , currSym   = Just Yellow                                         
      , rightSyms = fromList [Nothing, Just Blue, Nothing]                               
      }                                                                         
  }   
-----------------------                                                         
                                                                                
eleventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->      
                    Int -> Maybe (MachineCfg BallOfWool Kitten)                  
eleventhTestedCfg m inp hpos =                                                   
  (runMach m inp hpos)                                                          
  |> reverse                                                                     
  |> head                                                                       
                                                                                
                                                                                
eleventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                   
eleventhCorrectMaybeCfg = Just eleventhCorrectCfg                                 
                                                                                
                                                                                
eleventhCorrectCfg : MachineCfg BallOfWool Kitten                                
eleventhCorrectCfg =                                                             
  { currState = Orange                                                            
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                   
      , currSym   = Nothing                                 
      , rightSyms = fromList [Nothing, Nothing, Just Blue, Nothing]                               
      }                                                                         
  }       
-----------------------

emptyWordLastTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool)   
                         -> Int -> Maybe (MachineCfg BallOfWool Kitten)         
emptyWordLastTestedCfg m inp hpos =                                             
  (runMach m inp hpos)                                                          
  |> drop 2                                                                    
  |> head                                                                       
                                                                                
                                                                                
emptyWordLastCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)             
emptyWordLastCorrectMaybeCfg = Just emptyWordLastCorrectCfg                     
                                                                                
                                                                                
emptyWordLastCorrectCfg : MachineCfg BallOfWool Kitten                          
emptyWordLastCorrectCfg =                                                       
  { currState = Orange                                                          
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]                                          
      , currSym   = Nothing                                                     
      , rightSyms = empty                                                       
      }                                                                         
  }   
-------------------------------------------------------------------------------


--TESTS------------------------------------------------------------------------
tests : Test                                                                    
tests =                                                                         
  suite "TasksBlock3Tests.DeleteAllExceptLast"                                                        
    [ test "first cfg"     
      <| assertEqual (fstTestedCfg machine input1 machine.initHeadPosForMach)
                     fstCorrectMaybeCfg
    , test "second cfg"                                                                 
      <| assertEqual (sndTestedCfg machine input1 machine.initHeadPosForMach) 
                     sndCorrectMaybeCfg
    , test "sixth cfg"                                                           
      <| assertEqual (sixthTestedCfg machine input1 machine.initHeadPosForMach)  
                     sixthCorrectMaybeCfg 
    , test "seventh cfg"                                                          
      <| assertEqual (seventhTestedCfg machine input1 
                                       machine.initHeadPosForMach)  
                     seventhCorrectMaybeCfg  
    , test "eighth cfg"                                                        
      <| assertEqual (eighthTestedCfg machine input1                            
                                      machine.initHeadPosForMach)              
                     eighthCorrectMaybeCfg       
    , test "last cfg"                                                        
      <| assertEqual (eleventhTestedCfg machine input1 
                                        machine.initHeadPosForMach)              
                     eleventhCorrectMaybeCfg       
    , test "last cfg when empty word"                                           
      <| assertEqual (emptyWordLastTestedCfg machine input2                     
                                             machine.initHeadPosForMach)        
                     emptyWordLastCorrectMaybeCfg   
    , test "number of cfgs"
      <| assertEqual ((runMach machine input1 machine.initHeadPosForMach) 
                      |> length)
                     11
    ]
-------------------------------------------------------------------------------

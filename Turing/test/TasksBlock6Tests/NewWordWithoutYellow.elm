-- 6_1 - Delete yellow balls from the word, if they exist, else dont change the 
-- word  

module TasksBlock6Tests.NewWordWithoutYellow exposing (tests)

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
    , { key   = (White, Nothing)                                                
      , value = (LightGrey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Red)                                           
      , value = (LightGrey, Just Red, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Yellow)                                        
      , value = (LightGrey, Just Yellow, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Just Green)                                         
      , value = (LightGrey, Just Green, MoveLeft)                                 
      }                                                                         
    , { key   = (LightGrey, Nothing)                                            
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Red)                                                
      , value = (Brown, Nothing, MoveRight)                                
      }     
    , { key   = (Grey, Just Yellow)                                             
      , value = (Grey, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Grey, Just Green)                                              
      , value = (Orange, Nothing, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Red)                                               
      , value = (Brown, Just Red, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Yellow)                                            
      , value = (Brown, Just Yellow, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Just Green)                                             
      , value = (Brown, Just Green, MoveRight)                                
      }                                                                         
    , { key   = (Brown, Nothing)                                                
      , value = (LightGrey, Just Red, MoveLeft)                                 
      }                                                                         
    ]


input : List (Maybe BallOfWool)                                              
input =  
  [Nothing, Just Yellow, Just Yellow, Just Red, Just Yellow, Nothing, Nothing,  
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
      { leftSyms  = fromList [Nothing]                                                           
      , currSym   = Just Yellow                                                        
      , rightSyms = fromList [Just Yellow, Just Red, Just Yellow, Nothing, 
                              Nothing, Nothing]
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
      { leftSyms  = fromList [Nothing, Just Yellow, Just Yellow, Just Red]
      , currSym   = Just Yellow                                                                
      , rightSyms = fromList [Just Green, Nothing, Nothing]          
      }
  }  
-----------------------                                                         
                                                                                
eleventhTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->        
                    Int -> Maybe (MachineCfg BallOfWool Kitten)                    
eleventhTestedCfg m inp hpos =                                                     
  (runMach m inp hpos)                                                          
  |> drop 10                                                                     
  |> head                                                                       
                                                                                
                                                                                
eleventhCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                     
eleventhCorrectMaybeCfg = Just eleventhCorrectCfg                                     
                                                                                
                                                                                
eleventhCorrectCfg : MachineCfg BallOfWool Kitten                                  
eleventhCorrectCfg =                                                               
  { currState = Grey                                                       
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing]      
      , currSym   = Just Yellow                                                 
      , rightSyms = fromList [Just Yellow, Just Red, Just Yellow, Just Green,
                              Nothing, Nothing]                     
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
  { currState = Grey                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing]                                          
      , currSym   = Just Yellow                                         
      , rightSyms = fromList [Just Red,Just Yellow,Just Green,Nothing,Nothing]                                 
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
  { currState = Brown                                                            
  , currDir   = MoveRight                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Nothing]                                 
      , currSym   = Just Yellow
      , rightSyms = fromList [Just Green, Nothing, Nothing]  
      }                                                                         
  } 
-----------------------                                                         
                                                                                
seventeenthTestedCfg : Machine BallOfWool Kitten -> List (Maybe BallOfWool) ->   
                       Int -> Maybe (MachineCfg BallOfWool Kitten)               
seventeenthTestedCfg m inp hpos =                                                
  (runMach m inp hpos)                                                          
  |> drop 16                                                                    
  |> head                                                                       
                                                                                
                                                                                
seventeenthCorrectMaybeCfg : Maybe (MachineCfg BallOfWool Kitten)                
seventeenthCorrectMaybeCfg = Just seventeenthCorrectCfg                           
                                                                                
                                                                                
seventeenthCorrectCfg : MachineCfg BallOfWool Kitten                             
seventeenthCorrectCfg =                                                          
  { currState = LightGrey                                                           
  , currDir   = MoveLeft                                                       
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Nothing, Just Yellow]                        
      , currSym   = Just Green
      , rightSyms = fromList [Just Red, Nothing]        
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
  , currDir   = MoveRight                                                        
  , tapeCfg   =                                                                 
      { leftSyms  = fromList [Nothing, Nothing, Nothing, Nothing, Nothing, 
                              Nothing]  
      , currSym   = Just Red     
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
    , test "sixth cfg"                                                                    
      <| assertEqual (sixthTestedCfg machine input machine.initHeadPosForMach) 
                     sixthCorrectMaybeCfg 
    , test "eleventh cfg"                                                          
      <| assertEqual (eleventhTestedCfg machine input 
                                        machine.initHeadPosForMach)  
                     eleventhCorrectMaybeCfg
    , test "twelfth cfg"                                                       
      <| assertEqual (twelfthTestedCfg machine input                           
                                       machine.initHeadPosForMach)             
                     twelfthCorrectMaybeCfg  
    , test "fourteenth cfg"                                                        
      <| assertEqual (fourteenthTestedCfg machine input                            
                                          machine.initHeadPosForMach)             
                     fourteenthCorrectMaybeCfg       
    , test "sixteenth cfg"                                                     
      <| assertEqual (seventeenthTestedCfg machine input                         
                                           machine.initHeadPosForMach)           
                     seventeenthCorrectMaybeCfg   
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

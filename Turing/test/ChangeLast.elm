-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)

module ChangeLast exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)                                         
import Turing exposing ( Machine, MachineCfg, TapeCfg, Direction(..),            
                         TransTable, run, initMachineCfg, transFunc)      
                                                                                
                                                                                
--TEST DATA-------------------------------------------------------------------  
    
-- Test Data for tests (machine, transition table and input word)

testMachine : Machine Char Int                                                  
testMachine =                                                                   
  { transition = (transFunc transTable (4, Nothing, MoveLeft))                  
  , startState = 0                                                              
  , acceptState = 3                                                             
  , rejectState = 4                                                             
  }                                                                             
                                                                                
                                                                                
transTable : TransTable Char Int                                                
transTable =                                                                    
  [ { key = (0, Nothing),  value = (0, Nothing, MoveRight)}                     
  , { key = (0, Just '0'), value = (1, Just '0', MoveRight)}                    
  , { key = (1, Nothing),  value = (2, Nothing, MoveLeft)}                      
  , { key = (1, Just '0'), value = (1, Just '0', MoveRight)}                    
  , { key = (2, Just '0'), value = (3, Just '1', MoveLeft)}                     
  ]                                                                             
                                                                                
                                                                                
input : List (Maybe Char)                                                       
input =                                                                         
  [Nothing, Just '0', Just '0', Just '0', Nothing]                                                                              
------------------------------------------------------------------------------  
------------------------------------------------------------------------------  
                                                                                
                                                                                
--HELPERS---------------------------------------------------------------------  
                                                                                
-- Common run function for all tests                                            
                                                                                
runRes : Machine Char Int -> List (Maybe Char) -> List (MachineCfg Char Int)    
runRes m inp =                                                                  
  let                                                                           
    init = (initMachineCfg m inp)                                               
  in                                                                            
    (run m init [init])                                                         
                                                                                
                                                                                
--HEAD----------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs   
                                                                                
headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgForCheck m inp =                                                         
  (List.head (runRes m inp))                                                    
                                                                                
headCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe Char) -> TapeCfg Char                                    
headTCfg inp =                                                                  
  { leftSyms =  Array.empty                                                     
  , currSym =                                                                   
      case (List.head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms =                                                                 
      case (List.tail inp) of                                                   
        Just t -> (Array.fromList t)                                            
        Nothing -> Array.empty                                                  
  } 

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 


--MIDDLE----------------------------------------------------------------------

-- Check if do transition in two important cases:                               
-- 1) go to state 1 when read first Just char                                   
-- 2) go to state 2 from state 1 when read Nothing                              
                                                                                
fstTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgForCheck m inp =                                                     
  (List.head (List.drop 2 (runRes m inp)))                                      
                                                                                
fstTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                            
                                                                                
fstTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int     
fstTransMCfg m inp =                                                            
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfg inp)                                                
  }                                                                             
                                                                                
fstTransTCfg : List (Maybe Char) -> TapeCfg Char                                
fstTransTCfg inp =                                                              
  { leftSyms = Array.fromList (List.take 2 inp)                                 
  , currSym =                                                                   
      case (List.head (List.drop 2 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = Array.fromList (List.drop 3 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  
                                                                                
sndTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgForCheck m inp =                                                     
  (List.head (List.drop 5 (runRes m inp))) -- 6 config in list                  
                                                                                
sndTransCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
sndTransCfgCorrect m inp = Just (sndTransMCfg m inp)                            
                                                                                
sndTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int     
sndTransMCfg m inp =                                                            
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfg inp)                                                
  }                                                                             
                                                                                
sndTransTCfg : List (Maybe Char) -> TapeCfg Char                                
sndTransTCfg inp =                                                              
  { leftSyms = Array.fromList (List.take 3 inp)                                 
  , currSym =                                                                   
      case (List.head (List.drop 3 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = Array.fromList (List.drop 4 inp)                                
  }   

------------------------------------------------------------------------------  
------------------------------------------------------------------------------


--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs                          

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp =                                                         
  (List.head (List.reverse (runRes m inp)))                                     
                                                                                
lastCfgCorrect : Machine Char Int -> List (Maybe Char) -> Maybe (MachineCfg Char Int)
lastCfgCorrect m inp = Just (lastMCfg m inp)                                    
                                                                                
lastMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
lastMCfg m inp =                                                                
  { currState = m.acceptState                                                   
  , tapeCfg = (lastTCfg inp)                                                    
  }                                                                             
                                                                                
                                                                                
{-                                                                              
-- get index for last number (start with 1)                                     
indexOfLastNum : List (Maybe Char) -> Int -> Int                                
indexOfLastNum inp ind =                                                        
  case (List.head inp) of                                                       
    Just h ->                                                                   
      case h of                                                                 
        Just c ->                                                               
          case (List.head (List.drop 1 inp)) of                                 
            Just hh ->                                                          
              case hh of                                                        
                Just ch -> (indexOfLastNum (List.drop 1 inp) (ind+1))           
                Nothing -> ind                                                  
            Nothing -> -1                                                       
        Nothing -> (indexOfLastNum (List.drop 1 inp) (ind+1))                   
    Nothing -> -1                                                               
-}                                                                              
                                                                                
lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =                                                                  
    { leftSyms = (Array.fromList (List.take 2 inp))                             
    , currSym =                                                                 
       case (List.head (List.drop 2 inp)) of                                    
          Just sec -> sec                                                       
          Nothing -> Nothing                                                    
    , rightSyms = ( Array.fromList ([Just '1'] ++ (List.drop 4 inp)) )          
    }  

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual ( headCfgForCheck testMachine input ) 
                   ( headCfgCorrect testMachine input )
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual ( fstTransCfgForCheck testMachine input )                       
                   ( fstTransCfgCorrect testMachine input )  
  , test "second transition (see in the middle block)"                           
    <| assertEqual ( sndTransCfgForCheck testMachine input )                    
                   ( sndTransCfgCorrect testMachine input )  
  , test "last"                                                                    
    <| assertEqual ( lastCfgForCheck testMachine input ) 
                   ( lastCfgCorrect testMachine input )
  , test "count"
    <| assertEqual (List.length (runRes testMachine input)) 7
  ] 



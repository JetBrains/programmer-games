-- Change one last item (_ 0 0 0 _ -> _ 0 0 1 _)

module ChangeLast exposing (tests)

import ElmTest exposing (..)
import Array exposing (fromList, empty)     
import List exposing (head, tail, reverse, length, drop, take)

import TuringTypes exposing ( Machine, MachineCfg, TapeCfg, Direction(..), 
                              TransTable )
import RunTuring exposing (runMachine, transFunc)                                      
import InitUpdate exposing (initMachineCfg)  

                                                                                
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
  
initHeadPos : Int                                                               
initHeadPos = 0    

------------------------------------------------------------------------------  
------------------------------------------------------------------------------  
                                                                                
                                                                                
--HELPERS---------------------------------------------------------------------  
                                                                                
-- Common run function for all tests                                            
                                                                                
runRes : Machine Char Int -> List (Maybe Char) -> Int 
         -> List (MachineCfg Char Int)    
runRes m inp hpos =                                                                  
  let                                                                           
    init = (initMachineCfg m inp hpos)                                               
  in                                                                            
    (runMachine m init [init])                                                         
                                                                                
                                                                                
--HEAD----------------------------------------------------------------------    
                                                                                
-- Check the first MachineConfig in the list of configs   
                                                                                
headCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                  -> Maybe (MachineCfg Char Int)
headCfgForCheck m inp hpos =                                                         
  (head (runRes m inp hpos))                                                    
                                                                                
headCfgCorrect : Machine Char Int -> List (Maybe Char) 
                 -> Maybe (MachineCfg Char Int)
headCfgCorrect m inp = Just (headMCfg m inp)                                    
                                                                                
headMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int         
headMCfg m inp =                                                                
  { currState = m.startState                                                    
  , tapeCfg = (headTCfg inp)                                                    
  }                                                                             
                                                                                
headTCfg : List (Maybe Char) -> TapeCfg Char                                    
headTCfg inp =                                                                  
  { leftSyms = empty                                                     
  , currSym =                                                                   
      case (head inp) of                                                   
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms =                                                                 
      case (tail inp) of                                                   
        Just t -> (fromList t)                                            
        Nothing -> empty                                                  
  } 

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 


--MIDDLE----------------------------------------------------------------------

-- Check if do transition in two important cases:                               
-- 1) go to state 1 when read first Just char                                   
-- 2) go to state 2 from state 1 when read Nothing                              
                                                                                
fstTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                      -> Maybe (MachineCfg Char Int)
fstTransCfgForCheck m inp hpos =                                                     
  (head (drop 2 (runRes m inp hpos)))                                      
                                                                                
fstTransCfgCorrect : Machine Char Int -> List (Maybe Char) 
                     -> Maybe (MachineCfg Char Int)
fstTransCfgCorrect m inp = Just (fstTransMCfg m inp)                            
                                                                                
fstTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int     
fstTransMCfg m inp =                                                            
  { currState = 1                                                               
  , tapeCfg = (fstTransTCfg inp)                                                
  }                                                                             
                                                                                
fstTransTCfg : List (Maybe Char) -> TapeCfg Char                                
fstTransTCfg inp =                                                              
  { leftSyms = fromList (take 2 inp)                                 
  , currSym =                                                                   
      case (head (drop 2 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = fromList (drop 3 inp)                                
  }                                                                             
                                                                                
------------------------------------------------------------------------------  
                                                                                
sndTransCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                      -> Maybe (MachineCfg Char Int)
sndTransCfgForCheck m inp hpos =                                                     
  (head (drop 5 (runRes m inp hpos))) -- 6 config in list                  
                                                                                
sndTransCfgCorrect : Machine Char Int -> List (Maybe Char) 
                     -> Maybe (MachineCfg Char Int)
sndTransCfgCorrect m inp = Just (sndTransMCfg m inp)                            
                                                                                
sndTransMCfg : Machine Char Int -> List (Maybe Char) -> MachineCfg Char Int     
sndTransMCfg m inp =                                                            
  { currState = 2                                                               
  , tapeCfg = (sndTransTCfg inp)                                                
  }                                                                             
                                                                                
sndTransTCfg : List (Maybe Char) -> TapeCfg Char                                
sndTransTCfg inp =                                                              
  { leftSyms = fromList (take 3 inp)                                 
  , currSym =                                                                   
      case (head (drop 3 inp)) of                                     
        Just h -> h                                                             
        Nothing -> Nothing                                                      
  , rightSyms = fromList (drop 4 inp)                                
  }   

------------------------------------------------------------------------------  
------------------------------------------------------------------------------


--LAST------------------------------------------------------------------------  
                                                                                
-- Check the last MachineConfig in the list of configs                          

lastCfgForCheck : Machine Char Int -> List (Maybe Char) -> Int 
                  -> Maybe (MachineCfg Char Int)
lastCfgForCheck m inp hpos =                                                         
  (head (reverse (runRes m inp hpos)))                                     
                                                                                
lastCfgCorrect : Machine Char Int -> List (Maybe Char) 
                 -> Maybe (MachineCfg Char Int)
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
  case (head inp) of                                                       
    Just h ->                                                                   
      case h of                                                                 
        Just c ->                                                               
          case (head (drop 1 inp)) of                                 
            Just hh ->                                                          
              case hh of                                                        
                Just ch -> (indexOfLastNum (drop 1 inp) (ind+1))           
                Nothing -> ind                                                  
            Nothing -> -1                                                       
        Nothing -> (indexOfLastNum (drop 1 inp) (ind+1))                   
    Nothing -> -1                                                               
-}                                                                              
                                                                                
lastTCfg : List (Maybe Char) -> TapeCfg Char                                    
lastTCfg inp =                                                                  
    { leftSyms = (fromList (take 2 inp))                             
    , currSym =                                                                 
       case (head (drop 2 inp)) of                                    
          Just sec -> sec                                                       
          Nothing -> Nothing                                                    
    , rightSyms = ( fromList ([Just '1'] ++ (drop 4 inp)) )          
    }  

------------------------------------------------------------------------------  
------------------------------------------------------------------------------ 

tests : Test                                                                    
tests =                                                                         
  suite "A Test Suite"                                                        
  [ test "head"     
    <| assertEqual (headCfgForCheck testMachine input initHeadPos) 
                   (headCfgCorrect testMachine input)
  , test "first transition (see in the middle block)"                                                                 
    <| assertEqual (fstTransCfgForCheck testMachine input initHeadPos)                       
                   (fstTransCfgCorrect testMachine input)  
  , test "second transition (see in the middle block)"                           
    <| assertEqual (sndTransCfgForCheck testMachine input initHeadPos)                    
                   (sndTransCfgCorrect testMachine input)  
  , test "last"                                                                    
    <| assertEqual (lastCfgForCheck testMachine input initHeadPos) 
                   (lastCfgCorrect testMachine input)
  , test "count"
    <| assertEqual (length (runRes testMachine input initHeadPos)) 7
  ] 

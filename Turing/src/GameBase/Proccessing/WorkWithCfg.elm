module GameBase.Proccessing.WorkWithCfg exposing (getTapeFromCfg, getHeadCfg, 
                                                 getAllCfgs, emptyMCfg, 
                                                 getNextCfg)                        

import List exposing (drop, head)
import Array exposing (toList, empty)

import TuringMachine.RunTuring exposing (runMachine)
import TuringMachine.TuringTypes exposing (MachineCfg, TapeCfg, Direction(..))
import GameBase.Data.GameTypes exposing (BallOfWool, Kitten(..), Model)
import GameBase.Proccessing.TranslateTables exposing (getTrTFromUserTrT)

emptyTape : TapeCfg BallOfWool                                                  
emptyTape =                                                                     
  { leftSyms = empty                                                            
  , currSym = Nothing                                                           
  , rightSyms = empty                                                           
  }                                                                             
                                                                                
                                                                                
emptyMCfg : MachineCfg BallOfWool Kitten                                        
emptyMCfg =                                                                     
  { currState = White                                                           
  , currDir = Stay                                                              
  , tapeCfg  = emptyTape                                                        
  }


getTapeFromCfg : Maybe (MachineCfg BallOfWool Kitten) -> List (Maybe BallOfWool)                                     
getTapeFromCfg maybeCfg =                                                       
  case maybeCfg of                                                              
    Just cfg ->                                                                 
      (                                                                         
        (toList cfg.tapeCfg.leftSyms) ++                                        
        [cfg.tapeCfg.currSym] ++                                                
        (toList cfg.tapeCfg.rightSyms)                                          
      )                                                                         
    Nothing -> []  


getHeadCfg : Model -> MachineCfg BallOfWool Kitten                              
getHeadCfg model =                                                              
    case (head (model.machineCfgs)) of                                          
      Just c -> c                                                               
      Nothing -> emptyMCfg                                                      
                                                                                
                                                                                
getAllCfgs : Model -> Model                                                     
getAllCfgs model =                                                              
  let                                                                           
    initCfg  = (getHeadCfg model)
    updModel = (getTrTFromUserTrT model)
  in                                                                            
    { model | machineCfgs = (runMachine updModel.machine initCfg [initCfg])        
    }                                                                           
                                                                                
                                                                                
getNextCfg : Model -> Model                                                     
getNextCfg model =                                                              
  { model | machineCfgs = (drop 1 model.machineCfgs) }     

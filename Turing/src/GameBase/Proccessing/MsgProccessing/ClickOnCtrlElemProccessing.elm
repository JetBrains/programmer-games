module GameBase.Proccessing.MsgProccessing.ClickOnCtrlElemProccessing  exposing 
        (clickTrTableProccessing, clickHelpProccessing, clickRunProccessing)

import GameBase.Proccessing.WorkWithCfg exposing (getAllCfgs)       
import GameBase.Data.GameTypes exposing (Model, Position, BallOfWool(..),
                                         Kitten(..))            
import GameBase.UI.MainObjects.Cat exposing (updCatParam)
import GameBase.UI.TransTable.EmptyCellsCoord exposing (getEmptyCellsCoord)     
import TuringMachine.TuringTypes exposing (Cell(..), Direction(..))
                                                                                
import List exposing (head, drop) 
import Array exposing (get, set)
import Time exposing (Time)


setPushFlag : Model -> Model
setPushFlag model =                                                             
  { model 
      | flags =
          { ifPushRun   = True
          , ifStart     = model.flags.ifStart
          , ifPlay      = model.flags.ifPlay 
          , ifHistory   = model.flags.ifHistory  
          , ifRules     = model.flags.ifRules  
          , ifAuthors   = model.flags.ifAuthors
          , ifEnd       = model.flags.ifEnd 
          , ifCatLooks  = model.flags.ifCatLooks  
          , ifTableFull = model.flags.ifTableFull
          }
  } 


setTime : Time -> Model -> Model                                                
setTime time model =                                                            
  { model                                                                       
      | options = 
          { winSize        = model.options.winSize                                                            
          , timeUnit       = time
          , whenGameStarts = model.options.whenGameStarts                                                     
          , currTime       = model.options.currTime
          , tapeCellsNumb  = model.options.tapeCellsNumb
          } 
  }


-- get State from usedCats array by click number                                
-- provide running through cats arr by click                                    
getStateByClick : Int -> Model -> Cell Kitten                                   
getStateByClick clickNum m =                                                    
  let                                                                           
    len = (Array.length m.usedObj.usedCats)                                             
  in                                                                            
    if clickNum >= len                                                          
      then getStateByClick (clickNum - len) m                                   
    else                                                                        
      case (get clickNum m.usedObj.usedCats) of                                         
        Just state -> state                                                     
        Nothing    -> EmptyCell
                                                                                
                                                                                
-- get Symb from usedBalls array by click number                                  
-- provide running through balls arr by click                                   
getSymbByClick : Int -> Model -> Cell (Maybe BallOfWool)                        
getSymbByClick clickNum m =                                                     
  let                                                                           
    len = (Array.length m.usedObj.usedBalls)                                            
  in                                                                            
    if clickNum >= len                                                          
      then getSymbByClick (clickNum - len) m                                    
    else                                                                        
      case (get clickNum m.usedObj.usedBalls) of                                        
        Just symb -> symb                                                       
        Nothing   -> EmptyCell                                                    
                                                                                
                                                                                
-- get Dir from usedDirs array by click number                                  
-- provide running through dirs arr by click                                    
getDirByClick : Int -> Model -> Cell Direction                                  
getDirByClick clickNum m =                                                      
  let                                                                           
    len = (Array.length m.usedObj.usedDirs)                                             
  in                                                                            
    if clickNum >= len                                                          
      then getDirByClick (clickNum - len) m                                     
    else                                                                        
      case (get clickNum m.usedObj.usedDirs) of                                         
        Just dir -> dir                                                         
        Nothing  -> EmptyCell   


emptyCoord : (Int, Int, Int, Int, Int, String)
emptyCoord = (-1, -1, -1, -1, -1, "")


-- run throught the list of empty cells coordinates and if pos coordinates      
-- are in one of these intervals - return array index of KV for changing        
getIndIfClickOnEmpty :  Position -> List (Int, Int, Int, Int, Int, String) ->   
                        (Int, String)                                           
getIndIfClickOnEmpty pos list =                                                 
  let                                                                           
    (topFrom, topTo, leftFrom, leftTo, arrInd, elemName) =                      
      case (head list) of                                                       
        Just coord -> coord                                                     
        Nothing    -> emptyCoord                                     
                                                                                
  in                                                                            
     if (List.isEmpty list) == False                                            
        then if pos.y >= topFrom && pos.y <= topTo &&                           
                pos.x >= leftFrom && pos.x <= leftTo                            
                then (arrInd, elemName)                                         
             else getIndIfClickOnEmpty pos (drop 1 list)                        
     else (-1, "") 


-- change user transition table when click if click pos coordinates             
-- are the same as some coord of empty cells                                    
clickTrTableProccessing : Model -> Position -> (Model, Cmd msg)               
clickTrTableProccessing m pos =                                                 
  let                                                                           
    emptyCellsCoord = (getEmptyCellsCoord m.transTables.trTableInit [] 0)                   
    (kvIndForChange, elemForChange) =                                           
          (getIndIfClickOnEmpty pos emptyCellsCoord)                            
    kvForChange =                                                               
      case (get kvIndForChange m.transTables.trTableUser) of                                
        Just kv -> kv                                                           
        Nothing -> { key   = (Violet, Nothing)                                    
                   , value = { state = EmptyCell                                
                             , symb  = EmptyCell                                 
                             , dir   = EmptyCell}                                 
                   , clickNum = 0                                               
                   }                                                            
  in
    if kvIndForChange > -1                                                      
       then let                                                                 
              (newState, newSymb, newDir) =                                     
                case elemForChange of                                           
                  "state" ->                                                    
                    ( (getStateByClick kvForChange.clickNum m)                  
                    , kvForChange.value.symb                                    
                    , kvForChange.value.dir)                                    
                  "symb" ->                                                     
                    ( kvForChange.value.state                                   
                    , (getSymbByClick kvForChange.clickNum m)                   
                    , kvForChange.value.dir)                                    
                  _ ->                                                          
                    ( kvForChange.value.state                                   
                    , kvForChange.value.symb                                    
                    , (getDirByClick kvForChange.clickNum m))                   
            in
              ( { m 
                    | transTables = 
                        { trTableInit = m.transTables.trTableInit
                        , trTableUser = 
                            ( set kvIndForChange                         
                                  { key   = kvForChange.key                
                                  , value =                              
                                      { state = newState                  
                                      , symb  = newSymb                   
                                      , dir   = newDir                    
                                      }                                   
                                  , clickNum = kvForChange.clickNum + 1  
                                  }                                      
                                  m.transTables.trTableUser                          
                            )
                        }
                 }                                                               
              , Cmd.none                                                        
              )                                                                 
    else (m, Cmd.none)  


-- draw help img if click on help                                               
clickHelpProccessing : Model -> ( Model, Cmd msg )                              
clickHelpProccessing model =                                                    
  if model.imgParam.helpImg == " "
     then ({model 
              | imgParam = 
                  { catLeft      = model.imgParam.catLeft
                  , menuCatTop   = model.imgParam.menuCatTop
                  , catPos       = model.imgParam.catPos
                  , gameHistPage = model.imgParam.gameHistPage 
                  , catImg       = model.imgParam.catImg
                  , helpImg      = "../img/helpMsg/helpLevel" ++ 
                                   (toString model.levels.currLevel) ++ 
                                   ".png"
                  , finalImg     = model.imgParam.finalImg
                  , gameHistImg  = model.imgParam.gameHistImg
                  }
           }
          , Cmd.none
          )
  else ({ model 
            | imgParam =    
                { catLeft      = model.imgParam.catLeft
                , menuCatTop   = model.imgParam.menuCatTop
                , catPos       = model.imgParam.catPos
                , gameHistPage = model.imgParam.gameHistPage 
                , catImg       = model.imgParam.catImg 
                , helpImg      = " "
                , finalImg     = model.imgParam.finalImg
                , gameHistImg  = model.imgParam.gameHistImg
                }
        }
       , Cmd.none
       )                                                     
       

-- check if table full and run the machine                                      
clickRunProccessing : Model -> Time -> ( Model, Cmd msg )                       
clickRunProccessing model time =                                                
  let                                                                           
    updModel = (getAllCfgs model)                                               
  in                                                                            
    if updModel.flags.ifTableFull == False                                            
       then (updModel, Cmd.none)                                                
    else                                                                        
      ( setPushFlag updModel                                                    
        |> updCatParam time                                                     
        |> setTime time                                                         
      , Cmd.none                                                                
      ) 

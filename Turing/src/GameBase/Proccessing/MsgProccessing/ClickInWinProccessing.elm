module GameBase.Proccessing.MsgProccessing.ClickInWinProccessing exposing 
            (clickInMenuWin, clickInAuthorsRulesWin, clickInGameWin, 
             clickInFinalFinImg, clickInFinalPosImg, clickInFinalNegImg)

import GameBase.Data.GameTypes exposing (Msg(..), Model, Position)              
import GameBase.Data.Init exposing (getInitByLevel)
import GameBase.UI.MainObjects.DivSvgStyles exposing (mainRectW, mainRectH)
import GameBase.UI.TransTable.TransTableDraw exposing 
                                         (trTableW, trTableH)           
import GameBase.UI.TransTable.TransTableMargins exposing (trTableX, trTableY)     
import GameBase.UI.ControlObjects.ControlLabelsParam exposing 
         (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength, 
          sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength)
import GameBase.UI.ControlObjects.ControlElements exposing                      
              (runButtonX, runButtonY, runButtonW, runButtonH, fastRunButtonX, 
               quesButtonX, quesButtonY, quesButtonW, quesButtonH) 
import GameBase.Proccessing.MsgProccessing.ClickOnCtrlElemProccessing exposing                  
        (clickTrTableProccessing, clickHelpProccessing, clickRunProccessing)    
 
import Task exposing (perform)  
import Window exposing (size)
import Time exposing (Time, inSeconds, now, second, millisecond)


playGameFlags : Model -> Model                                                  
playGameFlags model =                                                           
  { model 
      | flags =
          { ifPushRun = model.flags.ifPushRun
          , ifStart = False 
          , ifPlay = True 
          , ifRules = model.flags.ifRules
          , ifAuthors = model.flags.ifAuthors
          , ifEnd = model.flags.ifEnd
          , ifCatLooks = model.flags.ifCatLooks
          , ifTableFull = model.flags.ifTableFull
          }
  }    


clickInMenuWin : Model -> Position -> ( Model, Cmd Msg )                        
clickInMenuWin m pos =
  -- if click "Continue"
  if m.levels.currLevel > 1 && pos.y >= (menuItemTopFrom 0) && 
                               pos.y <= (menuItemTopTo 0) && 
                               pos.x >= menuItemLeftFrom && 
                               pos.x <= fstMenuItemLength 
          then ( (playGameFlags m) 
               , perform (\_ -> Debug.crash "time") Tick now
               )              
  -- if click "New Game"
  else if pos.y >= (menuItemTopFrom 1) && 
          pos.y <= (menuItemTopTo 1) && 
          pos.x >= menuItemLeftFrom && 
          pos.x <= sndMenuItemLength
          then ( (getInitByLevel 1 m)  
                 |> playGameFlags 
               , (perform (\_ -> Debug.crash "time") Tick now)
               )
  -- if click "Rules"
  else if pos.y >= (menuItemTopFrom 2) && 
          pos.y <= (menuItemTopTo 2) && 
          pos.x >= menuItemLeftFrom && 
          pos.x <= thirdMenuItemLength
          then ( { m                                                            
                    | flags =                                                   
                        { ifPushRun = m.flags.ifPushRun                     
                        , ifStart = False                                       
                        , ifPlay = m.flags.ifPlay                           
                        , ifRules = True                         
                        , ifAuthors = m.flags.ifAuthors                                       
                        , ifEnd = m.flags.ifEnd                             
                        , ifCatLooks = m.flags.ifCatLooks                   
                        , ifTableFull = m.flags.ifTableFull                 
                        }        
                 }  
                 , Cmd.none                                                     
               )  
  -- if click "About the authors"
  else if pos.y >= (menuItemTopFrom 3) && 
          pos.y <= (menuItemTopTo 3) && 
          pos.x >= menuItemLeftFrom && 
          pos.x <= fourthMenuItemLength          
          then ({ m                                                                       
                    | flags =                                                                 
                        { ifPushRun = m.flags.ifPushRun                                   
                        , ifStart = False                                                     
                        , ifPlay = m.flags.ifPlay                                                       
                        , ifRules = m.flags.ifRules                                       
                        , ifAuthors = True                                   
                        , ifEnd = m.flags.ifEnd                                           
                        , ifCatLooks = m.flags.ifCatLooks                                 
                        , ifTableFull = m.flags.ifTableFull                               
                        }                                                                     
                } 
               , Cmd.none                                                     
               )  
  else (m, Cmd.none) 


-- AR Win is authors and rules windows
returnFromARWinTopFrom : Int                                                    
returnFromARWinTopFrom = 670                                                                                
                                                                                
returnFromARWinTopTo : Int                                                      
returnFromARWinTopTo = 740                                                      
                                                                                
returnFromARWinLeftFrom : Int                                                   
returnFromARWinLeftFrom = 660
                                                                                
returnFromARWinLeftTo : Int                                                     
returnFromARWinLeftTo = 980 


clickInAuthorsRulesWin : Model -> Position -> ( Model, Cmd Msg )                
clickInAuthorsRulesWin m pos =    
  -- click return lebel in authors or rules window
  if pos.y >= returnFromARWinTopFrom && 
     pos.y <= returnFromARWinTopTo && 
     pos.x >= returnFromARWinLeftFrom && 
     pos.x <= returnFromARWinLeftTo               
     then ( { m                                                                 
                | flags =
                    { ifPushRun = m.flags.ifPushRun                     
                    , ifStart = True
                    , ifPlay = m.flags.ifPlay                           
                    , ifRules = False                         
                    , ifAuthors = False                                      
                    , ifEnd = m.flags.ifEnd                             
                    , ifCatLooks = m.flags.ifCatLooks                   
                    , ifTableFull = m.flags.ifTableFull                 
                    } 
            }                                                                   
          , Cmd.none                                                            
          ) 
  else (m, Cmd.none)  


-- G Win is game window
returnFromGWinTopFrom : Int                                                    
returnFromGWinTopFrom = 20 
                                                                                
returnFromGWinTopTo : Int                                                      
returnFromGWinTopTo = 40
                                                                                
returnFromGWinLeftFrom : Int                                                   
returnFromGWinLeftFrom = 690
                                                                                
returnFromGWinLeftTo : Int                                                     
returnFromGWinLeftTo = 930

clickInGameWin : Model -> Position -> ( Model, Cmd Msg )                        
clickInGameWin m pos =                                                          
  if m.flags.ifCatLooks == True &&
     pos.y >= 0 && pos.y <= mainRectH && pos.x >= 0 && pos.x <= mainRectW                   
     then ( {m                                                                  
              | flags =                                                         
                  { ifPushRun = m.flags.ifPushRun                           
                  , ifStart = m.flags.ifStart                               
                  , ifPlay = m.flags.ifPlay                                 
                  , ifRules = m.flags.ifRules                               
                  , ifAuthors = m.flags.ifAuthors                           
                  , ifEnd = m.flags.ifEnd                                   
                  , ifCatLooks = False                                          
                  , ifTableFull = m.flags.ifTableFull                       
                  }                                                             
              , options =                                                                  
                  { winSize = m.options.winSize                                 
                  , timeUnit = m.options.timeUnit                                                          
                  , whenGameStarts = m.options.currTime                                                                   
                  , currTime = m.options.currTime       
                  , tapeCellsNumb = m.options.tapeCellsNumb
                  }  
            }                                                                   
          , Cmd.none                                                          
          )                                                                     
  -- if return from gameWindow to main menu                                     
  else if pos.y >= returnFromGWinTopFrom && 
          pos.y <= returnFromGWinTopTo && 
          pos.x >= returnFromGWinLeftFrom && 
          pos.x <= returnFromGWinLeftTo
          then ({ m 
                    | flags =
                        { ifPushRun = m.flags.ifPushRun                           
                        , ifStart = True                                
                        , ifPlay = False                                 
                        , ifRules = m.flags.ifRules                               
                        , ifAuthors = m.flags.ifAuthors                           
                        , ifEnd = m.flags.ifEnd                                   
                        , ifCatLooks = False                                          
                        , ifTableFull = m.flags.ifTableFull                       
                        }     
                }
               , Cmd.none             
               )                                                                
  else if pos.y >= trTableY &&                                          
          pos.y <= (trTableY + trTableH) &&                        
          pos.x >= trTableX &&                                         
          pos.x <= (trTableX + trTableW)                           
          then (clickTrTableProccessing m pos)                                  
  else if pos.y >= runButtonY && 
          pos.y <= runButtonY + runButtonH && 
          pos.x >= runButtonX && 
          pos.x <= runButtonX + runButtonW
          then (clickRunProccessing m second)                                   
  else if pos.y >= runButtonY && 
          pos.y <= runButtonY + runButtonH && 
          pos.x >= fastRunButtonX && 
          pos.x <= fastRunButtonX + runButtonW          
          then (clickRunProccessing m millisecond)                              
  else if pos.y >= quesButtonY && 
          pos.y <= quesButtonY + quesButtonH && 
          pos.x >= quesButtonX && 
          pos.x <= quesButtonX + quesButtonW          
          then (clickHelpProccessing m)                                         
  else (m, Cmd.none) 


-- G Win is game window                                                         
returnFromFFImgTopFrom : Int                                                     
returnFromFFImgTopFrom = 455 
                                                                                
returnFromFFImgTopTo : Int                                                       
returnFromFFImgTopTo = 490                                                        
                                                                                
returnFromFFImgLeftFrom : Int                                                    
returnFromFFImgLeftFrom = 200                                                    
                                                                                
returnFromFFImgLeftTo : Int                                                      
returnFromFFImgLeftTo = 840 


clickInFinalFinImg : Model -> Position -> ( Model, Cmd Msg )                    
clickInFinalFinImg m pos =                                                      
  if pos.y >= returnFromFFImgTopFrom && 
     pos.y <= returnFromFFImgTopTo && 
     pos.x >= returnFromFFImgLeftFrom && 
     pos.x <= returnFromFFImgLeftTo               
     then ( (getInitByLevel 1 m)
          , Cmd.none                  
          )                                                                     
  else (m, Cmd.none)                                                            
                      

goNextLevTopFrom : Int
goNextLevTopFrom = 220

goNextLevelTopTo : Int
goNextLevelTopTo = 250

goNextLevelLeftFrom : Int
goNextLevelLeftFrom = 240

goNextLevelLeftTo : Int
goNextLevelLeftTo = 860


clickInFinalPosImg : Model -> Position -> ( Model, Cmd Msg )                    
clickInFinalPosImg m pos =                                                      
  if pos.y >= goNextLevTopFrom && 
     pos.y <= goNextLevelTopTo && 
     pos.x >= goNextLevelLeftFrom && 
     pos.x <= goNextLevelLeftTo  
     then ( (getInitByLevel m.levels.currLevel m)
            |> playGameFlags
          , Cmd.none                 
          )                                                                     
  else (m, Cmd.none)                                                            
                                                                                

tryAgainTopFrom : Int
tryAgainTopFrom = 590 

tryAgainTopTo : Int
tryAgainTopTo = 630

tryAgainLeftFrom : Int
tryAgainLeftFrom = 440

tryAgainLeftTo : Int
tryAgainLeftTo = 860


clickInFinalNegImg : Model -> Position -> ( Model, Cmd Msg )                    
clickInFinalNegImg m pos =                                                      
  if pos.y >= tryAgainTopFrom && 
     pos.y <= tryAgainTopTo && 
     pos.x >= tryAgainLeftFrom && 
     pos.x <= tryAgainLeftTo   
     then ( (getInitByLevel m.levels.currLevel m)
            |> playGameFlags
          , Cmd.none
          )                                                                     
  else (m, Cmd.none)  

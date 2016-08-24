module GameBase.Proccessing.MsgProccessing.MsgProccessing exposing 
      (clickMsgProccessing, moveMsgProccessing, tickMsgProccessing)

import GameBase.Proccessing.WorkWithCfg exposing (getNextCfg)
import GameBase.Proccessing.CheckResult exposing (checkResult)
import GameBase.Proccessing.MsgProccessing.ClickInWinProccessing exposing                         
                 (clickInGameWin, clickInGameHistWin,
                  clickInMenuWin, clickInAuthorsRulesWin,
                  clickInFinalFinImg, clickInFinalPosImg, clickInFinalNegImg) 
import GameBase.Data.GameTypes exposing (Msg(..), Model, Position)
import GameBase.UI.MainObjects.Cat exposing (updCatParam, menuCatY)
import GameBase.UI.ControlObjects.ControlLabelsParam exposing                                      
        (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength,   
         sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength)  

import Time exposing (Time, inSeconds, now, second, millisecond)


-- proccessing user clicks in different windows, on different control elemets
clickMsgProccessing : Model -> Position -> ( Model, Cmd Msg )                   
clickMsgProccessing m pos =
  -- in menu window
  if m.flags.ifStart == True 
     then (clickInMenuWin m pos)
  -- in authors, rules windows
  else if m.flags.ifAuthors == True || m.flags.ifRules == True
          then (clickInAuthorsRulesWin m pos)
  -- if win all levels                                                          
  else if m.flags.ifEnd       == True && 
          m.imgParam.finalImg == "../img/finalImg/final.png"    
          then (clickInFinalFinImg m pos)
  -- if go to the next level                                                    
  else if m.flags.ifEnd       == True && 
          m.imgParam.finalImg == "../img/finalImg/pos.jpg"  
          then (clickInFinalPosImg m pos)
  -- if go to the current level again                                           
  else if m.flags.ifEnd       == True && 
          m.imgParam.finalImg == "../img/finalImg/neg.png"    
          then (clickInFinalNegImg m pos)
  -- in game history window
  else if m.flags.ifHistory == True
          then (clickInGameHistWin m pos) 
  -- in game window
  else if m.flags.ifPlay == True
          then (clickInGameWin m pos) 
  else (m, Cmd.none) 


-- proccessing cat`s movements (cursor movements) in menu window
moveMsgProccessing : Model -> Position -> ( Model, Cmd Msg )                    
moveMsgProccessing m pos =                                                      
  if m.flags.ifStart == True                                                     
     -- if click "Continue"
     then if m.levels.currLevel > 1 && 
                                pos.y >= (menuItemTopFrom 0) &&
                                pos.y <= (menuItemTopTo 0) && 
                                pos.x >= menuItemLeftFrom &&
                                pos.x <= fstMenuItemLength
                  then ( { m                                                    
                            | imgParam = 
                                { catLeft      = m.imgParam.catLeft
                                , menuCatTop   = (menuCatY 0)
                                , catPos       = m.imgParam.catPos
                                , gameHistPage = m.imgParam.gameHistPage
                                , catImg       = m.imgParam.catImg
                                , helpImg      = m.imgParam.helpImg
                                , finalImg     = m.imgParam.finalImg
                                , gameHistImg  = m.imgParam.gameHistImg
                                }
                          }                                                     
                       , Cmd.none                                               
                       )                                                      
          -- if click "New Game"             
          else if pos.y >= (menuItemTopFrom 1) && pos.y <= (menuItemTopTo 1) &&
                  pos.x >= menuItemLeftFrom && pos.x <= sndMenuItemLength
                  then ( { m    
                            | imgParam =
                                { catLeft      = m.imgParam.catLeft                     
                                , menuCatTop   = (menuCatY 1)
                                , catPos       = m.imgParam.catPos  
                                , gameHistPage = m.imgParam.gameHistPage
                                , catImg       = m.imgParam.catImg                       
                                , helpImg      = m.imgParam.helpImg                  
                                , finalImg     = m.imgParam.finalImg  
                                , gameHistImg  = m.imgParam.gameHistImg
                                } 
                          }   
                       , Cmd.none   
                       ) 
          -- if click "Rules"                                                       
          else if pos.y >= (menuItemTopFrom 2) && pos.y <= (menuItemTopTo 2) &&
                  pos.x >= menuItemLeftFrom && pos.x <= thirdMenuItemLength  
                  then ( { m                                                    
                            | imgParam = 
                                { catLeft      = m.imgParam.catLeft                  
                                , menuCatTop   = (menuCatY 2)                     
                                , catPos       = m.imgParam.catPos      
                                , gameHistPage = m.imgParam.gameHistPage
                                , catImg       = m.imgParam.catImg                    
                                , helpImg      = m.imgParam.helpImg                  
                                , finalImg     = m.imgParam.finalImg
                                , gameHistImg  = m.imgParam.gameHistImg
                                }   
                          }   
                       , Cmd.none   
                       )
          -- if click "About the authors"
          else if pos.y >= (menuItemTopFrom 3) && pos.y <= (menuItemTopTo 3) && 
                  pos.x >= menuItemLeftFrom && pos.x <= fourthMenuItemLength
                  then ( { m  
                            | imgParam = 
                                { catLeft      = m.imgParam.catLeft                  
                                , menuCatTop   = (menuCatY 3)                     
                                , catPos       = m.imgParam.catPos 
                                , gameHistPage = m.imgParam.gameHistPage 
                                , catImg       = m.imgParam.catImg                    
                                , helpImg      = m.imgParam.helpImg                  
                                , finalImg     = m.imgParam.finalImg
                                , gameHistImg  = m.imgParam.gameHistImg
                                }   
                          } 
                       , Cmd.none     
                       )
          else (m, Cmd.none)    
  else (m, Cmd.none)


timeLimitForFillingGaps : Float
timeLimitForFillingGaps = 60


-- proccessing time parameters for checking result, 
-- getting next cfg for drawing, drawing CatLooks
tickMsgProccessing : Model -> Time -> ( Model, Cmd Msg )                        
tickMsgProccessing m time =                                                     
  if m.options.whenGameStarts == 0 && m.flags.ifPlay == True
     then ( { m                                                                 
                | options =                                          
                    { winSize        = m.options.winSize
                    , timeUnit       = m.options.timeUnit
                    , whenGameStarts = time
                    , currTime       = time
                    , tapeCellsNumb  = m.options.tapeCellsNumb   
                    } 
            }                                                                   
          , Cmd.none                                                            
          )                                                                     
  else if m.options.whenGameStarts > 0 && m.flags.ifPlay == True &&                           
          m.flags.ifPushRun == False                                                  
          then if ((inSeconds m.options.currTime) - 
                   (inSeconds m.options.whenGameStarts)) > 
                                                  timeLimitForFillingGaps 
                  then ({m 
                          | flags =  
                              { ifPushRun   = m.flags.ifPushRun
                              , ifStart     = m.flags.ifStart
                              , ifPlay      = m.flags.ifPlay
                              , ifHistory   = m.flags.ifHistory
                              , ifRules     = m.flags.ifRules
                              , ifAuthors   = m.flags.ifAuthors
                              , ifEnd       =  m.flags.ifEnd
                              , ifCatLooks  = True 
                              , ifTableFull = m.flags.ifTableFull
                              }
                          , options = 
                              { winSize        = m.options.winSize 
                              , timeUnit       = m.options.timeUnit
                              , whenGameStarts = m.options.whenGameStarts
                              , currTime       = time   
                              , tapeCellsNumb  = m.options.tapeCellsNumb
                              }
                        }
                       , Cmd.none
                       )     
               else ({m 
                        | options =
                            { winSize        = m.options.winSize                     
                            , timeUnit       = m.options.timeUnit                   
                            , whenGameStarts = m.options.whenGameStarts       
                            , currTime       = time 
                            , tapeCellsNumb  = m.options.tapeCellsNumb
                            } 
                     }
                    , Cmd.none
                    )                           
  else if m.flags.ifPlay == True && m.flags.ifPushRun == True                               
     then if (List.length m.modelMachine.machineCfgs) > 1                                         
             then ( (getNextCfg m)                                              
                    |> updCatParam time                                         
                  , Cmd.none                                                    
                  )                                                             
          else (checkResult m time)                                             
  else                                                                          
    ({m
        | options =
            { winSize        = m.options.winSize                     
            , timeUnit       = m.options.timeUnit                   
            , whenGameStarts = m.options.whenGameStarts       
            , currTime       = time
            , tapeCellsNumb  = m.options.tapeCellsNumb
            } 
     }
    , Cmd.none
    )

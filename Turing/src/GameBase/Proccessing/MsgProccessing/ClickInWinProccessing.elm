module GameBase.Proccessing.MsgProccessing.ClickInWinProccessing exposing 
               (clickInGameWin, clickInGameHistWin,
                clickInMenuWin, clickInAuthorsRulesWin, 
                clickInFinalFinImg, clickInFinalPosImg, clickInFinalNegImg)

import GameBase.Data.GameTypes exposing (Msg(..), Model, Position)              
import GameBase.Data.Init exposing (getInitByLevel, getInitByLevelWithOldTable)
import GameBase.UI.MainObjects.DivSvgStyles exposing (mainRectW, mainRectH)
import GameBase.UI.TransTable.TransTableDraw exposing 
                                         (trTableW, trTableH)           
import GameBase.UI.TransTable.TransTableMargins exposing (trTableX, trTableY)     
import GameBase.UI.ControlObjects.ControlLabelsParam exposing 
         (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength, 
          sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength)
import GameBase.UI.ControlObjects.ControlElements exposing                      
              (runButtonX, runButtonY, runButtonW, runButtonH, fastRunButtonX)
               --quesButtonX, quesButtonY, quesButtonW, quesButtonH) 
import GameBase.Proccessing.MsgProccessing.ClickOnCtrlElemProccessing exposing                  
        (clickTrTableProccessing, clickRunProccessing) --clickHelpProccessing)    
 
import Task exposing (perform)  
import Window exposing (size)
import Time exposing (Time, inSeconds, now, second, millisecond)


playGameFlags : Model -> Model                                                  
playGameFlags model =                                                           
  { model 
      | flags =
          { ifPushRun   = model.flags.ifPushRun
          , ifStart     = False 
          , ifPlay      = True                                      
          , ifHistory   = False 
          , ifRules     = model.flags.ifRules
          , ifAuthors   = model.flags.ifAuthors
          , ifEnd       = model.flags.ifEnd
          , ifCatLooks  = model.flags.ifCatLooks
          , ifTableFull = model.flags.ifTableFull
          }
  }


showNextHistoryModel : Model -> Model
showNextHistoryModel model =
  let                                                                           
    nextPageNum = model.imgParam.gameHistPage + 1                                   
  in 
    { model 
        | flags =                                                                 
            { ifPushRun   = model.flags.ifPushRun 
            , ifStart     = False 
            , ifPlay      = False 
            , ifHistory   = True
            , ifRules     = model.flags.ifRules                                       
            , ifAuthors   = model.flags.ifAuthors                                   
            , ifEnd       = model.flags.ifEnd                                           
            , ifCatLooks  = model.flags.ifCatLooks
            , ifTableFull = model.flags.ifTableFull 
            }
        , imgParam =                                        
            { catLeft      = model.imgParam.catLeft             
            , menuCatTop   = model.imgParam.menuCatTop          
            , catPos       = model.imgParam.catPos              
            , gameHistPage = nextPageNum                     
            , catImg       = model.imgParam.catImg              
            , helpImg      = model.imgParam.helpImg             
            , finalImg     = model.imgParam.finalImg            
            , gameHistImg  = "../img/windows/gameHistory" ++ 
                             (toString nextPageNum) ++ ".jpg"                      
            }   
    }


showPrevHistoryModel : Model -> Model                                           
showPrevHistoryModel model =                                                    
  let                                                                           
    prevPageNum = model.imgParam.gameHistPage - 1                               
  in                                                                            
    { model                                                                     
        | flags =                                                               
            { ifPushRun   = model.flags.ifPushRun                               
            , ifStart     = False                                               
            , ifPlay      = False                                               
            , ifHistory   = True                                                
            , ifRules     = model.flags.ifRules                                 
            , ifAuthors   = model.flags.ifAuthors                               
            , ifEnd       = model.flags.ifEnd                                   
            , ifCatLooks  = model.flags.ifCatLooks                              
            , ifTableFull = model.flags.ifTableFull                             
            }                                                                   
        , imgParam =                                                            
            { catLeft      = model.imgParam.catLeft                             
            , menuCatTop   = model.imgParam.menuCatTop                          
            , catPos       = model.imgParam.catPos                              
            , gameHistPage = prevPageNum                                        
            , catImg       = model.imgParam.catImg                              
            , helpImg      = model.imgParam.helpImg                             
            , finalImg     = model.imgParam.finalImg                            
            , gameHistImg  = "../img/windows/gameHistory" ++                    
                             (toString prevPageNum) ++ ".jpg"                   
            }                                                                   
    }


updHPageAfterInit : Int -> Model -> Model
updHPageAfterInit pageNumb model =
  { model 
      | imgParam =                                                            
          { catLeft      = model.imgParam.catLeft                             
          , menuCatTop   = model.imgParam.menuCatTop                          
          , catPos       = model.imgParam.catPos                              
          , gameHistPage = pageNumb                                        
          , catImg       = model.imgParam.catImg                              
          , helpImg      = model.imgParam.helpImg                             
          , finalImg     = model.imgParam.finalImg                            
          , gameHistImg  = "../img/windows/gameHistory" ++                    
                           (toString pageNumb) ++ ".jpg"                   
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
                 |> showNextHistoryModel 
                 -- dont start time because catLooks img should not be showed 
                 -- during history
               , Cmd.none
               )
  -- if click "Rules"
  else if pos.y >= (menuItemTopFrom 2) && 
          pos.y <= (menuItemTopTo 2) && 
          pos.x >= menuItemLeftFrom && 
          pos.x <= thirdMenuItemLength
          then ( { m                                                            
                    | flags =                                                   
                        { ifPushRun   = m.flags.ifPushRun                     
                        , ifStart     = False                                       
                        , ifPlay      = m.flags.ifPlay                           
                        , ifHistory   = m.flags.ifHistory
                        , ifRules     = True                         
                        , ifAuthors   = m.flags.ifAuthors                                       
                        , ifEnd       = m.flags.ifEnd                             
                        , ifCatLooks  = m.flags.ifCatLooks                   
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
                        { ifPushRun   = m.flags.ifPushRun                                   
                        , ifStart     = False                                                     
                        , ifPlay      = m.flags.ifPlay                                                       
                        , ifHistory   = m.flags.ifHistory 
                        , ifRules     = m.flags.ifRules                                       
                        , ifAuthors   = True                                   
                        , ifEnd       = m.flags.ifEnd                                           
                        , ifCatLooks  = m.flags.ifCatLooks                                 
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
                    { ifPushRun   = m.flags.ifPushRun                     
                    , ifStart     = True
                    , ifPlay      = m.flags.ifPlay  
                    , ifHistory   = m.flags.ifHistory
                    , ifRules     = False                         
                    , ifAuthors   = False                                      
                    , ifEnd       = m.flags.ifEnd                             
                    , ifCatLooks  = m.flags.ifCatLooks                   
                    , ifTableFull = m.flags.ifTableFull                 
                    } 
            }                                                                   
          , Cmd.none                                                            
          ) 
  else (m, Cmd.none)


-------------------- 
prevButtonTopFrom : Int                                                         
prevButtonTopFrom = 650                                                         
                                                                                
prevButtonTopTo : Int                                                           
prevButtonTopTo = prevButtonTopFrom + prevButtonH                               
                                                                                
prevButtonLeftFrom : Int                                                        
prevButtonLeftFrom = 85 
                                                                                
prevButtonLeftTo : Int                                                          
prevButtonLeftTo = prevButtonLeftFrom + prevButtonW                             
                                                                                
prevButtonW : Int                                                               
prevButtonW = 180 
                                                                                
prevButtonH : Int                                                               
prevButtonH = 20  
--------------------
                                        
skipButtonTopFrom : Int 
skipButtonTopFrom = 650
                                                                                
skipButtonTopTo : Int
skipButtonTopTo = skipButtonTopFrom + skipButtonH

skipButtonLeftFrom : Int  
skipButtonLeftFrom = 515 

skipButtonLeftTo : Int
skipButtonLeftTo = skipButtonLeftFrom + skipButtonW

skipButtonW : Int 
skipButtonW = 90 
                                                                                
skipButtonH : Int 
skipButtonH = 20
-------------------- 

nextButtonTopFrom : Int                                                         
nextButtonTopFrom = 650
                                                                                
nextButtonTopTo : Int                                                           
nextButtonTopTo = nextButtonTopFrom + nextButtonH                                                              
                                                                                
nextButtonLeftFrom : Int                                                        
nextButtonLeftFrom = 850
                                                                                
nextButtonLeftTo : Int                                                          
nextButtonLeftTo = nextButtonLeftFrom + nextButtonW 

nextButtonW : Int                                                               
nextButtonW = 90
                                                                                
nextButtonH : Int                                                               
nextButtonH = 20 
--------------------


clickInGameHistWin : Model -> Position -> ( Model, Cmd Msg )                
clickInGameHistWin m pos =
  -- click Previous button
  if pos.y >= prevButtonTopFrom  &&                                        
     pos.y <= prevButtonTopTo    &&                                        
     pos.x >= prevButtonLeftFrom &&                                        
     pos.x <= prevButtonLeftTo                                             
     then if m.imgParam.gameHistPage == 1  ||      
             m.imgParam.gameHistPage == 9  ||     
             m.imgParam.gameHistPage == 11 ||     
             m.imgParam.gameHistPage == 13 ||     
             m.imgParam.gameHistPage == 15 ||      
             m.imgParam.gameHistPage == 17 ||      
             m.imgParam.gameHistPage == 19         
             then (m, Cmd.none)
             else                                                             
                 ( (showPrevHistoryModel m) -- go to prev hist img                  
                 , Cmd.none                                                     
                 )  
  -- click Skip button
  else if pos.y >= skipButtonTopFrom  &&                                         
          pos.y <= skipButtonTopTo    &&                                           
          pos.x >= skipButtonLeftFrom &&                                        
          pos.x <= skipButtonLeftTo                                             
          then if m.imgParam.gameHistPage < 8
                  then ({ m 
                            | imgParam =                                                              
                                { catLeft      = m.imgParam.catLeft                               
                                , menuCatTop   = m.imgParam.menuCatTop                            
                                , catPos       = m.imgParam.catPos                                
                                , gameHistPage = m.imgParam.gameHistPage + 
                                                 (8 - m.imgParam.gameHistPage)
                                , catImg       = m.imgParam.catImg                                
                                , helpImg      = m.imgParam.helpImg                               
                                , finalImg     = m.imgParam.finalImg                              
                                , gameHistImg  = m.imgParam.gameHistImg                        
                                }     
                        }
                        |> playGameFlags
                       , perform (\_ -> Debug.crash "time") Tick now
                       )
               else if m.imgParam.gameHistPage < 10 && 
                       m.imgParam.gameHistPage > 8
                  then ({ m                                                     
                            | imgParam =                                      
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (10 - m.imgParam.gameHistPage)  
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       ) 
               else if m.imgParam.gameHistPage < 12 &&                          
                       m.imgParam.gameHistPage > 10                              
                  then ({ m                                                     
                            | imgParam =                                        
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (12 - m.imgParam.gameHistPage) 
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )   
               else if m.imgParam.gameHistPage < 14 &&                          
                       m.imgParam.gameHistPage > 12                             
                  then ({ m                                                     
                            | imgParam =                                        
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (14 - m.imgParam.gameHistPage) 
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )  
               else if m.imgParam.gameHistPage < 16 &&                          
                       m.imgParam.gameHistPage > 14                             
                  then ({ m                                                     
                            | imgParam =                                        
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (16 - m.imgParam.gameHistPage) 
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )  
               else if m.imgParam.gameHistPage < 18 &&                          
                       m.imgParam.gameHistPage > 16                             
                  then ({ m                                                     
                            | imgParam =                                        
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (18 - m.imgParam.gameHistPage) 
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )
               else if m.imgParam.gameHistPage < 21 &&                          
                       m.imgParam.gameHistPage > 18                             
                  then ({ m                                                     
                            | imgParam =                                        
                                { catLeft      = m.imgParam.catLeft             
                                , menuCatTop   = m.imgParam.menuCatTop          
                                , catPos       = m.imgParam.catPos              
                                , gameHistPage = m.imgParam.gameHistPage +      
                                                 (21 - m.imgParam.gameHistPage) 
                                , catImg       = m.imgParam.catImg              
                                , helpImg      = m.imgParam.helpImg             
                                , finalImg     = m.imgParam.finalImg            
                                , gameHistImg  = m.imgParam.gameHistImg         
                                }                                               
                        }                                                       
                        |> playGameFlags                                        
                       , perform (\_ -> Debug.crash "time") Tick now            
                       )  
       else
          ( (playGameFlags m)    
          , perform (\_ -> Debug.crash "time") Tick now  
          )   
  -- click Next button                                                          
  else if pos.y >= nextButtonTopFrom  &&       
          pos.y <= nextButtonTopTo    &&   
          pos.x >= nextButtonLeftFrom &&   
          pos.x <= nextButtonLeftTo  
          -- go from hist to game
          then if m.imgParam.gameHistPage == 8  || -- after start history 
                  m.imgParam.gameHistPage == 10 || -- after block 1 history
                  m.imgParam.gameHistPage == 12 || -- after block 2 history
                  m.imgParam.gameHistPage == 14 || -- after block 3 history 
                  m.imgParam.gameHistPage == 16 || -- after block 4 history
                  m.imgParam.gameHistPage == 18 || -- after block 5 history
                  m.imgParam.gameHistPage == 21    -- after block 6 history  
                  then
                    ( (playGameFlags m)   
                    , perform (\_ -> Debug.crash "time") Tick now                                                    
                    )     
               else   
                 ( (showNextHistoryModel m) -- go to next hist img
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
                  { ifPushRun   = m.flags.ifPushRun                           
                  , ifStart     = m.flags.ifStart                               
                  , ifPlay      = m.flags.ifPlay 
                  , ifHistory   = m.flags.ifHistory  
                  , ifRules     = m.flags.ifRules                               
                  , ifAuthors   = m.flags.ifAuthors                           
                  , ifEnd       = m.flags.ifEnd                                   
                  , ifCatLooks  = False                                          
                  , ifTableFull = m.flags.ifTableFull                       
                  }                                                             
              , options =                                                                  
                  { winSize        = m.options.winSize                                 
                  , timeUnit       = m.options.timeUnit                                                          
                  , whenGameStarts = m.options.currTime                                                                   
                  , currTime       = m.options.currTime       
                  , tapeCellsNumb  = m.options.tapeCellsNumb
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
                        { ifPushRun   = m.flags.ifPushRun                           
                        , ifStart     = True                                
                        , ifPlay      = False  
                        , ifHistory   = m.flags.ifHistory  
                        , ifRules     = m.flags.ifRules                               
                        , ifAuthors   = m.flags.ifAuthors                           
                        , ifEnd       = m.flags.ifEnd                                   
                        , ifCatLooks  = False                                          
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
{--  else if pos.y >= quesButtonY && 
          pos.y <= quesButtonY + quesButtonH && 
          pos.x >= quesButtonX && 
          pos.x <= quesButtonX + quesButtonW          
          then (clickHelpProccessing m) --}                                         
  else (m, Cmd.none) 


returnFromFFImgTopFrom : Int                                                     
returnFromFFImgTopFrom = 0 
                                                                                
returnFromFFImgTopTo : Int                                                       
returnFromFFImgTopTo = mainRectH                                                        
                                                                                
returnFromFFImgLeftFrom : Int                                                    
returnFromFFImgLeftFrom = 0                                                    
                                                                                
returnFromFFImgLeftTo : Int                                                      
returnFromFFImgLeftTo = mainRectW 


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
goNextLevTopFrom = 0

goNextLevelTopTo : Int
goNextLevelTopTo = mainRectH

goNextLevelLeftFrom : Int
goNextLevelLeftFrom = 0

goNextLevelLeftTo : Int
goNextLevelLeftTo = mainRectW


clickInFinalPosImg : Model -> Position -> ( Model, Cmd Msg )                    
clickInFinalPosImg m pos =    
  let
    lastPageViewed = m.imgParam.gameHistPage
  in 
    if pos.y >= goNextLevTopFrom && 
       pos.y <= goNextLevelTopTo && 
       pos.x >= goNextLevelLeftFrom && 
       pos.x <= goNextLevelLeftTo  
       then 
         -- go to history img after last level in block
         -- m.levels.currLevel is the next level number there
         if m.levels.currLevel == 2  || -- if the next lev is 1_1 show history
            m.levels.currLevel == 5  || -- if the next lev is 2_1 show history
            m.levels.currLevel == 8  || -- if the next lev is 3_1 show history
            m.levels.currLevel == 17 || -- if the next lev is 4_1 show history
            m.levels.currLevel == 24 || -- if the next lev is 5_1 show history
            m.levels.currLevel == 32    -- if the next lev is 6_1 show history 
            then
              ( (getInitByLevel m.levels.currLevel m)
                |> updHPageAfterInit lastPageViewed
                |> showNextHistoryModel 
              , Cmd.none                 
              )  
         else
           ( (getInitByLevel m.levels.currLevel m)   
             |> updHPageAfterInit lastPageViewed
             |> playGameFlags                                               
           , Cmd.none                                                          
           )   
    else (m, Cmd.none)                                                            
                                                                                

tryAgainTopFrom : Int
tryAgainTopFrom = 0 

tryAgainTopTo : Int
tryAgainTopTo = mainRectH

tryAgainLeftFrom : Int
tryAgainLeftFrom = 0

tryAgainLeftTo : Int
tryAgainLeftTo = mainRectW


clickInFinalNegImg : Model -> Position -> ( Model, Cmd Msg )                    
clickInFinalNegImg m pos =      
  let                                                                           
    lastPageViewed = m.imgParam.gameHistPage                                    
  in     
    if pos.y >= tryAgainTopFrom && 
       pos.y <= tryAgainTopTo && 
       pos.x >= tryAgainLeftFrom && 
       pos.x <= tryAgainLeftTo   
       then ( (getInitByLevelWithOldTable m.levels.currLevel m)
              |> updHPageAfterInit lastPageViewed
              |> playGameFlags
            , Cmd.none
            )                                                                     
    else (m, Cmd.none)  

module GameBase.UI.TransTable.TransTableMargins exposing (trTableLeftMargin, 
                                               trTableTopMargin, 
                                               cellLeftMargin, cellTopMargin)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))     
import TuringMachine.TuringTypes exposing (UserValue)                      


trTableLeftMargin : Int                                                         
trTableLeftMargin = 380                                                         
    

trTableTopMargin : Int                                                          
trTableTopMargin = 60 


-- elem [0..2], column [0..2]                                                   
-- symb is 0 elem, dir is 1 elem, state is 2 elem                               
fromParamToLeftMargin : Int -> Int -> Int                                       
fromParamToLeftMargin elem column =                                             
  trTableLeftMargin + 155 + column*80 + elem*20                                 
                                                                                
                                                                                
-- ifDir is 0 or 1 (bool), row [0..4]                                           
-- "dir" is 1, _ is 0                                                           
fromParamToTopMargin : Int -> Int -> Int                                        
fromParamToTopMargin ifDir row =                                                
  trTableTopMargin + 40 + ifDir*20 + row*40                                     
                                                                                
                                                                                
-- get left margin from key parameters                                          
cellLeftMargin : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten       
                 , String) -> Int                                               
cellLeftMargin key =                                                            
  case key of                                                                   
    ((White, sym), new, "symb") -> (fromParamToLeftMargin 0 0)                  
    ((White, sym), new, "state") -> (fromParamToLeftMargin 2 0)                 
    ((White, sym), new, _) -> (fromParamToLeftMargin 1 0)                       
    ((LightGrey, sym), new, "symb") -> (fromParamToLeftMargin 0 1)              
    ((LightGrey, sym), new, "state") -> (fromParamToLeftMargin 2 1)             
    ((LightGrey, sym), new, _) -> (fromParamToLeftMargin 1 1)                   
    ((Grey, sym), new, "symb") -> (fromParamToLeftMargin 0 2)                   
    ((Grey, sym), new, "state") -> (fromParamToLeftMargin 2 2)                  
    ((Grey, sym), new, _) -> (fromParamToLeftMargin 1 2)                        
    ((st, sym), new, _) -> (fromParamToLeftMargin 0 3)                          
                                                                                
                                                                                
-- get top margin from key parameters                                           
cellTopMargin : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten        
                , String) -> Int                                                
cellTopMargin key =                                                             
  case key of                                                                   
    ((st, Just Red), new, "dir") -> (fromParamToTopMargin 1 0)                  
    ((st, Just Red), new, s) -> (fromParamToTopMargin 0 0)                      
    ((st, Just Yellow), new, "dir") -> (fromParamToTopMargin 1 1)               
    ((st, Just Yellow), new, s) -> (fromParamToTopMargin 0 1)                   
    ((st, Just Green), new, "dir") -> (fromParamToTopMargin 1 2)                
    ((st, Just Green), new, s) -> (fromParamToTopMargin 0 2)                    
    ((st, Just Blue), new, "dir") -> (fromParamToTopMargin 1 3)                 
    ((st, Just Blue), new, s) -> (fromParamToTopMargin 0 3)                     
    ((st, Nothing), new, "dir") -> (fromParamToTopMargin 1 4)                   
    ((st, Nothing), new, s) -> (fromParamToTopMargin 0 4)                       

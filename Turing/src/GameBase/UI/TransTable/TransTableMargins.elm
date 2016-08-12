module GameBase.UI.TransTable.TransTableMargins exposing (trTableX, trTableY, 
                                                          cellX, cellY)

import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))     
import TuringMachine.TuringTypes exposing (UserValue)                      


trTableX : Int                                                         
trTableX = 370
    

trTableY : Int                                                          
trTableY = 70 


-- elem [0..2], column [0..2]                                                   
-- symb is 0 elem, dir is 1 elem, state is 2 elem                               
fromParamToLeftMargin : Int -> Int -> Int                                       
fromParamToLeftMargin elem column =                                             
  trTableX + 210 + column*105 + elem*20
                                                                                
                                                                                
-- ifDir is 0 or 1 (bool), row [0..4]                                           
-- "dir" is 1, _ is 0                                                           
fromParamToTopMargin : Int -> Int -> Int                                        
fromParamToTopMargin ifDir row =                                                
  trTableY + 55 + ifDir*25 + row*55
                                                                                
                                                                                
-- get left margin from key parameters                                          
cellX : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten       
                 , String) -> Int                                               
cellX key =                                                            
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
    ((Brown, sym), new, "symb") -> (fromParamToLeftMargin 0 3)                   
    ((Brown, sym), new, "state") -> (fromParamToLeftMargin 2 3)                  
    ((Brown, sym), new, _) -> (fromParamToLeftMargin 1 3)     
    ((st, sym), new, _) -> (fromParamToLeftMargin 0 3)                          
                                                                                
                                                                                
-- get top margin from key parameters                                           
cellY : ((Kitten, Maybe BallOfWool), UserValue BallOfWool Kitten        
                , String) -> Int                                                
cellY key =                                                             
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

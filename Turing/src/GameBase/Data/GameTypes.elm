module GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..), Position, Msg(..), Model)

import Time exposing (Time)
import Window exposing (Size)

import TuringMachine.TuringTypes exposing (Machine, UserTransTable, MachineCfg)


--white is start, orange is natural, violet is reject                           
type BallOfWool = Red | Yellow | Green | Blue -- a  
type Kitten = White | LightGrey | Grey | Orange | Violet -- b   


type alias Position =                                                           
  { x : Int, y : Int }                                                          


-- MODEL                                                                        
type alias Model =                                                              
  { -- options                                                                  
    windSize       : Size -- for WindowsSize message                            
  , timeUnit       : Time                                                       
  , whenGameStarts : Time                                                       
  , currTime       : Time                                                       
  -- machine                                                                    
  , input        : List (Maybe BallOfWool)                                      
  , machine      : Machine BallOfWool Kitten                                    
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)                          
  --tables                                                                      
  , trTableInit  : UserTransTable BallOfWool Kitten                                 
  , trTableUser  : UserTransTable BallOfWool Kitten                                 
  --pictures                                                                    
  , catLeft      : Int    -- different for catImg                               
  , menuCatTop   : Int                                                          
  , catPos       : Int                                                          
  , catImg       : String -- catPush, catThink                                  
  , helpImg      : String -- help text                                          
  , finalImg     : String                                                       
  --levels and result                                                           
  , currLevel    : Int                                                          
  , maxLevel     : Int                                                          
  -- expected results                                                           
  , expPos       : Int                                                          
  , expRes       : List (Maybe BallOfWool)                                      
  -- flags                                                                      
  , ifPushRun    : Bool                                                         
  , ifStart      : Bool                                                         
  , ifPlay       : Bool                                                         
  , ifRules      : Bool                                                         
  , ifAuthors    : Bool                                                         
  , ifEnd        : Bool                                                         
  , ifCatLooks   : Bool 
  , ifTableFull  : Bool
  }  


-- MESSAGES                                                                     
type Msg                                                                        
  = Click Position                                                              
  | Move Position                                                               
  | WindowSize Size                                                             
  | Tick Time  

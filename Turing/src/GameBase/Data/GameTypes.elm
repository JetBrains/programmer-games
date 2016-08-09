module GameBase.Data.GameTypes exposing 
                        (BallOfWool(..), Kitten(..), Position, Msg(..), Model, 
                         ModelOptions, ModelMachine, ModelTransTables, 
                         ModelImgParam, ModelLevels, ModelExpResults, 
                         ModelFlags, ModelObjectsSet)

import TuringMachine.TuringTypes exposing 
                (Machine, UserTransTable, MachineCfg, Cell(..), Direction(..))

import Time exposing (Time)
import Window exposing (Size)
import Array exposing (Array) 


--white is start, orange is natural, violet is reject                           
type BallOfWool = Red | Yellow | Green | Blue -- a  
type Kitten = White | LightGrey | Grey | Orange | Violet -- b   


type alias Position =                                                           
  { x : Int, y : Int }                                                          


-- MODEL
type alias Model =  
  { options      : ModelOptions  
  , modelMachine : ModelMachine
  , transTables  : ModelTransTables
  , imgParam     : ModelImgParam 
  , levels       : ModelLevels
  , expResults   : ModelExpResults 
  , flags        : ModelFlags
  , usedObj      : ModelObjectsSet
  }  


type alias ModelOptions = 
  { winSize       : Size -- for WindowsSize message                            
  , timeUnit       : Time -- seconds or milliseconds                                                      
  , whenGameStarts : Time -- for looking cat drawing                                                      
  , currTime       : Time -- for looking cat drawing  
  }


type alias ModelMachine =
  { input        : List (Maybe BallOfWool)                                      
  , machine      : Machine BallOfWool Kitten                                    
  , machineCfgs  : List (MachineCfg BallOfWool Kitten)     
  }


type alias ModelTransTables =
  { trTableInit  : UserTransTable BallOfWool Kitten                             
  , trTableUser  : UserTransTable BallOfWool Kitten  
  }


type alias ModelImgParam = 
  { catLeft      : Int    -- different for catImg                               
  , menuCatTop   : Int    -- for menu cat                                                      
  , catPos       : Int    -- for game cat                                                      
  , catImg       : String -- catPush, catThink                                  
  , helpImg      : String -- help text                                          
  , finalImg     : String -- pos, neg, final
  }


type alias ModelLevels = 
  { currLevel    : Int                                                          
  , maxLevel     : Int  
  }


type alias ModelExpResults = 
  { expPos       : Int                                                          
  , expRes       : List (Maybe BallOfWool)  
  }


type alias ModelFlags =
  { ifPushRun    : Bool -- if run button was pushed                                                       
  , ifStart      : Bool -- if menu window                                                         
  , ifPlay       : Bool -- if game window                                                       
  , ifRules      : Bool -- if rules window                                                       
  , ifAuthors    : Bool -- if authors window                                                       
  , ifEnd        : Bool -- if final img window                                                       
  , ifCatLooks   : Bool -- if drawing looking cat                                                       
  , ifTableFull  : Bool -- if user fill all gaps 
  }


type alias ModelObjectsSet =
  { usedCats  : Array (Cell Kitten) -- different for levels                                           
  , usedBalls : Array (Cell (Maybe BallOfWool))                                 
  , usedDirs  : Array (Cell Direction) 
  }


-- MESSAGES                                                                     
type Msg                                                                        
  = Click Position  -- for all clicks                                                            
  | Move Position   -- for menu cat                                                            
  | WindowSize Size -- window size change                                                            
  | Tick Time       -- for machine run and looking cat

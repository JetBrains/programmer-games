module GameBase.UI.TransTable.EmptyCellsCoord exposing (getEmptyCellsCoord)

import GameBase.UI.TransTable.CellsDraw exposing (cellHeight, cellWidth)
import GameBase.UI.TransTable.TransTableMargins exposing (cellLeftMargin, cellTopMargin) 
import GameBase.Data.GameTypes exposing (BallOfWool(..), Kitten(..))     
import TuringMachine.TuringTypes exposing (Cell(..), UserKeyValue, 
                                           UserTransTable) 

import Array exposing (get, length)


ifSymbEmpty : (UserKeyValue BallOfWool Kitten) -> Int -> 
               List (Int, Int, Int, Int, Int, String)     
ifSymbEmpty userKV arrInd =                                                            
  case userKV.value.symb of                                                     
    EmptyCell ->                                                                
      let                                                                       
        topFrom = cellTopMargin (userKV.key, userKV.value, "symb")              
        topTo = topFrom + cellHeight                                            
        leftFrom = cellLeftMargin (userKV.key, userKV.value, "symb")            
        leftTo = leftFrom + cellWidth                                           
      in                                                                        
        [(topFrom, topTo, leftFrom, leftTo, arrInd, "symb")]                                   
    _ -> []                                                                     
                                                                                
                                                                                
ifStateEmpty : (UserKeyValue BallOfWool Kitten) -> Int -> 
                List (Int, Int, Int, Int, Int, String)    
ifStateEmpty userKV arrInd =                                                           
  case userKV.value.state of                                                    
    EmptyCell ->                                                                
      let                                                                       
        topFrom = cellTopMargin (userKV.key, userKV.value, "state")             
        topTo = topFrom + cellHeight                                            
        leftFrom = cellLeftMargin (userKV.key, userKV.value, "state")           
        leftTo = leftFrom + cellWidth                                           
      in                                                                        
        [(topFrom, topTo, leftFrom, leftTo, arrInd, "state")]                                   
    _ -> []                                                                     
                                                                                
                                                                                
ifDirEmpty : (UserKeyValue BallOfWool Kitten) -> Int -> 
              List (Int, Int, Int, Int, Int, String)      
ifDirEmpty userKV arrInd =                                                         
  case userKV.value.dir of                                                      
    EmptyCell ->                                                                
      let                                                                       
        topFrom = cellTopMargin (userKV.key, userKV.value, "dir")               
        topTo = topFrom + cellHeight                                            
        leftFrom = cellLeftMargin (userKV.key, userKV.value, "dir")             
        leftTo = leftFrom + cellWidth                                           
      in                                                                        
        [(topFrom, topTo, leftFrom, leftTo, arrInd, "dir")]                                   
    _ -> [] 


ifEmptyComputeCoord : Maybe (UserKeyValue BallOfWool Kitten) -> Int -> 
                  List (Int, Int, Int, Int, Int, String)                                     
ifEmptyComputeCoord maybeUserKV arrInd =                                               
  case maybeUserKV of                                                           
    Just userKV ->                                                              
      (ifSymbEmpty userKV arrInd) ++                                                   
      (ifStateEmpty userKV arrInd) ++                                                  
      (ifDirEmpty userKV arrInd)                                                       
    Nothing -> []                                                               
                                                                                
                                                                                
getEmptyCellsCoord : UserTransTable BallOfWool Kitten ->                        
                     List (Int, Int, Int, Int, Int, String) -> Int ->  
                     List (Int, Int, Int, Int, Int, String)     
getEmptyCellsCoord table res ind =                                                  
  let                                                                           
    updRes = res ++ (ifEmptyComputeCoord (get ind table) ind)                        
    len = (length table)                                                        
  in                                                                            
    if (ind < len)
      then (getEmptyCellsCoord table updRes (ind+1))                      
    else res      

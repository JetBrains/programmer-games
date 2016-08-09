module GameBase.UI.ControlObjects.ControlLabelsParam exposing 
        (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength, 
         sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength)


menuItemTopFrom : Int -> Int                                                    
menuItemTopFrom ind =                                                           
  190 + ind * 57                                                                


menuItemTopTo : Int -> Int                                                      
menuItemTopTo ind =                                                             
  (menuItemTopFrom ind) + 27                                                    


menuItemLeftFrom : Int                                                          
menuItemLeftFrom = 360                                                          


fstMenuItemLength : Int                                                         
fstMenuItemLength = 525                                                         


sndMenuItemLength : Int                                                         
sndMenuItemLength = 525                                                         


thirdMenuItemLength : Int                                                       
thirdMenuItemLength = 460                                                       


fourthMenuItemLength : Int                                                      
fourthMenuItemLength = 713    

module GameBase.UI.ControlObjects.ControlLabelsParam exposing 
        (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength, 
         sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength, 
         fstItemTopFrom, menuItemsInterval)


fstItemTopFrom : Int 
fstItemTopFrom = 250


menuItemsInterval : Int 
menuItemsInterval = 70


menuItemHeight : Int
menuItemHeight = 27


menuItemTopFrom : Int -> Int 
menuItemTopFrom ind =
  fstItemTopFrom + ind * menuItemsInterval


menuItemTopTo : Int -> Int 
menuItemTopTo ind =  
  (menuItemTopFrom ind) + menuItemHeight


menuItemLeftFrom : Int 
menuItemLeftFrom = 460  


fstMenuItemLength : Int  
fstMenuItemLength = 670


sndMenuItemLength : Int    
sndMenuItemLength = 670   


thirdMenuItemLength : Int  
thirdMenuItemLength = 585  


fourthMenuItemLength : Int  
fourthMenuItemLength = 910

module GameBase.UI.ControlObjects.ControlLabelsParam exposing 
        (menuItemTopFrom, menuItemTopTo, menuItemLeftFrom, fstMenuItemLength, 
         sndMenuItemLength, thirdMenuItemLength, fourthMenuItemLength, 
         fstItemTopFrom, menuItemsInterval)


fstItemTopFrom : Int 
fstItemTopFrom = 180


menuItemsInterval : Int 
menuItemsInterval = 70


menuItemHeight : Int
menuItemHeight = 30


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
sndMenuItemLength = 690   


thirdMenuItemLength : Int  
thirdMenuItemLength = 630  


fourthMenuItemLength : Int  
fourthMenuItemLength = 690

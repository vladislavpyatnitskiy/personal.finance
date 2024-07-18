salary.calculator.uk <- function(x){ # Calculator for UK salaries
  
  if (isTRUE(x <= 12570)){ # When salary is below or equal to £12,570
    
    sprintf("Your salary would be %s annually or %s monthly after tax", x) } 
  
  else if (isTRUE(x >= 12571) && isTRUE(x <= 50270)){ # > £12,571 & < £50,270
    
    sprintf("Your salary would be %s annually or %s monthly after tax",
            12571 + (x - 12571) * .8, (12571 + (x - 12571) * .8) / 12) }
  
  else if (isTRUE(x >= 50271) && isTRUE(x <= 125140)){ # > £50,271 & < £125,140
    
    sprintf("Your salary would be %s annually or %s monthly after tax",
            12571 + (50270 - 12571) * .8 + (x - 50271) * .6,
            (12571 + (50270 - 12571) * .8 + (x - 50271) * .6) / 12) } 
  
  else { # > £125,140
    
    sprintf("Your salary would be %s annually or %s monthly after tax",
            12571 + (50270-12571) * .8 + (125140-50271) * .6 + (x-125140) * .55,
            (12571+(50270-12571)*.8+(125140-50271)*.6+(x-125140)*.55) / 12) }
}
salary.calculator.uk(60000) # Test

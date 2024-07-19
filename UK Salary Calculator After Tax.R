salary.calculator.uk <- function(x){ # Calculator for UK salaries after tax
  
  if (isTRUE(x <= 12570)){ y <- c(x, round(x / 12, 2)) } # =< £12,570
  
  else if (isTRUE(x >= 12571) && isTRUE(x <= 50270)){ # > £12,571 & < £50,270
    
    y <- c(2514.2 + x * .8, round((2514.2 + x * .8) / 12, 2)) } 
  
  else if (isTRUE(x >= 50271) && isTRUE(x <= 125140)){ # > £50,271 & < £125,140
    
    y <- c(12567.6 + x * .6, round((12567.6 + x * .6) / 12, 2)) } 
  
  else { y <- c(18824.6 + x*.55, round((18824.6 + x*.55)/12, 2)) } # > £125,140
  
  sprintf("Salary would be %s annually or %s monthly after tax", y[1], y[2])
}
salary.calculator.uk(152000) # Test

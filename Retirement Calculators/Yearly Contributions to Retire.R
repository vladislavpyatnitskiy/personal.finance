retire.contributions <- function(C, rd, t, rr){
  
  # Input:
  # C - amount you want to save in total 
  # rd - discount rate
  # t - time period
  # rr - return rate
  
  # Output: Amount you have to save each year
  
  C * (1 + rd) ^ t / (1 + rr) / ((1 - (1 + rr) ^ t) / (1 - (1 + rr)))
}
retire.contributions(1000000, .03, 40, .05) # Test

retire.contributions <- function(C, rd, t, rr, y = NULL){
  
  # Input:
  # C - amount you want to save in total 
  # rd - discount rate
  # t - time period
  # rr - return rate
  
  # Output: Amount you have to save each year
  
  if (is.null(y)){ y = as.numeric(strsplit(as.character(Sys.Date()),
                                           split = "-")[[1]][1]) }
  R <- 1 + rr # Return Rate
  
  payment = C * (1 + rd) ^ t / R / ((1 - R ^ t) / (1 - R))
  
  D = cbind(seq(t, 1), seq(y, y + t - 1), (C / t), (C / t) * R ^ seq(t, 1))
  
  rownames(D) <- seq(nrow(D)) # Standard Numeric Row Names
  
  colnames(D) <- c("Years to retirement", "Year", "Expected Contributions",
                   "Future Values of Contributions")
  
  df <- list(sum(D[,4]), payment, D) # Put all in a single list
  
  names(df) <- c("Future Value", "Yearly Payment", "Table") # Names
  
  df # Display
}
retire.contributions(1000000, .03, 40, .05, 2024) # Test

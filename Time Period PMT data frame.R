# data frame with time periods and monthly amounts
pmt.df.period <- function(a, r) { l <- NULL 

  for (t in seq(1, 50, 1)){ l <- rbind(l, (a*r/12)/(1 - (1 + r/12)^(-t*12))) }

  l <- cbind(seq(1, 50, 1), l) # Add data frame with time periods
  
  rownames(l) <- seq(nrow(l)) # Set numbers as row names
  
  colnames(l) <- c("Period", "Amount") # Set column names
  
  l # Display
}
pmt.df.period(200000, 0.045) # Test

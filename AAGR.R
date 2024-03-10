AAGR <- function(x){ # Average Annual Growth Rate 
  
  l <- NULL # Store values
  
  for (n in 1:length(x)){ while (x[n] != x[length(x)]){ # Add values
    
      l <- c(l, round((x[n + 1] / x[n]) - 1, 2)) # Till last number
    
      break } } # Stop when cycle is over
  
  l # Display
}
AAGR(c(1000, 1200, 900, 1600, 1800, 2250)) # Test

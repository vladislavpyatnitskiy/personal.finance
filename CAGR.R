CAGR <- function(x){ # Compound Annual Growth Rate 
  
  l <- NULL # Store values
  
  for (n in 1:length(x)){ while (x[n] != x[length(x)]){ # Add values
    
      l <- c(l, round((x[n + 1] / x[1]) ^ (1 / n) - 1, 2)) # Till last number
    
    break } } # Stop when cycle is over
  
  l # Display
}
CAGR(c(1000, 1200, 900, 1600, 1800, 2250)) # Test

CAGR <- function(x){ # Compound Annual Growth Rate 
  
  l <- NULL # Store values
  
  for (n in 1:(length(x) - 1)) l <- c(l, round((x[n+1] / x[1]) ^ (1/n) - 1, 2))
  
  l # Display
}
CAGR(c(1000, 1200, 900, 1600, 1800, 2250)) # Test

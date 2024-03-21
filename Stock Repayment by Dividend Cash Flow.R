dividend.cf <- function(x, y, f){ # How much time needed for stock to be repaid
  
  cf.sum <- 0 # Original cash flow value
  
  while (x > cf.sum){ cf.sum <- cf.sum + y } # Add until cash flow sum < price
  
  sprintf("It will take %s years for stock to be repaid by dividends",
          cf.sum / (y * f)) # Display period
}
dividend.cf(51, 0.51, 4) # Test

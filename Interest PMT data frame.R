# Function to get df with interest and monthly mortgage payment
pmt.df.interest <- function(a, t) { l <- NULL
  
  for (r in seq(0, .5, .01)){ l <- rbind(l, (a*r/12)/(1-(1 + r/12)^(-t*12))) }
  
  l <- cbind(seq(0, .5, .01), l) # Add data frame with interests
  
  l[1,2] <- a / (t * 12) # Insert right monthly amount
  
  colnames(l) <- c("Interest", "Amount")
  
  l # Display
}
pmt.df.interest(200000, 30) # Test

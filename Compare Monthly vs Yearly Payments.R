monthly.vs.yearly.payment <- function(x, m, r, P){ # Compare plans
  
  # Calculate Monthly and Yearly plans
  M <- sum(rep(-x, m) / ((1 + r) ^ (1 / m)) ^ (seq(along = rep(-x, m)) - 1)) 
  Y <- sum(rep(-P / m, m) * ((1 + r) ^ (1 / m))^(seq(along = rep(-P / m, m)))) 
  
  D <- round(abs(Y - M), 2) # Calculate difference and show beneficial one
  
  if (M > Y){ sprintf("Subscribe to a monthly payment as it is %s cheaper", D)
    
    } else { sprintf("Subscribe to a yearly payment as it is %s cheaper", D) }
}
monthly.vs.yearly.payment(x = 6.99, m = 12, r = 0.05, P = 69.99) # Test

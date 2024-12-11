pmt.comp <- function(x, m, r, P){ # Compare monthly and yearly plans
  
  M <- sum(rep(x, m) / ((1 + r) ^ (1 / m)) ^ (seq(along = rep(x, m)) - 1)) 
  Y <- sum(rep(P / m, m) * ((1 + r) ^ (1 / m)) ^ (seq(along = rep(P / m, m))))
  
  l <- paste("Subscribe to a %s plan as it is %s %% cheaper")
  
  ifelse(M > Y, sprintf(l, "yearly", round(abs((Y - M) / M), 2) * 100),
         sprintf(l, "monthly", round(abs((M - Y) / Y), 2) * 100))
}
pmt.comp(x = 6.99, m = 12, r = 0.05, P = 69.99) # Test

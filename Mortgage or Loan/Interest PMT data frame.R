pmt.df.interest <- function(a, t, overpayment = F, max = 0.3) { 
  
  l <- NULL # Data Frame of interest & payment

  for (r in seq(0,max,.01)){ if (r==0){ l<-rbind(l,cbind(0,a/(t*12))) } else {
    
      l <- rbind(l, cbind(r * 100, (a * r/12)/(1 - (1 + r/12)^(-t * 12)))) } }
  
  colnames(l) <- c("Interest (%)", "Monthly Payment") # Column names
  rownames(l) <- seq(nrow(l)) # Numeric Row Names
  
  ifelse(overpayment == T, summary(lm(l[,2] ~ l[,1]))$coefficient[2], return(l))
}
pmt.df.interest(200000, 30, T, 0.1) # Data frame or average overpayment amount

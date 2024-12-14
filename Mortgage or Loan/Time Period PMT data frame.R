pmt.df.period <- function(a, r, i = .05) { 
  
  l <- NULL # Mortgage without excessive overpayment

  for (t in seq(1,40,1)){ l<-rbind(l, cbind(t,(a*r/12)/(1-(1+r/12)^(-t*12)))) }
  
  rownames(l) <- seq(nrow(l)) # Set numbers as row names
  colnames(l) <- c("Period", "Amount") # Set column names
  
  a <- c(3, 6, 9)
  b <- c("'000", "'000,000", "'000,000,000")
  
  D <- l
  
  for (n in 1:(length(a) - 1)){
    
    if (max(l[,2]) > 10^a[n] && max(l[,2]) < 10^a[n+1]){ D[,2] <- D[,2]/10^a[n]
    
      B <- sprintf("Monthly Payment in %s", b[n]) } else { next } }
  
  L <- NULL # Iterate over rows to find the highest ratio under the conditions
  
  for (n in 1:(nrow(l) - 2)){ 
    
    if ((l[n,2]/l[n+1,2] > 1+i) && (l[n+1,2]/l[n+2,2] < 1+i)){ L <- l[n,2]
    
      d <- D[n,2]
    
    break } } # Stop at the first valid match as per the logic
  
  P <- plot(l[,1], D[,2], type = "l", xlab = "Mortgage Period", ylab = B,
            las = 1, main = "Optimal Period and Monthly Payment for Mortgage")
  
  abline(v = l[which(l == L, arr.ind = T)[1],1], lwd = 3, col = "red")
  
  par(mar = c(5, 5, 5, 5)) # Define borders of the plot
  
  list(l[which(l == L, arr.ind = T),][1,], l, P)
}
pmt.df.period(120000, .05, .01) # Test

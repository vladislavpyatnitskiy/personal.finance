mortgage.pmt.calculator <- function(a,r,t,c="£"){ # mortgage payment calculator
  
  sprintf("Payment for %s%s mortgage for %s year at %s%% is %s%s a month",
          c, a, t, r * 100, c, round((a*r/12) / (1 - (1 + r/12)^(-t*12)), 2))
}
mortgage.pmt.calculator(120000, 0.05, 30) # Test

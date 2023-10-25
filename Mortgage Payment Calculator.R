# Function to calculate monthly mortgage payment
mortgage.payment.calculator <- function(a, r, t, c = "£") {
  
  m.payment <- round((a * r / 12) / (1 - (1 + r / 12) ^ (-t * 12)), 2)
  
  sprintf("Payment for %s %s mortgage for %s year at %s%% is %s%s a month",
          c, a, t, r * 100, c, m.payment)
}
# Test
mortgage.payment.calculator(200000, 0.045, 30)

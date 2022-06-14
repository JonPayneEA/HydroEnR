#Urban adjustments

# LCV Urban Adjustment
Urb.Lcv<- function (x, URBEXT2000, DeUrb = FALSE) 
{
  if (DeUrb == FALSE) {
    x * 0.68654^(1.567 * URBEXT2000)
  }
  else {
    x/(0.68654^(1.567 * URBEXT2000))
  }
}

Urb.LSkew <- function (x, URBEXT2000, DeUrb = FALSE) 
{
  if (DeUrb == FALSE) {
    ((x + 1) * 1.096017^(1.567 * URBEXT2000)) - 1
  }
  else {
    ((x + 1)/1.096017^(1.567 * URBEXT2000)) - 1
  }
}

Urb <- function(x,...) {
  UseMethod('Urb', x)
}

# Accounts for missing capital U
urb <- function(x,...) {
  UseMethod('Urb', x)
}

# L calcs
Ls <- function(x, URBEXT2000 = NULL, DeUrb = FALSE, ...) {
  L1 <- L1(x)
  Lcv <- LCV(x)
  LSkew <- LSKEW(x)
  QMED <- QMED(x)
  if(!is.null(URBEXT2000)){
    Lcv <- Urb(Lcv, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
    LSkew <- Urb(LSkew, URBEXT2000 = URBEXT2000, DeUrb = DeUrb)
  }
  x <- data.frame(L1, Lcv, LSkew, QMED)
  class(x) <- append(class(x), 'Ls')
  return(x)
}

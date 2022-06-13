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

Lmoments <- function(x) {
  
  camp <- sort(x)
  n <- length(camp)
  
  nn <- rep(n-1,n)
  pp <- seq(0,n-1)
  p1 <- pp/nn
  p2 <- p1 * (pp-1)/(nn-1)
  p3 <- p2 * (pp-2)/(nn-2)
  
  b0 <- sum(camp)/n
  b1 <- sum(p1*camp)/n
  b2 <- sum(p2*camp)/n
  b3 <- sum(p3*camp)/n
  
  l1 <- b0
  l2 <- 2*b1-b0
  lcv <- 2*b1/b0-1
  lca <- 2*(3*b2-b0)/(2*b1-b0)-3
  lkur <- 5*(2*(2*b3-3*b2)+b0)/(2*b1-b0)+6
  
  Lmom <- c(l1,l2,lcv,lca,lkur)
  names(Lmom) <- c("l1","l2","lcv","lca","lkur")
  
  return(Lmom)
}
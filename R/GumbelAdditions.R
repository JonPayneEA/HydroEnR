GumbelGF <- function(lcv, RP){
  B <- lcv/(log(2)-lcv*(0.5772+log(log(2))))
  gf <- 1+B*(log(log(2))-log(-log(1-(1/RP))))
  return(gf)
}

GumbelEst <- function(loc, scale, q = NULL,RP = 100){
  if(is.null(q) == TRUE) {res <- loc+scale*(-log(-log(1-(1/RP))))}
  else {
    Prob <- 1- exp(-exp(-(q - loc)/scale))
    res <- 1/Prob}
  return(res)
}

GumbelPars <- function(x = NULL, mle = FALSE, L1, LCV){
  if(is.null(x) == FALSE & is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  if(mle == FALSE) {
    if(is.null(x)) {Scale <- (L1*LCV)/log(2)
    Loc <- L1 - 0.5772*Scale
    return(data.frame(Loc, Scale))
    } else {
      L1 <- mean(x, na.rm = TRUE)
      LCV <- Lcv(x)
      Scale <- (L1*LCV)/log(2)
      Loc <- L1 - 0.5772*Scale
      return(data.frame(Loc, Scale))
    }
  } else {
    pars <- c(mean(x), sd(x)/1.5)
    max.lhd <- function(q, par) {
      abs(sum(log(gum.pdf(q, loc = par[1], scale = par[2]))))
    }
    gum.pdf <- function(q, loc, scale) {
      p <- scale^-1 * exp(-(q-loc)/scale)*exp(-exp(-(q-loc)/scale))
      return(p)
    }
    result <- suppressWarnings(optim(par = pars, fn = max.lhd, q = x))
    loc <- result$par[1]
    scale <- result$par[2]
    log.likelihood <- -result$value[1]
    message <- result$message
    Res <- data.frame(loc, scale, log.likelihood)
    return(Res)
  }
}

GumbelAM <- function(x, RP = 100, q = NULL, trend = FALSE){
  if(is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  Sort.x <- sort(x)
  Rank <- seq(1, length(x))
  b0 <- mean(x, na.rm = TRUE)
  b1 <- mean((Rank-1)/(length(x)-1)*Sort.x, na.rm = TRUE)
  L1 <- b0
  L2 <- 2*b1-b0
  a <- L2/log(2)
  loc <- L1 - 0.5772*a
  if(is.null(q) == TRUE) {res <- loc - a * log(-log(1-(1/RP)))}
  else {
    Prob <- 1- exp(-exp(-(q - loc)/a))
    res <- 1/Prob
  }
  m <- function(i, j) {sum((i-mean(i))*(j-mean(j)))/sum((i-mean(i))^2)}
  M <- m(i = seq(1, length(x)), j = x)
  b <- mean(x)-M*mean(seq(1,length(x)))
  LM <- (M*(length(x))+b)-(M*median(seq(1,length(x)))+b)
  LocTrend <- loc+LM
  if (is.null(q) == TRUE) {resTrend <- LocTrend - a * log(-log(1-(1/RP)))}
  else {
    ProbTrend <- 1- exp(-exp(-(q - LocTrend)/a))
    resTrend <- 1/ProbTrend}
  if(trend == FALSE) {return(res)} else {return(resTrend)}
}

GoTF1 <- function(x, dist = "GenLog", pars = NULL, GF = NULL, RepDist = NULL){
  if(is.numeric(x) == FALSE) {stop("x must be a numeric vector")}
  if(is.null(RepDist) == FALSE) {
    if(length(RepDist) != (5000*length(x))) {print("Warning: RepDist not equal to 5000 * length(x), resampling has been used")}
    if(length(RepDist) != (5000*length(x))) {RepDist <- sample(RepDist, 5000*length(x), replace = TRUE)}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75, na.rm = TRUE)])/mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.1 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf | Ind == 0) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf | res == 0) {res <- "< 0.0002"} else {res <- res/0.5}
    
    TailMean <- function(x) {mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.2 <- matrix(RepDist, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf | Ind2 == 0) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf | res2 == 0) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}
    
    ResDF <- data.frame(res, res2)
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)
    
  } else {
    
    if(dist == "GenLog") {funcX <- GenLogAM
    funcPars <- GenLogEst
    funcGF <- GenLogGF}
    if(dist == "GEV")
    {funcX <- GEVAM
    funcPars <- GEVEst
    funcGF <- GEVGF}
    if(dist == "Gumbel")
    {funcX <- GumbelAM
    funcPars <- GumbelEst
    funcGF <- GumbelGF}
    if(dist == "GenPareto")
    {funcX <- GenParetoPOT
    funcPars <- GenParetoEst
    funcGF <- GenParetoGF}
    MMR <- function(x) {sd(x[x > quantile(x, 0.75, na.rm = TRUE)])/mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Rands <- 1/runif(length(x)*5000)
    if(is.null(pars) == TRUE & is.null(GF) == TRUE) {Sims <- funcX(x, RP = Rands)}
    if(is.null(pars) == FALSE) {
      if(dist == "Gumbel") {Sims <- funcPars(pars[1], pars[2], RP = Rands)} else
      {Sims <- funcPars(pars[1], pars[2], pars[3], RP = Rands)}}
    if(is.null(GF) == FALSE)  {
      if(dist == "Gumbel") {Sims <- funcGF(GF[1], RP = Rands)*GF[3]} else
      {Sims <- funcGF(GF[1], GF[2], RP = Rands)*GF[3]}}
    
    Mat.1 <- matrix(Sims, nrow = length(x), ncol = 5000)
    MMRs <- apply(Mat.1, 2, MMR)
    MMRo <- MMR(x)
    SortMMRs <- sort(MMRs)
    Ind <- suppressWarnings(min(which(SortMMRs >  MMRo)))
    if(Ind == Inf | Ind == 0) {Prop <- 0.0002} else {Prop <- Ind/5000}
    if(Prop > 0.5) {res <- 1-Prop} else {res <- Prop}
    if(Ind == Inf | res == 0) {res <- "< 0.0002"} else {res <- res/0.5}
    
    TailMean <- function(x) {mean(x[x > quantile(x, 0.75, na.rm = TRUE)])}
    Mat.2 <- matrix(Sims, nrow = length(x), ncol = 5000)
    TMs <- apply(Mat.1, 2, TailMean)
    TMo <- TailMean(x)
    SortTMs <- sort(TMs)
    Ind2 <- suppressWarnings(min(which(SortTMs >  TMo)))
    if(Ind2 == Inf | Ind2 == 0) {Prop2 <- 0.0002} else {Prop2 <- Ind2/5000}
    if(Prop2 > 0.5) {res2 <- 1-Prop2} else {res2 <- Prop2}
    if(Ind2 == Inf | res2 == 0) {res2 <- "< 0.0002"} else {res2 <- res2/0.5}
    if(class(res) == "character") {res <- res} else {res <- round(res, 4)}
    if(class(res2) == "character") {res2 <- res2} else {res2 <- round(res2, 4)}
    ResDF <- data.frame(res, res2)
    colnames(ResDF) <- c("p(Tail cv)", "p(Tail mean)")
    return(ResDF)
    
  }
}

EVPlot1 <- function(x, dist = "GenLog", scaled = TRUE, Title = "Extreme value plot", ylabel = NULL, LineName = NULL, Unc = TRUE) {
  if(class(x) != "numeric") stop ("x must be a numeric vector")
  if(dist == "GenLog") {func <- GenLogGF}
  if(dist == "GEV") {func <- GEVGF}
  if(dist == "GenPareto") {func <- GenParetoGF}
  if(dist == "Gumbel") {func <- GumbelGF}
  Ranks <- seq(500, 1)
  Gringorten <- function(Rank, n) {(Rank-0.44)/(n+0.12)}
  Gring <- Gringorten(Ranks, 500)
  Log.Red.Var <- log((1/Gring)-1)
  RPs <- 1/Gring
  Ranks.obs <- seq(length(x), 1)
  Gring.obs <- Gringorten(Ranks.obs, length(x))
  LRV.obs <- log((1/Gring.obs)-1)
  Scale <- x/median(x)
  if(scaled == TRUE) {AM.sort <- sort(Scale, decreasing = F)} else {AM.sort <- sort(x, decreasing = F)}
  ss.lcv <- Lcv(x)
  ss.lskew <- LSkew(x)
  if(scaled == TRUE) {
    if(dist == "Gumbel") {SimSS <- func(ss.lcv, RP = 1/Gring)} else {
      SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)}} else {
        if(dist == "Gumbel") {SimSS <- func(ss.lcv, RP = 1/Gring)*median(x)} else
        {SimSS <- func(ss.lcv, ss.lskew, RP = 1/Gring)*median(x)}}
  if(is.null(ylabel) == TRUE) {
    if(scaled == TRUE) {YLab <- "Q/QMED"} else {YLab <- "Discharge (m3/s)"}} else {YLab = ylabel}
  Ymax <- median(c(max(AM.sort), max(SimSS)))
  UpperYRange <- (Ymax-median(AM.sort))
  UpperObsRange <- (max(AM.sort)-median(AM.sort))
  LowerYRange <- median(AM.sort)-min(AM.sort)
  ymin <- median(AM.sort)-(UpperObsRange)
  if(Ymax < max(AM.sort)) {Ymax <- max(AM.sort)} else {Ymax <- Ymax}
  if(LowerYRange > 0.143*UpperYRange) {ymin <- min(AM.sort)} else {ymin <- ymin}
  
  plot(Log.Red.Var, SimSS, type = "l", xlim = c(min(LRV.obs),7), ylim = c(ymin, Ymax), main = Title, ylab = YLab, xlab = "logistic reduced variate", lwd = 2)
  points(LRV.obs, AM.sort, col = "blue", lwd = 1.5)
  if(Unc == FALSE) {
    if(is.null(LineName) == TRUE) {
      if(scaled == FALSE) {legend("topleft", legend = c("Frequency curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)} else {legend("topleft", legend = c("Growth curve", "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
    } else {legend("topleft", legend = c(LineName, "Observed"), col = c("black", "blue"), lty = c(1,0), pch = c(NA, 1), bty = "n", lwd = c(2,NA), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
  } else {
    if(is.null(LineName) == TRUE) {
      if(scaled == FALSE) {legend("topleft", legend = c("Frequency curve", "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)} else {legend("topleft", legend = c("Growth curve", "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
    } else {legend("topleft", legend = c(LineName, "Observed", "95% Intervals"), col = c("black", "blue", "black"), lty = c(1,0,3), pch = c(NA, 1, NA), bty = "n", lwd = c(2,NA,2), pt.lwd = 1.5, seg.len = 2, x.intersp = 0.8, y.intersp = 0.8, cex = 0.8)}
  }
  T.Plot.Lab <- c(2,5,10,20,50,100, 500)
  At <- log(T.Plot.Lab-1)
  AxisPos <- median(c(ymin, median(SimSS)))
  axis(side = 1, at = At, pos = AxisPos, lty = 1, tck = -0.02, labels = T.Plot.Lab, cex.axis = 0.7, padj = -1.5)
  TextY <- as.numeric(quantile(seq(ymin, median(SimSS), by = abs(ymin/10)), 0.86))
  text(2, TextY, labels = "Return Period (yrs)", cex = 0.75, pos = 4)
  abline(v = 0, lty = 3)
  if(scaled == TRUE) {abline(h = 1, lty = 3)} else {abline(h = median(x), lty = 3)}
  if(Unc == TRUE){
    resample <- sample(x, size = length(x)*500, replace = TRUE)
    mat <- matrix(resample, nrow = length(x), ncol = 500)
    Medians <- apply(mat, 2, median)
    LmomsAll <- Lmoms(mat[,1])
    for(i in 2:500) {LmomsAll <- rbind(LmomsAll, Lmoms(mat[,i]))}
    if(dist == "Gumbel") {FCs <- func(LmomsAll$Lcv[1], RP = RPs)*Medians[1]
    for(i in 2:500) {FCs <- rbind(FCs, func(LmomsAll$Lcv[i], RP = RPs)*Medians[i])} } else {
      FCs <- func(LmomsAll$Lcv[1], LmomsAll$LSkew[1], RP = RPs)*Medians[1]
      for(i in 2:500) {FCs <- rbind(FCs, func(LmomsAll$Lcv[i], LmomsAll$LSkew[i], RP = RPs)*Medians[i])}}
    lower95 <- as.numeric(apply(FCs, 2, quantile, 0.025, na.rm = TRUE))
    upper95 <- as.numeric(apply(FCs, 2, quantile, 0.975, na.rm = TRUE))
    if(scaled == TRUE) {
      lower95 <- lower95/median(x)
      upper95 <- upper95/median(x)
    }
    points(Log.Red.Var, lower95, type = "l", lty = 3, lwd = 2)
    points(Log.Red.Var, upper95, type = "l", lty = 3, lwd = 2)
    
  }
}

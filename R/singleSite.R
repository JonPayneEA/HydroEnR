# singleSite
# Carries out a single site analysis using AMAX data

library(UKFE)

singleSite <- function(Date, AMAX, returns = c(2,5,15,20,50,75,100,200,1000), fit = 'all', flows = NULL, trend = FALSE) {
  data <- list()
  if(fit == 'all'){
    
    # Growth curve plots
    par(mfrow = c(2,2))
    EVPlot1(AMAX, dist = "GEV", 
            Title = 'Generalised Extreme Value')
    EVPlot1(AMAX, dist = "GenPareto", 
            Title = 'Generalised Pareto')
    EVPlot1(AMAX, dist = "Gumbel", 
            Title = 'Gumbel')
    EVPlot1(AMAX, dist = "GenLog", 
            Title = 'Generalised Logistic')
    par(mfrow = c(1,1))
    
    # Growth curve parameters
    param <- c('Loc', 'Scale','Shape')
    gev <- GEVPars(AMAX)
    genP <- GenParetoPars(AMAX)
    gum <- GumbelPars(AMAX)
    gum$Shape <- NA
    genL <- GenLogPars(AMAX)
    
    Shapes <- data.frame(GEV = t(gev), 
                         GenPareto = t(gum),
                         Gumbel = t(genP),
                         GenLogistic = t(genL))
    
    # Model Fits
    gev_fit <- GoTF1(AMAX, dist = "GEV")
    genP_fit <- GoTF1(AMAX, dist = "GenPareto")
    gum_fit <- GoTF1(AMAX, dist = "Gumbel")
    genL_fit <- GoTF1(AMAX, dist = "GenLog")
    
    fits <- data.frame(GEV = t(gev_fit), 
                       GenPareto = t(genP_fit), 
                       Gumbel = t(gum_fit),
                       GenLogistic = t(genL_fit))
    
    # Growth Factors
    gev_gf <- GEVGF(Lcv(AMAX), LSkew(AMAX), RP = returns)
    genP_gf <- GenParetoGF(Lcv(AMAX), LSkew(AMAX), ppy = 1, RP = returns)
    gum_gf <- GumbelGF(Lcv(AMAX), RP = returns)
    genL_gf <- GenLogGF(Lcv(AMAX), LSkew(AMAX), RP = returns)
    
    gf <- data.frame(Return_period = returns,
                     GEV = gev_gf,
                     GenPareto = genP_gf,
                     Gumbel = gum_gf,
                     GenLogistic = genL_gf)
    
    # Flows by return period
    GEV_flows <- GEVAM(AMAX, RP = returns, q = NULL, trend = trend)
    GenPareto_flows <- GenParetoPOT(AMAX, ppy = 1, RP = returns, q = NULL)
    Gumbel_flows <- GumbelAM(AMAX, RP = returns, q = NULL, trend = trend)
    GenLog_flows <- GenLogAM(AMAX, RP = returns, q = NULL, trend = trend)
    
    Flows <- data.frame(Return_Period = returns, 
                        GEV = GEV_flows, 
                        GenPareto = GenPareto_flows, 
                        Gumbel = Gumbel_flows,
                        GenLogistic = GenLog_flows)
    
    data[['Model_Shapes']] <- Shapes
    data[['Model_Fits']] <- fits
    data[['Growth_Factors']] <- gf
    data[['Estimated_Flows']] <- Flows
    
    
    # Estimated return period of user specified flow
    if(is.null(flows)){
      stop
    } else{
      
      GEV_rp <- GEVAM(AMAX, RP = NULL, q = flows, trend = trend)
      Gumbel_rp <- GumbelAM(AMAX, RP = NULL, q = flows, trend = trend)
      GenPareto_rp <- GenParetoPOT(AMAX, ppy = 1, RP = NULL, q = flows)
      GenLog_rp <- GenLogAM(AMAX, RP = NULL, q = flows, trend = trend)
      
      RPs <- data.frame(Sample_Flows = flows, 
                        GEV = GEV_rp, 
                        Gumbel = Gumbel_rp,
                        GenPareto = GenPareto_rp, 
                        GenLogistic = GenLog_rp)
      
      data[['Estimated_RP']] <- RPs
    }
    
    return(data)
    
  }
}

singleSite(Buildwas_Analysis[[5]]$HydrologicalYear, Buildwas_Analysis[[5]]$HydroYear_Max, flows = c(300,500, 980))

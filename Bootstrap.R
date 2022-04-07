#####################BOOTSTRAP###
#BS functions
OLS_BS <- function(resp,pred){
  y <- as.matrix(resp)
  X <- as.matrix(cbind(1,pred))
  beta <- solve(t(X)%*%X)%*%t(X)%*%y
  res <- as.matrix(y-beta[1]-beta[2]*X[,2])
  n <- length(resp)
  I<- rep(1,n)
  k <- ncol(X)
  VCV <- 1/(n-k)*as.numeric(t(res)%*%res)*solve(t(X)%*%X)
  se <- sqrt(diag(VCV))
  p_val <- rbind(2*pt(abs(beta[1]/se[1]),df=n-k,
                      lower.tail= FALSE),
                 2*pt(abs(beta[2]/se[2]),df=n-k,
                      lower.tail= FALSE))
  v <- ((c(y)-beta[1:1]-beta[2:2]*(c(X)-pred[1]))^2)%*%t(I)
  var <- (1/n)*v[1] #MLE variance estimate
  #bundle to return
  outVec <- c(0,0,0)
  #rownames(outMat) <- c('alpha', 'beta')
  outVec[1] <- beta[1:1]
  outVec[2] <- beta[2:2]
  outVec[3] <- var
  return(outVec)
}
{
  S_2 <- function(y,x){
    n <- length(y)
    (1/(n-2))*OLS_BS(y,x)[3]
  }
  
  crit_val_BS <- function(Bootstrap){
    alpha <- 0.05															# Choose a significance level alpha
    c.alpha.star <- quantile(Bootstrap, probs = 1 - alpha)						# Get the bootstrap critical value
    return(c.alpha.star)
  }
  
  Q_BS <- function(Beta, Beta.0, X, Y){
    S_xx <- var(X)
    S_2 <- cov(X, Y)
    Q_BS <- (Beta - Beta.0)/sqrt(S_2/S_xx)
    return(Q_BS)
  }
  
  ResidualVector<- function(n,X,Y,a,b){
    resid<-rep(0,n)
    for(i in 1:length(Y)){ 
      resid[i]= Y[i]-a-b*X[i]
    }
    return(resid)
  }
  
  BS_Int <- function(b,crit_alpha_half,crit_one_minus_alpha_half, S_2, X){
    
    lb <- b-(crit_alpha_half*sqrt(S_2/var(X)))
    ub <- b-(crit_one_minus_alpha_half*sqrt(S_2/var(X)))
    
    BS_CI <- c(0,0)
    BS_CI[1] <- lb
    BS_CI[2] <- ub
    
    return(BS_CI)
  }
  
  Resid_BS <- function(n, X, Y, resid, a, beta){
    B<- 9999
    Q.Star<- rep(NA,B)
    BetaLSstar <- rep(NA,B)
    AlphaLSstar <- rep(NA,B)
    
    for(b in 1:B){
      
      J <- sample.int(length(Y), size = n, replace= TRUE)  
      resid.star <- resid[J] 
      X.star <- X #fix
      Y.star <- a +beta*X.star + resid.star #We take alpha* and beta* as estimators in order to bootstrap the residuals, we can also take alpha*=alpha.0 and beta*=beta.0
      
      X.star.bar<- mean(X.star)
      Y.star.bar<- mean(Y.star)
      S.XX.star <- var(X.star)
      S.XY.star <- cov(X.star, Y.star)
      
      BetaLSstar[b]<-S.XY.star/S.XX.star
      AlphaLSstar[b]<-Y.star.bar-(BetaLSstar[b]*X.star.bar)
      
      S.squared.star<-(1/(n-2))*sum(n, (Y.star-AlphaLSstar[b]-BetaLSstar[b]*X.star)^2)
      
      Q.Star[b]<- (BetaLSstar[b]-beta)/sqrt(S.squared.star/S.XX.star)
      
    }
    return(Q.Star)
  }
  
  Pairs_BS <- function(n,X,Y,BetaLS){ #pages 33-34 BS notes
    
    B <- 9999 #numreps
    Q.star <- rep(NA, B) #ret vector
    BetaLSstar <- rep(NA, B) #beta
    AlphaLSstar <- rep(NA, B) #alpha
    
    for(b in 1:B){
      
      J <- sample.int(nrow(da), size = n, replace= TRUE)  	
      X.star <- X[J]										
      Y.star <- Y[J]
      
      X.star.bar <- mean(X.star) 
      Y.star.bar <- mean(Y.star)
      S.XX.star <- var(X.star) 
      S.XY.star <- cov(X.star, Y.star)
      
      BetaLSstar[b] <- S.XY.star/S.XX.star  #BS beta
      AlphaLSstar[b] <- Y.star.bar- BetaLSstar[b]*X.star.bar #BS alpha
      
      S.squared.star <- (1/(n-2))*sum(n, (Y.star-AlphaLSstar[b]-BetaLSstar[b]*X.star)^2) 
      
      Q.star[b] <- (BetaLSstar[b]-BetaLS)/sqrt(S.squared.star/S.XX.star) #bootstrap Q
    }
    return(Q.star)
  }
  
  BS_t <- function(X){ #H0: Q*<=0 H1: Q*>0
    B<-9999
    Q.star <- rep(NA,B)
    n<- length(X)
    t.n <- t.test(X, alternative = "greater", mu = 0)$statistic
    for(b in 1:B){
      
      J <- sample.int(n,size = n, replace = TRUE)
      X_star <- X[J]
      X_bar_star <- mean(X_star)
      X_Sd_bar <- sd(X_star)
      
      Q.star[b]<- sqrt(n)*(X_bar_star-mean(X))/X_Sd_bar
      p.val <- sum(Q.star > t.n) #see p.27 of bootstrap pdf
    }
    
    return(list( Q = Q.star, p = p.val, t = t.n))
  }
  
  BS_CI_t <- function(X, critical_alpha_half, critical_1min__alpha_half){
    
    lb <- mean(X) -(critical_alpha_half*sd(X))/sqrt(length(X))
    ub <- mean(X)-(critical_1min__alpha_half*sd(X))/sqrt(length(X))
    
    CI <- c(0,0)
    CI[1]<-lb
    CI[2]<-ub
    
    return(CI)
  }
}

#prep data
{
  aD <- OLS_BS(da$de_bilt, da$year)[1]
  bD <- OLS_BS(da$de_bilt, da$year)[2]
  aM <- OLS_BS(da$maastricht, da$year)[1]
  bM <- OLS_BS(da$maastricht, da$year)[2]
  aE <- OLS_BS(da$eelde, da$year)[1]
  bE <- OLS_BS(da$eelde, da$year)[2]
  
  aDM <- OLS_BS(dm$de_bilt, dm$month)[1]
  bDM <- OLS_BS(dm$de_bilt, dm$month)[2]
  aMM <- OLS_BS(dm$maastricht, dm$month)[1]
  bMM <- OLS_BS(dm$maastricht, dm$month)[2]
  aEM <- OLS_BS(dm$eelde, dm$month)[1]
  bEM <- OLS_BS(dm$eelde, dm$month)[2]
}

#BS analysis
{
  Beta.0 <- 0 #beta under H0: Beta.0<=0 H1: Beta.0>0
  set.seed(234987)
  
  n <- nrow(da)		
  alpha <- 0.05
  
  #get results
  {
    #Quantities
    QMY <- Q_BS(bM, Beta.0, da$year, da$maastricht)
    QDY <- Q_BS(bM, Beta.0, da$year, da$de_bilt)
    QEY <- Q_BS(bM, Beta.0, da$year, da$eelde)
    
    QMM <- Q_BS(bM, Beta.0, dm$maastricht, dm$maastricht)
    QDM <- Q_BS(bM, Beta.0, dm$maastricht, dm$de_bilt)
    QEM <- Q_BS(bM, Beta.0, dm$maastricht, dm$eelde)
    
    #BS
    PairsBSM <- Pairs_BS(n, da$year, da$maastricht, bM)
    PairsBSD <- Pairs_BS(n, da$year, da$de_bilt, bD)
    PairsBSE <- Pairs_BS(n, da$year, da$eelde, bE)
    #monthly
    PairsBSMM <- Pairs_BS(n, dm$month, dm$maastricht, bMM)
    PairsBSDM <- Pairs_BS(n, dm$month, dm$de_bilt, bDM)
    PairsBSEM <- Pairs_BS(n, dm$month, dm$eelde, bEM)
    
    ResidBSM <- Resid_BS(length(da$year), da$maastricht, da$year, ResidualVector(length(da$year), da$maastricht, da$year, aM, bM), aM, bM)
    ResidBSD <- Resid_BS(length(da$year), da$de_bilt, da$year, ResidualVector(length(da$year), da$de_bilt, da$year, aD, bD), aD, bD)
    ResidBSE <- Resid_BS(length(da$year), da$eelde, da$year, ResidualVector(length(da$year), da$eelde, da$year, aE, bE), aE, bE)
    
    #monthly
    ResidBSMM <- Resid_BS(length(dm$month), dm$maastricht, dm$month, ResidualVector(length(dm$month), dm$maastricht, dm$mont, aMM, bMM), aMM, bMM)
    ResidBSDM <- Resid_BS(length(dm$month), dm$de_bilt, dm$month, ResidualVector(length(dm$month), dm$de_bilt, dm$mont, aDM, bDM), aDM, bDM)
    ResidBSEM <- Resid_BS(length(dm$month), dm$eelde, dm$month, ResidualVector(length(dm$month), dm$eelde, dm$mont, aEM, bEM), aEM, bEM)
    
    #CIs
    CIPairsM <- BS_Int(bM, quantile(PairsBSM, probs = 1-(alpha/2)), quantile(PairsBSM, probs = (alpha/2)), S_2(da$year, da$maastricht), da$maastricht)
    CIResidM <- BS_Int(bM, quantile(ResidBSM, probs = 1-(alpha/2)), quantile(ResidBSM, probs = (alpha/2)), S_2(da$year, da$maastricht), da$maastricht)
    
    CIPairsD <- BS_Int(bD, quantile(PairsBSD, probs = 1-(alpha/2)), quantile(PairsBSD, probs = (alpha/2)), S_2(da$year, da$de_bilt), da$de_bilt)
    CIResidD <- BS_Int(bD, quantile(ResidBSD, probs = 1-(alpha/2)), quantile(ResidBSD, probs = (alpha/2)), S_2(da$year, da$de_bilt), da$de_bilt)
    
    CIPairsE <- BS_Int(bE, quantile(PairsBSE, probs = 1-(alpha/2)), quantile(PairsBSE, probs = (alpha/2)), S_2(da$year, da$eelde), da$eelde)
    CIResidE <- BS_Int(bE, quantile(ResidBSE, probs = 1-(alpha/2)), quantile(ResidBSE, probs = (alpha/2)), S_2(da$year, da$eelde), da$eelde)
    
    #monthly
    CIPairsMM <- BS_Int(bMM, quantile(PairsBSMM, probs = 1-(alpha/2)), quantile(PairsBSMM, probs = (alpha/2)), S_2(dm$month, dm$maastricht), dm$maastricht)
    CIResidMM <- BS_Int(bMM, quantile(ResidBSMM, probs = 1-(alpha/2)), quantile(ResidBSMM, probs = (alpha/2)), S_2(dm$month, dm$maastricht), dm$maastricht)
    
    CIPairsDM <- BS_Int(bDM, quantile(PairsBSDM, probs = 1-(alpha/2)), quantile(PairsBSDM, probs = (alpha/2)), S_2(dm$month, dm$de_bilt), dm$de_bilt)
    CIResidDM <- BS_Int(bDM, quantile(ResidBSDM, probs = 1-(alpha/2)), quantile(ResidBSDM, probs = (alpha/2)), S_2(dm$month, dm$de_bilt), dm$de_bilt)
    
    CIPairsEM <- BS_Int(bEM, quantile(PairsBSEM, probs = 1-(alpha/2)), quantile(PairsBSEM, probs = (alpha/2)), S_2(dm$month, dm$eelde), dm$eelde)
    CIResidEM <- BS_Int(bEM, quantile(ResidBSEM, probs = 1-(alpha/2)), quantile(ResidBSEM, probs = (alpha/2)), S_2(dm$month, dm$eelde), dm$eelde)
    
  }
  
  #export results
  {
    BSmat1 <- matrix(nrow = 12, ncol=3)
    rownames(BSmat1) <- c('De Bilt, Yearly Data, Pairs', 'De Bilt, Yearly Data, Residuals', 'Eelde, Yearly Data, Pairs', 
                          'Eelde, Yearly Data, Residuals','Maastricht, Yearly Data, Pairs','Maastricht, Yearly Data, Residuals',
                          'De Bilt, Monthly Data, Pairs', 'De Bilt, Monthly Data, Residuals', 'Eelde, Monthly Data, Pairs', 
                          'Eelde, Monthly Data, Residuals','Maastricht, Monthly Data, Pairs','Maastricht, Monthly Data, Residuals')
    colnames(BSmat1) <- c('$Q^*$', 'CI lower', 'CI upper')
    
    BSmat1[1,1] <- QDY
    BSmat1[3,1] <- QEY
    BSmat1[5,1] <- QMY
    BSmat1[7,1] <- QDY
    BSmat1[9,1] <- QEY
    BSmat1[11,1] <- QMY
    
    
    BSmat1[1,2] <- CIPairsD[1:1]
    BSmat1[2,2] <- CIResidD[1:1]
    
    BSmat1[3,2] <- CIPairsE[1:1]
    BSmat1[4,2] <- CIResidE[1:1]
    
    BSmat1[5,2] <- CIPairsM[1:1]
    BSmat1[6,2] <- CIResidM[1:1]
    
    BSmat1[7,2] <- CIPairsDM[1:1]
    BSmat1[8,2] <- CIResidDM[1:1]
    
    BSmat1[9,2] <- CIPairsEM[1:1]
    BSmat1[10,2] <- CIResidEM[1:1]
    
    BSmat1[11,2] <- CIPairsMM[1:1]
    BSmat1[12,2] <- CIResidMM[1:1]
    
    BSmat1[1,3] <- CIPairsD[2:2]
    BSmat1[2,3] <- CIResidD[2:2]
    
    BSmat1[3,3] <- CIPairsE[2:2]
    BSmat1[4,3] <- CIResidM[2:2]
    
    BSmat1[5,3] <- CIPairsM[2:2]
    BSmat1[6,3] <- CIResidM[2:2]
    
    BSmat1[7,3] <- CIPairsDM[2:2]
    BSmat1[8,3] <- CIResidDM[2:2]
    
    BSmat1[9,3] <- CIPairsEM[2:2]
    BSmat1[10,3] <- CIResidMM[2:2]
    
    BSmat1[11,3] <- CIPairsMM[2:2]
    BSmat1[12,3] <- CIResidMM[2:2]
  }
  
  ##BS t-test for Climate Break
  {
    #diff <- postCBY[, .(de_bilt, eelde, maastricht)] - preCBY[, .(de_bilt, eelde, maastricht)] (already exists)
    diffM <- postCBM[, .(de_bilt, eelde, maastricht)] - preCBM[, .(de_bilt, eelde, maastricht)]
    
    BStM <- BS_t(diff$maastricht)
    BSCItM <- BS_CI_t(diff$maastricht, quantile(BStM$Q,probs=1-(alpha/2)), quantile(BStM$Q,probs=(alpha/2)))
    
    BStD <- BS_t(diff$de_bilt)
    BSCItD <- BS_CI_t(diff$de_bilt, quantile(BStD$Q,probs=1-(alpha/2)), quantile(BStD$Q,probs=(alpha/2)))
    
    BStE <- BS_t(diff$eelde)
    BSCItE <- BS_CI_t(diff$eelde, quantile(BStE$Q,probs=1-(alpha/2)), quantile(BStE$Q,probs=(alpha/2)))
    
    #monthly
    BStMM <- BS_t(diffM$maastricht)
    BSCItMM <- BS_CI_t(diffM$maastricht, quantile(BStMM$Q,probs=1-(alpha/2)), quantile(BStMM$Q,probs=(alpha/2)))
    
    BStDM <- BS_t(diffM$de_bilt)
    BSCItDM <- BS_CI_t(diffM$de_bilt, quantile(BStDM$Q,probs=1-(alpha/2)), quantile(BStDM$Q,probs=(alpha/2)))
    
    BStEM <- BS_t(diffM$eelde)
    BSCItEM <- BS_CI_t(diffM$eelde, quantile(BStEM$Q,probs=1-(alpha/2)), quantile(BStEM$Q,probs=(alpha/2)))
  }
  
  
  #export results
  {
    BSmat2 <- matrix(nrow = 6, ncol=4)
    rownames(BSmat2) <- c('De Bilt, Yearly Data', 'Eelde, Yearly Data','Maastricht, Yearly Data',
                          'De Bilt, Monthly Data', 'Eelde, Monthly Data','Maastricht, Monthly Data')
    colnames(BSmat2) <- c('$t_n$', 'p-value', 'CI lower', 'CI upper') 
    
    BSmat2[1,1] <- BStD$t
    BSmat2[2,1] <- BStE$t
    BSmat2[3,1] <- BStM$t
    
    BSmat2[1,2] <- BStD$p
    BSmat2[2,2] <- BStE$p
    BSmat2[3,2] <- BStM$p
    
    BSmat2[4,1] <- BStDM$t
    BSmat2[5,1] <- BStEM$t
    BSmat2[6,1] <- BStMM$t
    
    BSmat2[4,2] <- BStDM$p
    BSmat2[5,2] <- BStEM$p
    BSmat2[6,2] <- BStMM$p
    
    BSmat2[1,3] <- BSCItD[1]
    BSmat2[2,3] <- BSCItE[1]
    BSmat2[3,3] <- BSCItM[1]
    
    BSmat2[1,4] <- BSCItD[2]
    BSmat2[2,4] <- BSCItE[2]
    BSmat2[3,4] <- BSCItM[2]
    
    BSmat2[4,3] <- BSCItDM[1]
    BSmat2[5,3] <- BSCItEM[1]
    BSmat2[6,3] <- BSCItMM[1]
    
    BSmat2[4,4] <- BSCItDM[2]
    BSmat2[5,4] <- BSCItEM[2]
    BSmat2[6,4] <- BSCItMM[2]
  }
  
}
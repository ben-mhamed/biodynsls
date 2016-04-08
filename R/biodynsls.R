#' runs the jags model
#'
#'@export

biodynsls <- function(data,params,type,n.adapt,n.iter,thin){
  require(rjags)
  require(mcmcplots)
  if(type=="A"){
    model <-"model{
    log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
    log_B[1] ~ dnorm( log_B.m[1], tau_B)
    B[1] <- exp(log_B[1])
    # Process model
    for(t in 2 : n) {
    log_B.m[t]<-  log(max(r*B[t-1]*(1 -B[t-1]/K) - C[t-1] , 0.000001))
    log_B[t] ~ dnorm(log_B.m[t], tau_B)
    B[t] <- exp(log_B[t])
    }

    # Observation mode
    for (t in 1: n) {
    LogCPUE_mean[t] <- log( max(q*B[t], 0.000001))
    log_CPUE[t] ~ dnorm(LogCPUE_mean[t], tau_c)
    AACPUE_pred[t] <- q*B[t]
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(6000, 200000)
    r ~ dlnorm(0.0, 1.4)
    q ~ dlnorm(0.0, 6)I(1.0e-09,10)
    tau_c ~ dgamma(0.001,0.001)
    AobsErr <- 1./sqrt(tau_c)
    tau_B ~ dgamma(0.001,0.001)
    AprocErr <- 1./sqrt(tau_B)
  }
    "
  }else if(type=="SR"){
    model <- "model{
log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
    log_B[1] ~ dnorm( log_B.m[1], tau_B)
    B[1] <- exp(log_B[1])
    # Process model
    for(t in 2 : n) {
    log_B.m[t] <- log(max((r*(B[t-1] - C[t-1]))/(1 + (r/K)*(B[t-1] - C[t-1])),0.00001))
    log_B[t] ~ dnorm(log_B.m[t], tau_B)
    B[t] <- exp(log_B[t])
    }

    # Observation mode
    for (t in 1: n) {
    LogCPUE_mean[t] <- log( max(q*B[t], 0.000001))
    log_CPUE[t] ~ dnorm(LogCPUE_mean[t], tau_c)
    AACPUE_pred[t] <- q*B[t]
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(180, 7000)
    r ~ dlnorm(0.0, 1.4)
    q ~ dlnorm(0.0, 3)
    tau_c ~ dgamma(0.001,0.001)
    AobsErr <- 1./sqrt(tau_c)
    tau_B ~ dgamma(0.001,0.001)
    AprocErr <- 1./sqrt(tau_B)
  } "
  }else if(type=="Std"){
    model <-"model{
    log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
    log_B[1] ~ dnorm( log_B.m[1], tau_B)
    B[1] <- exp(log_B[1])
    # Process model
    for(t in 2 : n) {
    log_B.m[t]<-  log(max(B[t-1] + r*B[t-1]*(1 -B[t-1]/K) - C[t-1] , 0.000001))
    log_B[t] ~ dnorm(log_B.m[t], tau_B)
    B[t] <- exp(log_B[t])
    }

    # Observation mode
    for (t in 1: n) {
    LogCPUE_mean[t] <- log( max(q*B[t], 0.000001))
    log_CPUE[t] ~ dnorm(LogCPUE_mean[t], tau_c)
    AACPUE_pred[t] <- q*B[t]
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(1000, 200000)
    r ~ dlnorm(0.0, 1.4)
    q ~ dlnorm(0.0, 3)
    tau_c ~ dgamma(0.001,0.001)
    AobsErr <- 1./sqrt(tau_c)
    tau_B ~ dgamma(0.001,0.001)
    AprocErr <- 1./sqrt(tau_B)
  }
    "
  }
  #jags=jags.model(textConnection(model), data=data, inits=inits1,n.chains = 3,n.adapt = 10000)
  jags=jags.model(textConnection(model), data=data,n.chains = 3,n.adapt = n.adapt)
  output=coda.samples(model=jags,variable.names=c("AobsErr","AprocErr","r", "K","q","b","B","AACPUE_pred"),
                      n.iter=n.iter, thin=thin)
  psrf <- gelman.diag(x = output,multivariate = F)$psrf
  res<- summary(output)
  l <- list("stats"=res,"out"=output,"psrf"=psrf,"type"=type)
  return(l)
}

#' runs the jags model
#'
#'@export

biodynsls2 <- function(data,params,type){
  require(rjags)
  require(mcmcplots)
  if(type=="A"){
    model <-"model{
    log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
    log_B[1] ~ dnorm( log_B.m[1], tau_B)
    B[1] <- exp(log_B[1])
    # Process model
    for(t in 2 : n) {
    log_B.m[t]<-  log(max(r*B[t-1]*(1 -B[t-1]/K) - C1[t-1] - C2[t-1] - C3[t-1] , 0.000001))
    log_B[t] ~ dnorm(log_B.m[t], tau_B)
    B[t] <- exp(log_B[t])
    }

    # Observation mode
    for (t in 1: n) {
    LogCPUE1_mean[t] <- log( max(q1*B[t], 0.000001))
    LogCPUE2_mean[t] <- log( max(q2*B[t], 0.000001))
    LogCPUE3_mean[t] <- log( max(q3*B[t], 0.000001))
    log_CPUE1[t] ~ dnorm(LogCPUE1_mean[t], tau_c)
    log_CPUE2[t] ~ dnorm(LogCPUE2_mean[t], tau_c)
    log_CPUE3[t] ~ dnorm(LogCPUE3_mean[t], tau_c)
    AACPUE1_pred[t] <- q1*B[t]
    AACPUE2_pred[t] <- q2*B[t]
    AACPUE3_pred[t] <- q3*B[t]
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(180, 7000)
    r ~ dlnorm(0.0, 1.4)
    q1 ~ dlnorm(0.0, 3)
    q2 ~ dlnorm(0.0, 3)
    q3 ~ dlnorm(0.0, 3)
    tau_c ~ dgamma(0.001,0.001)
    AobsErr <- 1./sqrt(tau_c)
    tau_B ~ dgamma(0.001,0.001)
    AprocErr <- 1./sqrt(tau_B)
  }
    "
}else if(type=="SR"){
  model <- "model{
  log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
  log_B[1] ~ dnorm( log_B.m[1], tau_B)
  B[1] <- exp(log_B[1])
  # Process model
  for(t in 2 : n) {
  log_B.m[t] <- log(max((r*(B[t-1] - C[t-1]))/(1 + (r/K)*(B[t-1] - C[t-1])),0.00001))
  log_B[t] ~ dnorm(log_B.m[t], tau_B)
  B[t] <- exp(log_B[t])
  }

  # Observation mode
  for (t in 1: n) {
  LogCPUE_mean[t] <- log( max(q*B[t], 0.000001))
  log_CPUE[t] ~ dnorm(LogCPUE_mean[t], tau_c)
  AACPUE_pred[t] <- q*B[t]
  }

  # Prior
  b~dbeta(1, 1)
  K ~ dunif(180, 7000)
  r ~ dlnorm(0.0, 1.4)
  q ~ dlnorm(0.0, 3)
  tau_c ~ dgamma(0.001,0.001)
  AobsErr <- 1./sqrt(tau_c)
  tau_B ~ dgamma(0.001,0.001)
  AprocErr <- 1./sqrt(tau_B)
} "
  }else if(type=="Std"){
    model <-"model{
    log_B.m[1] <- log(max(K*b,0.00001))   # mean B[1]
    log_B[1] ~ dnorm( log_B.m[1], tau_B)
    B[1] <- exp(log_B[1])
    # Process model
    for(t in 2 : n) {
    log_B.m[t]<-  log(max(r*B[t-1]*(1 -B[t-1]/K) - C1[t-1] -C2[t-1] -C3[t-1] , 0.000001))
    log_B[t] ~ dnorm(log_B.m[t], tau_B)
    B[t] <- exp(log_B[t])
    }

    # Observation mode
    for (t in 1: n) {
    LogCPUE1_mean[t] <- log( max(q1*B[t], 0.000001))
    LogCPUE2_mean[t] <- log( max(q2*B[t], 0.000001))
    LogCPUE3_mean[t] <- log( max(q3*B[t], 0.000001))
    log_CPUE1[t] ~ dnorm(LogCPUE1_mean[t], tau_c)
    log_CPUE2[t] ~ dnorm(LogCPUE2_mean[t], tau_c)
    log_CPUE3[t] ~ dnorm(LogCPUE3_mean[t], tau_c)
    AACPUE1_pred[t] <- q1*B[t]
    AACPUE2_pred[t] <- q2*B[t]
    AACPUE3_pred[t] <- q3*B[t]
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(180, 7000)
    r ~ dlnorm(0.0, 1.4)
    q1 ~ dlnorm(0.0, 3)
    q2 ~ dlnorm(0.0, 3)
    q3 ~ dlnorm(0.0, 3)
    tau_c ~ dgamma(0.001,0.001)
    AobsErr <- 1./sqrt(tau_c)
    tau_B ~ dgamma(0.001,0.001)
    AprocErr <- 1./sqrt(tau_B)
}
  "
  }
  #jags=jags.model(textConnection(model), data=data, inits=inits1,n.chains = 3,n.adapt = 10000)
  jags=jags.model(textConnection(model), data=data,n.chains = 3,n.adapt = 10000)
  output=coda.samples(model=jags,variable.names=c("AobsErr","AprocErr","r", "K","q1","q2","q3","b","B","AACPUE1_pred","AACPUE2_pred","AACPUE3_pred"), n.iter=10000, thin=10)
  psrf <- gelman.diag(x = output,multivariate = F)$psrf
  res<- summary(output)
  l <- list("stats"=res,"out"=output,"psrf"=psrf,"type"=type)
  return(l)
}

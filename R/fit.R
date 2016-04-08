#' fitting the model
#' @export
fit <- function(data,params,type,n.index,n.adapt,n.iter,thin){
  require(rjags)
  require(mcmcplots)
  C <- data[,2]
  n <- nrow(data)
  nc <- ncol(data)
  tmp <- data[,3:nc]
  if(n.index==1){
    data <- list("log_CPUE"=matrix(log(tmp),ncol = 1))
  }else{
    data <- list("log_CPUE"=log(tmp))
  }
  data$n <- n
  data$C <- C
  data$n.index <- n.index

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
       for(i in 1:n.index){
             LogCPUE_mean[t,i] <- log( max(q[i]*B[t], 0.000001))
             log_CPUE[t,i] ~ dnorm(LogCPUE_mean[t,i], tau_c)
             AACPUE_pred[t,i] <- q[i]*B[t]
       }
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(6000, 200000)
    r ~ dlnorm(0.0, 1.4)
    for(i in 1:n.index){
        q[i] ~ dlnorm(0.0, 6)I(1.0e-09,10)
    }
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
       for(i in 1:n.index){
             LogCPUE_mean[t,i] <- log( max(q[i]*B[t], 0.000001))
             log_CPUE[t,i] ~ dnorm(LogCPUE_mean[t,i], tau_c)
             AACPUE_pred[t,i] <- q[i]*B[t]
       }
    }

  # Prior
  b~dbeta(1, 1)
  K ~ dunif(180, 7000)
  r ~ dlnorm(0.0, 1.4)
  for(i in 1:n.index){
      q[i] ~ dlnorm(0.0, 6)I(1.0e-09,10)
  }
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
       for(i in 1:n.index){
             LogCPUE_mean[t,i] <- log( max(q[i]*B[t], 0.000001))
             log_CPUE[t,i] ~ dnorm(LogCPUE_mean[t,i], tau_c)
             AACPUE_pred[t,i] <- q[i]*B[t]
       }
    }

    # Prior
    b~dbeta(1, 1)
    K ~ dunif(1000, 200000)
    r ~ dlnorm(0.0, 1.4)
    for(i in 1:n.index){
        q[i] ~ dlnorm(0.0, 6)I(1.0e-09,10)
    }
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

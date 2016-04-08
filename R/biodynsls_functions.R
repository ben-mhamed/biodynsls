#'refPoints
#'@export
refPoints1 <- function(l,data){
  type = l$type
  n <- data$n
  if(type=="A"){
    B <- l$stats$quantiles[(n+3):(n+3+(n-1)),"50%"]
    Ka <- l$stats$quantiles[(n+3+(n-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(n-1)+2),"50%"]
    q <- l$stats$quantiles[(n+3+(n-1)+3),"50%"]
    ra <- l$stats$quantiles[(n+3+(n-1)+4),"50%"]
    r_std <- ra-1
    K_std <- ((ra - 1)*Ka)/ra
    Bmsy <- ((ra-1)*Ka)/(2*ra)
    msy <- (((ra-1)^2)*Ka)/(4*ra)
    Bcurr <- B[n]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- data$C[n]/B[n]
    dt <- data.frame(Params=c("Ka","b","q","ra","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(Ka,b,q,ra,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="SR"){
    B <- l$stats$quantiles[(n+3):(n+3+(n-1)),"50%"]
    K_sr <- l$stats$quantiles[(n+3+(n-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(n-1)+2),"50%"]
    q <- l$stats$quantiles[(n+3+(n-1)+3),"50%"]
    r_sr <- l$stats$quantiles[(n+3+(n-1)+4),"50%"]
    r_std <- 2 - 2/r_sr
    K_std <- r_sr*r_std
    Bmsy <- K_sr*(1 - 1/sqrt(r_sr))
    msy <- K_sr*(1 - 1/sqrt(r_sr))^2
    Bcurr <- B[n]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- data$C[n]/B[n]
    dt <- data.frame(Params=c("K_sr","b","q","r_sr","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_sr,b,q,r_sr,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="Std"){
    B <- l$stats$quantiles[(n+3):(n+3+(n-1)),"50%"]
    K_std <- l$stats$quantiles[(n+3+(n-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(n-1)+2),"50%"]
    q <- l$stats$quantiles[(n+3+(n-1)+3),"50%"]
    r_std <- l$stats$quantiles[(n+3+(n-1)+4),"50%"]
    Bmsy <- K_std/2
    msy <- (r_std*K_std)/4
    Bcurr <- B[n]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- data$C[n]/B[n]
    dt <- data.frame(Params=c("K_std","b","q","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_std,b,q,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }
  return(dt)
}

#'refPoints
#'@export
refPoints2 <- function(l,data){
  type = l$type
  n <- data$n*3
  m <- data$n
  if(type=="A"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    Ka <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q1 <- l$stats$quantiles[(n+3+(m-1)+3),"50%"]
    q2 <- l$stats$quantiles[(n+3+(m-1)+4),"50%"]
    q3 <- l$stats$quantiles[(n+3+(m-1)+5),"50%"]
    ra <- l$stats$quantiles[(n+3+(m-1)+6),"50%"]
    r_std <- ra-1
    K_std <- ((ra - 1)*Ka)/ra
    Bmsy <- ((ra-1)*Ka)/(2*ra)
    msy <- (((ra-1)^2)*Ka)/(4*ra)
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- (data$C1[m]+data$C2[m]+data$C3[m])/Bcurr
    dt <- data.frame(Params=c("Ka","b","q1","q2","q3","ra","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(Ka,b,q1,q2,q3,ra,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="SR"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    K_sr <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q1 <- l$stats$quantiles[(n+3+(m-1)+3),"50%"]
    q2 <- l$stats$quantiles[(n+3+(m-1)+4),"50%"]
    q3 <- l$stats$quantiles[(n+3+(m-1)+5),"50%"]
    r_sr <- l$stats$quantiles[(n+3+(m-1)+6),"50%"]
    r_std <- 2 - 2/r_sr
    K_std <- r_sr*r_std
    Bmsy <- K_sr*(1 - 1/sqrt(r_sr))
    msy <- K_sr*(1 - 1/sqrt(r_sr))^2
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- data$C[m]/Bcurr
    dt <- data.frame(Params=c("K_sr","b","q","r_sr","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_sr,b,q,r_sr,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="Std"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    K_std <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q1 <- l$stats$quantiles[(n+3+(m-1)+3),"50%"]
    q2 <- l$stats$quantiles[(n+3+(m-1)+4),"50%"]
    q3 <- l$stats$quantiles[(n+3+(m-1)+5),"50%"]
    r_std <- l$stats$quantiles[(n+3+(m-1)+6),"50%"]
    Bmsy <- K_std/2
    msy <- (r_std*K_std)/4
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- (data$C1[m]+data$C2[m]+data$C3[m])/Bcurr
    dt <- data.frame(Params=c("K_std","b","q1","q2","q3","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_std,b,q1,q2,q3,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }
  return(dt)
}

#'refPoints
#'@export
refPoints <- function(l,data){
  type = l$type
  m <- nrow(data)
  nc <- length(3:ncol(data))
  n <- m*nc
  if(type=="A"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    Ka <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q <- numeric(nc)
    for(i in 1:nc){
      q[i] <- l$stats$quantiles[(n+3+(m-1)+2+i),"50%"]
    }
    ra <- l$stats$quantiles[(n+3+(m-1)+2+nc+1),"50%"]
    r_std <- ra-1
    K_std <- ((ra - 1)*Ka)/ra
    Bmsy <- ((ra-1)*Ka)/(2*ra)
    msy <- (((ra-1)^2)*Ka)/(4*ra)
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- (data$C[m])/Bcurr
    nq <-rep("",nc)
    for(i in 1:nc){
      nq[i] <- paste("q",i,sep = "")
    }
    dt <- data.frame(Params=c("Ka","b",nq,"ra","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(Ka,b,q,ra,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="SR"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    K_sr <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q <- numeric(nc)
    for(i in 1:nc){
      q[i] <- l$stats$quantiles[(n+3+(m-1)+2+i),"50%"]
    }
    r_sr <- l$stats$quantiles[(n+3+(m-1)+2+nc+1),"50%"]

    r_std <- 2 - 2/r_sr
    K_std <- r_sr*r_std
    Bmsy <- K_sr*(1 - 1/sqrt(r_sr))
    msy <- K_sr*(1 - 1/sqrt(r_sr))^2
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- data$C[m]/Bcurr
    nq <-rep("",nc)
    for(i in 1:nc){
      nq[i] <- paste("q",i,sep = "")
    }
    dt <- data.frame(Params=c("K_sr","b",nq,"r_sr","K_std","r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_sr,b,q,r_sr,K_std,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }else if(type=="Std"){
    B <- l$stats$quantiles[(n+3):(n+3+(m-1)),"50%"]
    K_std <- l$stats$quantiles[(n+3+(m-1)+1),"50%"]
    b <- l$stats$quantiles[(n+3+(m-1)+2),"50%"]
    q <- numeric(nc)
    for(i in 1:nc){
      q[i] <- l$stats$quantiles[(n+3+(m-1)+2+i),"50%"]
    }
    r_std <- l$stats$quantiles[(n+3+(m-1)+2+nc+1),"50%"]

    Bmsy <- K_std/2
    msy <- (r_std*K_std)/4
    Bcurr <- B[m]
    Bcurr.over.K <- Bcurr/K_std
    Fmsy <- msy/Bmsy
    Fcurr <- (data$C[m])/Bcurr
    nq <-rep("",nc)
    for(i in 1:nc){
      nq[i] <- paste("q",i,sep = "")
    }
    dt <- data.frame(Params=c("K_std","b",nq,"r_std","Bcurr","Bcurr/K","Bmsy","msy","Fmsy","Fcurr"),
                     Value=c(K_std,b,q,r_std,Bcurr,Bcurr.over.K,Bmsy,msy,Fmsy,Fcurr))
  }
  return(dt)
}

# # test
# getExt <- function(file)
#   tolower(substr(file,max(gregexpr("\\.", file)[[1]])+1,nchar(file)))
#
# checkExt=function(x) (tolower(getExt(x))=="inp")
#data <- read.csv2(file = "/home/ben-mhamed/Wahid/My_packages/biodynsls.biblio/crevette_data/crevette.csv",sep=",")
# year <- data$year
# C=data$yield
# log_CPUE=log(data$cpue)
# data <- list(C=C,log_CPUE=log_CPUE,year=year)

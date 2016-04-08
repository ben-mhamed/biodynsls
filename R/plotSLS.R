#'plot gelman-rubin values
#'
#'@export
plotDiag <- function(l,data) {
  psrf <- l$psrf
  output <- l$out
  require(DAAG)
  n<- nrow(psrf)
  readline(prompt = "Pause. Press <Enter> to continue...")
  plot(1:n,psrf[,1],ylim = c(0.9,1.2),xlab = 'Parameters',ylab = 'G-R Statistic',type = "b")
  abline(h = 1,col="blue")
  abline(h = 1.1,col="red")
  text(x = 1:n,y = psrf[,1]+0.05,labels = rownames(psrf),cex = 0.4)
  pause()
  traplot(output, parms = c('r','K','q','b',"B","AACPUE_pred","AobsErr","AprocErr"))
  pause()
  denplot(output, parms = c('r','K','q','b',"B","AACPUE_pred","AobsErr","AprocErr"))
}

#'
#'plot index
#'@export
#'
plotIndex1 <- function(l,data){
  y <- exp(data$log_CPUE)
  n <- data$n
  x <- data$year
  u <- l$stats$statistics[1:n,1]
  u_se <- l$stats$statistics[1:n,2]
  plot(x, y, col = "#00000080", pch = 21, bg = "#00000030", las = 1,
       ylab = "CPUE", xlab = "year")
  lines(x, u, col = "red", lty = 1, lwd = 1.5)
  polygon(c(x, rev(x)), c(u - 2 * u_se, rev(u + 2 * u_se)),
          border = NA, col = "#FF000050")
  legend("topright", legend = c("Observed", "Estimated"),
         pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
         col = c("#00000080", "red"),
         pt.bg = c("#00000030", NA))
}

#'
#'plot index
#'@export
#'
plotIndex2 <- function(l,data){
  y1 <- exp(data$log_CPUE1)
  y2 <- exp(data$log_CPUE2)
  y3 <- exp(data$log_CPUE3)
  n <- data$n
  x <- data$year
  u1 <- l$stats$statistics[1:n,1]
  u2 <- l$stats$statistics[(n+1):(2*n),1]
  u3 <- l$stats$statistics[(2*n+1):(3*n),1]
  u1_se <- l$stats$statistics[1:n,2]
  u2_se <- l$stats$statistics[(n+1):(2*n),2]
  u3_se <- l$stats$statistics[(2*n+1):(3*n),2]
  readline(prompt = "Pause. Press <Enter> to continue...")
  plot(x, y1, col = "#00000080", pch = 21, bg = "#00000030", las = 1,
       ylab = "CPUE", xlab = "year")
  lines(x, u1, col = "red", lty = 1, lwd = 1.5)
  polygon(c(x, rev(x)), c(u1 - 2 * u1_se, rev(u1 + 2 * u1_se)),
          border = NA, col = "#FF000050")
  legend("bottomright", legend = c("Observed", "Estimated"),
         pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
         col = c("#00000080", "red"),
         pt.bg = c("#00000030", NA))
  pause()
  plot(x, y2, col = "#00000080", pch = 21, bg = "#00000030", las = 1,
       ylab = "CPUE", xlab = "year")
  lines(x, u2, col = "red", lty = 1, lwd = 1.5)
  polygon(c(x, rev(x)), c(u2 - 2 * u2_se, rev(u2 + 2 * u2_se)),
          border = NA, col = "#FF000050")
  legend("bottomright", legend = c("Observed", "Estimated"),
         pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
         col = c("#00000080", "red"),
         pt.bg = c("#00000030", NA))
  pause()
  plot(x, y3, col = "#00000080", pch = 21, bg = "#00000030", las = 1,
       ylab = "CPUE", xlab = "year")
  lines(x, u3, col = "red", lty = 1, lwd = 1.5)
  polygon(c(x, rev(x)), c(u3 - 2 * u3_se, rev(u3 + 2 * u1_se)),
          border = NA, col = "#FF000050")
  legend("bottomright", legend = c("Observed", "Estimated"),
         pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
         col = c("#00000080", "red"),
         pt.bg = c("#00000030", NA))
}

#'
#'plot index
#'@export
#'
plotIndex <- function(l,data){
  n <- nrow(data)
  nc <- length(3:ncol(data))
  if(nc==1){
    y <- matrix(data[,3:ncol(data)],ncol = 1)
  }else{
    y <- data[,3:ncol(data)]
  }

  x <- data$year
  u <- matrix(l$stats$statistics[1:(n*nc),1],ncol = nc,byrow = FALSE)
  u_se <- matrix(l$stats$statistics[1:(n*nc),2],ncol = nc,byrow = FALSE)

  readline(prompt = "Pause. Press <Enter> to continue...")
 for(i in 1:nc){
   plot(x, y[,i], col = "#00000080", pch = 21, bg = "#00000030", las = 1,
        ylab = "CPUE", xlab = "year")
   lines(x, u[,i], col = "red", lty = 1, lwd = 1.5)
   polygon(c(x, rev(x)), c(u[,i] - 2 * u_se[,i], rev(u[,i] + 2 * u_se[,i])),
           border = NA, col = "#FF000050")
   legend("topright", legend = c("Observed", "Estimated"),
          pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
          col = c("#00000080", "red"),
          pt.bg = c("#00000030", NA))
   pause()
 }
}


# #'@export
# #'
# #'
# plot.sls <- function(par, y, mat, trans = I) {
#   points(trans(eval(parse(text = par))), y, pch = 21,
#          bg = "#00000030", col = "#00000080")
#   f <- mat[par, "Estimate"]
#   se <- mat[par, "Std. Error"]
#   points(trans(f), y, pch = 19, col = "red")
#   segments(trans(f - 2 * se), y, trans(f + 2 * se), y, pch = 19,
#            col = "red")
# }
#
# par(mfrow = c(2, 1), mar = c(4, 6, 0, 0), oma = c(.5, .5, .5, .5),
#     mgp = c(2, 0.5, 0), cex = 1, tck = -0.02)
# plot(1, 1, xlim = c(0, 2), ylim = c(1, 4), type = "n",
#      xlab = "Coefficient value", ylab = "", yaxt = "n")
# axis(2, at = 1:4, labels = c("a", "b", "sigma_obs", "sigma_proc"),
#      las = 1)
# plot_fixed("a", 1, fixed)
# plot_fixed("b", 2, fixed)
# plot_fixed("log_sigma_obs", 3, fixed, trans = function(z) exp(z))
# plot_fixed("log_sigma_proc", 4, fixed, trans = function(z) exp(z))

# CPUE_pred <- res$stats$statistics[10:18,"Mean"]
# x <- 1:9
# cpue <- exp(data$log_CPUE)
# plot(x, cpue, col = "#00000080", pch = 21, bg = "#00000030", las = 1,
#      ylab = "CPUE", xlab = "time")
# lines(x, cpue, lty = 2, lwd = 2)
# lines(x, CPUE_pred, col = "red", lty = 1, lwd = 1.5)
# polygon(c(x, rev(x)), c(u - 2 * u_se, rev(u + 2 * u_se)),
#         border = NA, col = "#FF000050")
# legend("bottomright", legend = c("Observed", "Estimated"),
#        pch = c(21, NA), lty = c(NA, 1), lwd = c(NA, 1.5),
#        col = c("#00000080", "red"),
#        pt.bg = c("#00000030",  NA))

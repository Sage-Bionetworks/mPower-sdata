require(rjson)
require(fractal)
require(pracma)

ShapeBalanceData <- function(x){
  timestamp <- sapply(x, function(x) x$timestamp)
  timestamp <- timestamp - timestamp[1]
  accel <- sapply(x, function(x) x$userAcceleration)
  accel <- t(accel)
  dat <- data.frame(timestamp, accel)
  dat$x <- as.numeric(dat$x) * 9.8
  dat$y <- as.numeric(dat$y) * 9.8
  dat$z <- as.numeric(dat$z) * 9.8
  return(dat)
}

TrimData <- function(dat, timeStart = 5, timeEnd = NULL){
  time <- dat$timestamp
  if (is.null(timeEnd)) {
    timeEnd <- time[length(time)]
  } 
  iStart <- min(which(time >= timeStart))
  iEnd <- max(which(time <= timeEnd))
  dat <- dat[iStart:iEnd,]
  dat$timestamp <- dat$timestamp - dat$timestamp[1]
  return(dat)
}

GetBalanceFeatures <- function(dat, timeStart = 5, timeEnd = NULL){
  dat <- TrimData(dat, timeStart, timeEnd)
  x <- dat$x
  y <- dat$y
  z <- dat$z
  aa <- sqrt(x^2 + y^2 + z^2)
  ###############################
  meanAA <- mean(aa, na.rm = TRUE)
  sdAA <- sd(aa, na.rm = TRUE)
  modeAA <- Mode(aa)
  skewAA <- Skewness(aa)
  kurAA <- Kurtosis(aa)
  auxAA <- quantile(aa, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  q1AA <- auxAA[[2]]
  medianAA <- auxAA[[3]]
  q3AA <- auxAA[[4]]
  iqrAA <- q3AA - q1AA
  rangeAA <- auxAA[[5]] - auxAA[[1]]  
  acfAA <- acf(aa, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
  zcrAA <- ZCR(aa)
  dfaAA <- try(DFA(aa, sum.order = 1)[[1]], silent = TRUE)
  if (inherits(dfaAA, "try-error")) {
    dfAA <- NA
  }
  out <- c(meanAA, sdAA, modeAA, skewAA, kurAA, q1AA, medianAA, q3AA, 
           iqrAA, rangeAA, acfAA, zcrAA, dfaAA)
  names(out) <- c("meanAA", "sdAA", "modeAA", "skewAA", "kurAA", "q1AA", 
                  "medianAA", "q3AA", "iqrAA", "rangeAA", "acfAA", "zcrAA", 
                  "dfaAA")  
  bpa <- FeaturesBpa(dat)
  dis <- BoxVolumeFeature(dat)
  return(c(out, bpa, dis))
}

FeaturesBpa <- function(post){
  ft <- rep(NA, 3)
  time <- post[, 1] - post[1, 1] 
  dTime <- time[length(time)] - time[1]
  post <- post[, -1]
  N <- nrow(post)
  # Orientation
  mg <- apply(post, 2, mean)  
  # Orientation-corrected force signals
  postforce <- post - matrix(rep(mg, N), N, 3, byrow = TRUE)  
  # Scaled velocity signals
  dt <- diff(time)
  dt <- c(dt, dt[length(dt)])
  postvel <- apply(postforce * matrix(rep(dt, 3), ncol = 3), 2, cumsum)
  # Average scaled power X, Y, Z
  postpower <- mean(apply(0.5 * 70 * postvel^2, 2, sum)/dTime)/1e4 
  # Force vector magnitude signal
  postmag <- sqrt(apply(postforce^2, 1, sum)) 
  # Maximum force
  postpeak <- as.numeric(quantile(postmag, 0.95))/10  
  # Detrended fluctuation analysis scaling exponent
  alpha <- DFA(postmag, sum.order = 1)[[1]]  
  # Output posture test feature vector
  ft <- c(postpeak, postpower, alpha)    
  names(ft) <- c("postpeak", "postpower", "alpha")
  return(ft)
}

Skewness <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(NA)
  } else{
    mu <- mean(x)
    return(mean((x - mu)^3)/(mean((x - mu)^2)^(3/2)))
  }
}

Kurtosis <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(NA)
  } else{
    mu <- mean(x)
    return(mean((x - mu)^4)/(mean((x - mu)^2)^2))
  }
}

ZCR <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(NA)
  } else{
    n <- length(x)
    aux.x <- rep(1, n)
    aux.x[x <= mean(x)] <- -1
    return(sum(aux.x[-n] * aux.x[-1] < 0)/(n - 1))
  }
}

GetDisplacement <- function(time, accel){
  deltaTime <- diff(time)
  n <- length(deltaTime)
  vel <- rep(NA, n)
  dis <- rep(NA, n)
  vel[1] <- 0
  dis[1] <- 0  
  for (i in 2:n) {
    vel[i] <- vel[i-1] + 0.5 * (accel[i] + accel[i-1]) * deltaTime[i]
    dis[i] <- dis[i-1] + 0.5 * (vel[i] + vel[i-1]) * deltaTime[i]
  }
  return(list(vel = vel, dis = dis))
}

GetXYZDisplacement <- function(x){
  disX <- GetDisplacement(x$timestamp, x$x)$dis
  disY <- GetDisplacement(x$timestamp, x$y)$dis
  disZ <- GetDisplacement(x$timestamp, x$z)$dis
  return(list(disX = disX, disY = disY, disZ = disZ))
}

CenterAcceleration <- function(x){
  x$x <- x$x - median(x$x)
  x$y <- x$y - median(x$y)
  x$z <- x$z - median(x$z)
  return(x)
}

BoxVolumeFeature <- function(x){
  x <- CenterAcceleration(x)
  aux <- GetXYZDisplacement(x)
  rdx <- range(aux$disX, na.rm = TRUE)
  rdy <- range(aux$disY, na.rm = TRUE)
  rdz <- range(aux$disZ, na.rm = TRUE)
  dVol <- diff(rdx) * diff(rdy) * diff(rdz)
  rddx <- range(diff(aux$disX), na.rm = TRUE)
  rddy <- range(diff(aux$disY), na.rm = TRUE)
  rddz <- range(diff(aux$disZ), na.rm = TRUE)
  ddVol <- diff(rddx) * diff(rddy) * diff(rddz)
  vols <- c(dVol, ddVol)
  names(vols) <- c("dVol", "ddVol")
  return(vols)
}

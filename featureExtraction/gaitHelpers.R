require(rjson)
require(fractal)
require(pracma)
require(lomb)

ShapeGaitData <- function(x) {
  timestamp <- sapply(x, function(x) x$timestamp)
  timestamp <- timestamp - timestamp[1]
  accel <- sapply(x, function(x) x$userAcceleration)
  accel <- t(accel)
  dat <- data.frame(timestamp, accel)
  dat$x <- as.numeric(dat$x)
  dat$y <- as.numeric(dat$y)
  dat$z <- as.numeric(dat$z)
  return(dat)
}

SingleAxisFeatures <- function(x, t, varName) {
  meanX <- mean(x, na.rm = TRUE)
  sdX <- sd(x, na.rm = TRUE)
  modeX <- Mode(x)
  skewX <- Skewness(x)
  kurX <- Kurtosis(x)
  auxX <- quantile(x, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
  q1X <- auxX[[2]]
  medianX <- auxX[[3]]
  q3X <- auxX[[4]]
  iqrX <- q3X - q1X
  rangeX <- auxX[[5]] - auxX[[1]]  
  acfX <- acf(x, lag.max = 1, plot = FALSE)$acf[2, 1, 1]
  zcrX <- ZCR(x)
  dfaX <- try(DFA(x, sum.order = 1)[[1]], silent = TRUE)
  if (inherits(dfaX, "try-error")) {
    dfaX <- NA
  }
  cvX <- Cv(x)
  tkeoX <- MeanTkeo(x)
  lspX <- lsp(cbind(t, x), plot = FALSE)
  F0X <- lspX$peak.at[1]
  P0X <- lspX$peak
  out <- c(meanX, sdX, modeX, skewX, kurX, q1X, medianX, q3X, iqrX, rangeX, acfX, 
           zcrX, dfaX, cvX, tkeoX, F0X, P0X)
  nms <- c("mean", "sd", "mode", "skew", "kur", "q1", "median", "q3", "iqr", "range", "acf", 
           "zcr", "dfa", "cv", "tkeo", "F0X", "P0X")
  names(out) <- paste(nms, varName, sep = "")
  return(out)
}

AccelLowPassFilter <- function(x, alpha) {
  n <- nrow(x)
  ax <- x$x
  ay <- x$y
  az <- x$z
  for (i in 2:n) {
    ax[i] <- alpha * ax[i] + (1 - alpha) * ax[i-1]
    ay[i] <- alpha * ay[i] + (1 - alpha) * ay[i-1]
    az[i] <- alpha * az[i] + (1 - alpha) * az[i-1]
  }
  x$x <- ax
  x$y <- ay
  x$z <- az
  return(x)
}

GetGaitFeatures <- function(dat, alpha = 1) {
  dat <- AccelLowPassFilter(dat, alpha)
  x <- dat$x
  y <- dat$y
  z <- dat$z
  aa <- sqrt(x^2 + y^2 + z^2)
  aj <- sqrt(diff(x)^2 + diff(y)^2 + diff(z)^2)
  t <- dat[, "timestamp"]
  ###############################
  outX <- SingleAxisFeatures(x, t, varName = "X")
  outY <- SingleAxisFeatures(y, t, varName = "Y")
  outZ <- SingleAxisFeatures(z, t, varName = "Z")
  outAA <- SingleAxisFeatures(aa, t, varName = "AA")
  outAJ <- SingleAxisFeatures(aj, t[-1], varName = "AJ")
  ###############################
  corXY <- cor(x, y, use = "p")
  corXZ <- cor(x, z, use = "p")
  corYZ <- cor(z, y, use = "p")
  cors <- c(corXY, corXZ, corYZ)
  names(cors) <- c("corXY", "corXZ", "corYZ") 
  c(outX, outY, outZ, outAA, outAJ, cors)
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

Cv <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(NA)
  } else{
    return((sd(x)/mean(x)) * 100)
  }
}

## Mean Teager-Kaiser energy operator of inter-taps
## intervals (from TKEO function in library(seewave)
## using f = 1, m = 1, M = 1)
MeanTkeo <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(NA)
  } else{
    y <- x^2 - c(x[-1], NA) * c(NA, x[1:(length(x) - 1)])
    return(mean(y, na.rm = TRUE))
  }
}

## TAPPING HELPER FUNCTIONS

GetXY <- function(x){
  x <- substr(x, 2, nchar(x) - 1)
  as.numeric(strsplit(x, ", ")[[1]])
}

ShapeTappingData <- function(x){
  time <- sapply(x, "[[", "TapTimeStamp")
  buttonid <- sapply(x, "[[", "TappedButtonId")
  tapcoord <- sapply(x, "[[", "TapCoordinate")
  coord <- sapply(tapcoord, GetXY)
  X <- coord[1,]
  Y <- coord[2,]  
  data.frame(time=time, X=X, Y=Y, buttonid=buttonid)
}

CleanTappedButtonNone <- function(x) {
  il <- x$buttonid == "TappedButtonLeft" ## get indexes of taps on left button 
  ir <- x$buttonid == "TappedButtonRight" ## get indexes of taps on right button
  ino <- x$buttonid == "TappedButtonNone" ## get indexes of taps outside the button
  xx <- rbind(x[il,], x[ir,], x[ino,]) ## create new matrix where the data from taps outside the button is at the bottom
  dupli <- duplicated(cbind(xx$X, xx$Y)) ## determine which data is duplicated
  ## we only want to drop TappedButtonNone duplications
  ## so we force a FALSE for data corresponding to taps on the right and left buttons 
  nlr <- sum(il) + sum(ir)
  dupli[seq(nlr)] <- FALSE
  ############################
  xx <- xx[which(!dupli),] ## now we remove on the duplicated data from taps outside the buttons
  xx[order(xx[, 1]),] ## order the data according to time
}

## Computes tapping time series
## (tapping interval and tapping position)
## dat expected to have: 
##   time 
##   X, Y touch screen coordinates
GetLeftRightEventsAndTapIntervals <- function(dat, depressThr = 20){
  tapT <- dat$time - dat$time[1]
  ## calculate X offset
  tapX <- dat$X
  tapX <- tapX - mean(tapX)
  ## find left/right finger "depress" event
  dX <- diff(tapX)
  i <- which(abs(dX) > depressThr)
  ## filter data
  dat <- dat[i,]
  tapT <- tapT[i]
  ## find depress event intervals
  tapInter <- diff(tapT)
  return(list(dat = dat, tapInter = tapInter))
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

Fatigue <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(list(fatigue10 = NA,
                fatigue25 = NA,
                fatigue50 = NA))
  } else{
    n <- length(x)
    top10 <- round(0.1 * n)
    top25 <- round(0.25 * n)
    top50 <- floor(0.5 * n)
    return(list(fatigue10 = mean(x[1:top10]) - mean(x[(n - top10):n]),
                fatigue25 = mean(x[1:top25]) - mean(x[(n - top25):n]),
                fatigue50 = mean(x[1:top50]) - mean(x[(n - top50):n])))
  }
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

Acf <- function(x){
  x <- x[!is.na(x)]
  if(length(x) < 3){
    return(list(NA, NA, NA))
  } else{
    return(acf(x, 2, plot = FALSE)$acf)
  }
}

Drift <- function(x, y) {
  dx <- diff(x, lag = 1)
  dy <- diff(y, lag = 1)
  sqrt(dx^2 + dy^2)
}

## FEATURE EXTRACTION
extractTapping <- function(x){
  aux <- GetLeftRightEventsAndTapIntervals(x, depressThr = 20)
  tapInter <- aux$tapInter
  dat <- aux$dat
  if(nrow(dat)==0){
    return(NULL)
  }
  meanX <- mean(dat$X)
  iL <- dat$X < meanX
  iR <- dat$X >= meanX
  driftLeft <- Drift(dat[iL, "X"], dat[iL, "Y"])
  driftRight <- Drift(dat[iR, "X"], dat[iR, "Y"])
  auxAcf <- try(Acf(tapInter))
  if (inherits(auxAcf, "try-error")) {
    auxAcf <- list(NA, NA, NA)
  }
  auxFatigue <- try(Fatigue(tapInter))
  if (inherits(auxFatigue, "try-error")) {
    auxFatigue <- list(NA, NA, NA)
  }
  auxDfa <- try(DFA(tapInter, sum.order = 1)[[1]], silent = TRUE)
  if (inherits(auxDfa, "try-error")) {
    auxDfa <- NA
  }
  res <-   c(meanTapInter  = mean(tapInter, na.rm = TRUE),
             medianTapInter = median(tapInter, na.rm = TRUE),
             iqrTapInter = IQR(tapInter, type = 7, na.rm = TRUE),
             minTapInter = min(tapInter, na.rm = TRUE),
             maxTapInter = max(tapInter, na.rm = TRUE),
             skewTapInter = Skewness(tapInter),
             kurTapInter = Kurtosis(tapInter),
             sdTapInter = sd(tapInter, na.rm = TRUE),
             madTapInter = mad(tapInter, na.rm = TRUE),
             cvTapInter = Cv(tapInter),
             rangeTapInter = diff(range(tapInter, na.rm = TRUE)),
             tkeoTapInter = MeanTkeo(tapInter),
             dfaTapInter = auxDfa,
             ar1TapInter = auxAcf[[2]],
             ar2TapInter = auxAcf[[3]],
             fatigue10TapInter = auxFatigue[[1]],
             fatigue25TapInter = auxFatigue[[2]],
             fatigue50TapInter = auxFatigue[[3]],    
             meanDriftLeft = mean(driftLeft, na.rm = TRUE),
             medianDriftLeft = median(driftLeft, na.rm = TRUE),
             iqrDriftLeft = IQR(driftLeft, type = 7, na.rm = TRUE),
             minDriftLeft = min(driftLeft, na.rm = TRUE),
             maxDriftLeft = max(driftLeft, na.rm = TRUE),
             skewDriftLeft = Skewness(driftLeft),
             kurDriftLeft = Kurtosis(driftLeft),
             sdDriftLeft = sd(driftLeft, na.rm = TRUE),
             madDriftLeft = mad(driftLeft, na.rm = TRUE),
             cvDriftLeft = Cv(driftLeft),
             rangeDriftLeft = diff(range(driftLeft, na.rm = TRUE)),
             meanDriftRight = mean(driftRight, na.rm = TRUE),
             medianDriftRight = median(driftRight, na.rm = TRUE),
             iqrDriftRight = IQR(driftRight, type = 7, na.rm = TRUE),
             minDriftRight = min(driftRight, na.rm = TRUE),
             maxDriftRight = max(driftRight, na.rm = TRUE),
             skewDriftRight = Skewness(driftRight),
             kurDriftRight = Kurtosis(driftRight),
             sdDriftRight = sd(driftRight, na.rm = TRUE),
             madDriftRight = mad(driftRight, na.rm = TRUE),
             cvDriftRight = Cv(driftRight),
             rangeDriftRight = diff(range(driftRight, na.rm = TRUE)),    
             numberTaps = nrow(dat),
             buttonNoneFreq = sum(dat$buttonid == "TappedButtonNone")/nrow(dat),
             corXY = cor(dat$X, dat$Y, use = "p"))
  return(res)
}

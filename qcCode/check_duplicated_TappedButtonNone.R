
GetXY <- function(x) {
  x <- substr(x, 2, nchar(x) - 1)
  as.numeric(strsplit(x, ", ")[[1]])
}

ShapeTappingData <- function(x) {
  time <- sapply(x, function(x) x$TapTimeStamp)
  buttonid <- sapply(x, function(x) x$TappedButtonId)
  tapcoord <- sapply(x, function(x) x$TapCoordinate)
  coord <- sapply(tapcoord, GetXY)
  X <- coord[1,]
  Y <- coord[2,]  
  data.frame(time, X, Y, buttonid)
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

#################################
#################################
#################################


require(synapseClient)

tapping <- synTableQuery("SELECT * FROM syn5511439")
dim(tapping@values)

rnms <- rownames(tapping@values)

r1 <- rep(NA, length(rnms))
r2 <- rep(NA, length(rnms))
for (i in seq(length(rnms))) {
  cat(i, "\n")
  try(fnms <- synDownloadTableFile(tapping, rnms[i], "tapping_results.json.TappingSamples"), silent = TRUE)
  if (class(fnms) != "try-error") {
    if (length(fnms) > 0) {
      dat <- fromJSON(fnms)
      dat1 <- ShapeTappingData(dat)
      dat2 <- CleanTappedButtonNone(dat1) 
      r1[i] <- nrow(dat1)
      r2[i] <- nrow(dat2) 
    }      
  }
}


## r1/r2 close to 2 indicates the problem




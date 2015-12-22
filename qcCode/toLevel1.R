require(synapseClient)
require(rjson)
synapseLogin()

## CORE METADATA
coreNames <- c("recordId", "healthCode", "createdOn", "appVersion", "phoneInfo")
releaseVersions <- c("version 1.0, build 7", "version 1.0.5, build 12", "version 1.1, build 22")
firstDate <- as.Date("2015-03-09")
lastDate <- as.Date("2015-09-09")

## x IS EXPECTED TO BE A CHARACTER VECTOR TO BE CLEANED UP
cleanString <- function(x){
  gsub('[', '', gsub(']', '', gsub('["', '', gsub('"]', '', x, fixed=T), fixed=T), fixed=T), fixed=T)
}

## x IS EXPECTED TO BE A LIST OF COLUMN MODEL OBJECTS
whichFilehandle <- function(x){
  cc <- sapply(as.list(1:length(x)), function(y){
    if(x[[y]]@columnType=="FILEHANDLEID"){
      return(x[[y]]@name)
    } else{
      return(NULL)
    }
  })
  cc <- unlist(cc)
  return(cc)
}


#####
## ENROLLMENT
#####
eId <- c("syn4961453")
eSc <- synGet(eId)
eStringCols <- sapply(as.list(1:length(eSc@columns)), function(x){
  if(eSc@columns[[x]]@columnType=="STRING"){
    return(eSc@columns[[x]]@name)
  } else{
    return(NULL)
  }
})
eStringCols <- unlist(eStringCols)
eStringCols <- eStringCols[ eStringCols != "race" ]

eTab <- synTableQuery(paste0("SELECT * FROM ", eId, " WHERE appVersion NOT LIKE '%YML%'"))

eDat <- eTab@values
for(i in eStringCols){
  eDat[[i]] <- cleanString(eDat[[i]])
}
eDat$race <- gsub('[', '', gsub(']', '', eDat$race, fixed=T), fixed=T)
eDat$externalId <- NULL
eDat$uploadDate <- NULL
eDat$Enter_State <- NULL
eDat$`last-smoked` <- as.numeric(format(eDat$`last-smoked`, "%Y"))
eDat$employment[ which(eDat$employment=="Military") ] <- "Employment for wages"
eDat$employment[ which(eDat$employment=="Out of work but not currently looking for work") ] <- "Out of work"
eDat$employment[ which(eDat$employment=="Out of work and looking for work") ] <- "Out of work"
eDat$`packs-per-day` <- as.integer(eDat$`packs-per-day`)
eDat$age[ which(eDat$age>90 & eDat$age<101) ] <- 90

## PULL IN THE COMORBIDITIES
eComFiles <- synDownloadTableColumns(eTab, "health-history")
eCom <- sapply(eComFiles, readLines)
for(rn in rownames(eDat)){
  if(!is.na(eDat[rn, "health-history"])){
    eDat[rn, "health-history"] <- eCom[[eDat[rn, "health-history"]]]
  }
}
eDat$`health-history` <- gsub(' (TIA)', '', gsub(' (COPD)', '', gsub('[', '', gsub(']', '', eDat$`health-history`, fixed=T), fixed=T), fixed=T), fixed=T)

## KEEP THE FIRST INSTANCE OF ENROLLMENT SURVEY
eDat <- eDat[ !duplicated(eDat$healthCode), ]
rownames(eDat) <- eDat$recordId

## THESE ENTERED INVALID AGES - EVEN THOUGH TWICE IN REGISTRATION CERTIFIED THAT OVER 18
theseOnes <- eDat$healthCode[ which(eDat$age < 18  | eDat$age > 100) ]

## x IS A DATAFRAME TO BE SUBSETTING STANDARDLY
subsetThis <- function(x){
  xSub <- x[, setdiff(names(x), coreNames)]
  xIdx <- rowSums(is.na(xSub)) != ncol(xSub)
  x <- x[ xIdx, ]
  x <- x[ as.Date(x$createdOn) >= firstDate & as.Date(x$createdOn) <= lastDate, ]
  x <- x[ x$appVersion %in% releaseVersions, ]
  x <- x[ which(!(x$healthCode %in% theseOnes)), ]
  x <- x[ which(!duplicated(x[, c("healthCode", "createdOn")])), ]
  x[ order(x$createdOn), ]
}
eDat <- subsetThis(eDat)

#####
## UPDRS
#####
uId <- c("syn4961480")
uSc <- synGet(uId)
uStringCols <- sapply(as.list(1:length(uSc@columns)), function(x){
  if(uSc@columns[[x]]@columnType=="STRING"){
    return(uSc@columns[[x]]@name)
  } else{
    return(NULL)
  }
})
uStringCols <- unlist(uStringCols)

uTab <- synTableQuery(paste0("SELECT * FROM ", uId, " WHERE appVersion NOT LIKE '%YML%'"))

uDat <- uTab@values
for(i in uStringCols){
  uDat[[i]] <- cleanString(uDat[[i]])
}
uDat$externalId <- NULL
uDat$uploadDate <- NULL
uDat$`MDS-UPDRS1.1` <- uDat$`MDS-UPRDRS1.1`
uDat$`MDS-UPRDRS1.1` <- NULL
uDat <- subsetThis(uDat)
rownames(uDat) <- uDat$recordId

#####
## PDQ8
#####
pId <- c("syn4961472")
pSc <- synGet(pId)
pStringCols <- sapply(as.list(1:length(pSc@columns)), function(x){
  if(pSc@columns[[x]]@columnType=="STRING"){
    return(pSc@columns[[x]]@name)
  } else{
    return(NULL)
  }
})
pStringCols <- unlist(pStringCols)

pTab <- synTableQuery(paste0("SELECT * FROM ", pId, " WHERE appVersion NOT LIKE '%YML%'"))

pDat <- pTab@values
for(i in pStringCols){
  pDat[[i]] <- cleanString(pDat[[i]])
}
pDat$externalId <- NULL
pDat$uploadDate <- NULL
pDat$`PDQ8-4` <- pDat$`PQD8-4`
pDat$`PQD8-4` <- NULL
pDat <- pDat[, c(names(pDat)[-grep("PDQ", names(pDat))], paste('PDQ8', 1:8, sep="-"))]

pDat <- subsetThis(pDat)
rownames(pDat) <- pDat$recordId


#####
## MEMORY
#####
mId <- c("syn4961459")
mSc <- synGet(mId)
mFilehandleCols <- whichFilehandle(mSc@columns)

mTab <- synTableQuery(paste0("SELECT * FROM ", mId, " WHERE appVersion NOT LIKE '%YML%'"))
mDat <- mTab@values
mDat$externalId <- NULL
mDat$uploadDate <- NULL
mDat$momentInDayFormat.json.choiceAnswers <- cleanString(mDat$momentInDayFormat.json.choiceAnswers)

mDat <- subsetThis(mDat)
rownames(mDat) <- mDat$recordId

#####
## TAPPING
#####
tId <- c("syn4961463", "syn4961465", "syn4961484")
tSc <- synGet(tId[length(tId)])
tFilehandleCols <- whichFilehandle(tSc@columns)

tAll <- lapply(as.list(tId), function(x){
  vals <- synTableQuery(paste0("SELECT * FROM ", x, " WHERE appVersion NOT LIKE '%YML%'"))@values
  return(vals)
})
tAllNames <- unique(unlist(sapply(tAll, names)))
tAll <- lapply(tAll, function(x){
  these <- setdiff(tAllNames, names(x))
  x[, these] <- NA
  return(x[, tAllNames])
})
tDat <- do.call(rbind, tAll)
rownames(tDat) <- tDat$recordId

tDat$externalId <- NULL
tDat$uploadDate <- NULL
tDat$tapping_results.json.item <- NULL
tDat$momentInDayFormat.json.saveable <- NULL
tDat$momentInDayFormat.json.answer <- NULL
tDat$momentInDayFormat.json.userInfo <- NULL
tDat$momentInDayFormat.json.questionTypeName <- NULL
tDat$momentInDayFormat.json.questionType <- NULL
tDat$momentInDayFormat.json.item <- NULL
tDat$momentInDayFormat.json.endDate <- NULL
tDat$momentInDayFormat.json.startDate <- NULL
tDat$accelerometer_tapping.items <- NULL
tDat$momentInDayFormat.json.choiceAnswers <- cleanString(tDat$momentInDayFormat.json.choiceAnswers)

tDat <- subsetThis(tDat)
rownames(tDat) <- tDat$recordId

#####
## VOICE
#####
vId1 <- c("syn4961455", "syn4961457", "syn4961464")
vId2 <- c("syn4961456")

vSc <- synGet(vId2)
vFilehandleCols <- whichFilehandle(vSc@columns)

## FIRST SET OF IDS HAVE TO PARSE INTO momentInDayFormat.json FILES TO EXTRACT MED INFO
vFirst <- lapply(as.list(vId1), function(x){
  vTab <- synTableQuery(paste0("SELECT * FROM ", x, " WHERE appVersion NOT LIKE '%YML%'"))
  vals <- vTab@values
  
  vMap <- synDownloadTableColumns(vTab, "momentInDayFormat.json")
  vMID <- sapply(as.list(rownames(vals)), function(rn){
    if( is.na(vals[rn, "momentInDayFormat.json"]) ){
      return(c(choiceAnswers=NA))
    } else{
      loc <- vMap[[vals[rn, "momentInDayFormat.json"]]]
      dat <- try(fromJSON(file=loc))
      if( class(dat) == "try-error" ){
        return(c(choiceAnswers=NA))
      } else{
        return(unlist(dat))
      }
    }
  })
  vAllNames <- unique(unlist(sapply(vMID, names)))
  vMID <- lapply(vMID, function(y){
    these <- setdiff(vAllNames, names(y))
    y[ these ] <- NA
    return(y[ vAllNames ])
  })
  vMID <- do.call(rbind, vMID)
  vMID <- as.data.frame(vMID, stringsAsFactors=FALSE)
  names(vMID) <- paste("momentInDayFormat.json", names(vMID), sep=".")
  vals$momentInDayFormat.json <- NULL
  vals$momentInDayFormat.json.choiceAnswers <- vMID$momentInDayFormat.json.choiceAnswers
  
  return(vals)
})
vFirst <- do.call(rbind, vFirst)
rownames(vFirst) <- vFirst$recordId

## SECOND SET (1) IS AS WE WOULD EXPECT
vSecond <- synTableQuery(paste0("SELECT * FROM ", vId2, " WHERE appVersion NOT LIKE '%YML%'"))@values
rownames(vSecond) <- vSecond$recordId

vDat <- rbind(vFirst, vSecond)
vDat$externalId <- NULL
vDat$uploadDate <- NULL
vDat <- subsetThis(vDat)

#####
## WALKING
#####
wId <- c("syn4961452", "syn4961466", "syn4961469")

wAll <- lapply(as.list(wId), function(x){
  vals <- synTableQuery(paste0("SELECT * FROM ", x, " WHERE appVersion NOT LIKE '%YML%'"))@values
  return(vals)
})
wAllNames <- unique(unlist(sapply(wAll, names)))
wAll2 <- lapply(wAll, function(x){
  these <- setdiff(wAllNames, names(x))
  x[, these] <- NA
  return(x[, wAllNames])
})
wDat <- do.call(rbind, wAll2)

wDat$externalId <- NULL
wDat$uploadDate <- NULL
wDat$momentInDayFormat.json.answers <- NULL
wDat$momentInDayFormat.json.item <- NULL
wDat$momentInDayFormat.json.endDate <- NULL
wDat$momentInDayFormat.json.questionType <- NULL
wDat$momentInDayFormat.json.questionTypeName <- NULL
wDat$momentInDayFormat.json.saveable <- NULL
wDat$momentInDayFormat.json.startDate <- NULL
wDat$momentInDayFormat.json.userInfo <- NULL
wDat$pedometer_walking.outbound.items <- NULL
wDat$accelerometer_walking.rest.items <- NULL
wDat$deviceMotion_walking.rest.items <- NULL

wDat <- subsetThis(wDat)
rownames(wDat) <- wDat$recordId


################################################
################################################
## NOW DO CLEANUP OF MISSING MED DATA FOR ACTIVITIES
theseColumns <- c("recordId", "healthCode", "createdOn", "momentInDayFormat.json.choiceAnswers")
allActs <- rbind(mDat[, theseColumns], tDat[, theseColumns], vDat[, theseColumns], wDat[, theseColumns])
allActs <- allActs[ order(allActs$healthCode, allActs$createdOn), ]
allActs$momentInDayFormat.json.choiceAnswers <- sub('"]', '', sub('["', '', allActs$momentInDayFormat.json.choiceAnswers, fixed=T))
reDo <- lapply(as.list(unique(allActs$healthCode)), function(pt){
  this <- allActs[ allActs$healthCode==pt, ]
  if( nrow(this) > 1 ){
    for( rec in 2:nrow(this) ){
      if( this$createdOn[rec]-this$createdOn[rec-1] < (60*20) ){
        if( is.na(this$momentInDayFormat.json.choiceAnswers[rec]) ){
          this$momentInDayFormat.json.choiceAnswers[rec] <- this$momentInDayFormat.json.choiceAnswers[rec-1]
        } else if( this$momentInDayFormat.json.choiceAnswers[rec] %in% c("", "[]") ){
          this$momentInDayFormat.json.choiceAnswers[rec] <- this$momentInDayFormat.json.choiceAnswers[rec-1]
        }
      }
    }
  }
  return(this)
})
newAllActs <- do.call(rbind, reDo)

## MERGE BACK INTO EACH TABLE
mDat$medTimepoint <- newAllActs[ rownames(mDat), "momentInDayFormat.json.choiceAnswers" ]
mDat$momentInDayFormat.json.choiceAnswers <- NULL
tDat$medTimepoint <- newAllActs[ rownames(tDat), "momentInDayFormat.json.choiceAnswers" ]
tDat$momentInDayFormat.json.choiceAnswers <- NULL
vDat$medTimepoint <- newAllActs[ rownames(vDat), "momentInDayFormat.json.choiceAnswers" ]
vDat$momentInDayFormat.json.choiceAnswers <- NULL
wDat$medTimepoint <- newAllActs[ rownames(wDat), "momentInDayFormat.json.choiceAnswers" ]
wDat$momentInDayFormat.json.choiceAnswers <- NULL

## ADDITIONAL SUBSETTING FOR MEMORY
mSub <- mDat[, grep("MemoryGameResults.json", names(mDat), fixed=T)]
mIdx <- rowSums(is.na(mSub)) != ncol(mSub)
mDat <- mDat[ mIdx, ]


################################################
################################################
## STORE BACK TO SYNAPSE
## LOG IN AS BRIDGE EXPORTER TO STORE BACK
# synapseLogout()
newParent <- "syn4993293"

storeThese <- list('Demographics Survey' = list(vals=eDat, fhCols=NULL),
                   'UPDRS Survey' = list(vals=uDat, fhCols=NULL),
                   'PDQ8 Survey' = list(vals=pDat, fhCols=NULL),
                   'Memory Activity' = list(vals=mDat, fhCols=intersect(names(mDat), mFilehandleCols)),
                   'Tapping Activity' = list(vals=tDat, fhCols=intersect(names(tDat), tFilehandleCols)),
                   'Voice Activity' = list(vals=vDat, fhCols=intersect(names(vDat), vFilehandleCols)),
                   'Walking Activity' = list(vals=wDat, fhCols=grep("json.items", names(wDat), value = TRUE)))

## SCHEMAS ALREADY STORED - FIND THEM
qq <- synQuery(paste0('SELECT id, name FROM table WHERE parentId=="', newParent, '"'))

## NOW LETS DO SOMETHING WITH ALL OF THIS DATA
## FINALLY, STORE THE OUTPUT
for(i in 1:length(storeThese)){
  thisId <- qq$table.id[qq$table.name == names(storeThese)[i]]
  ## REMOVE ALL ROWS FROM THE TABLE, IF THERE ARE ANY
  delThese <- synTableQuery(paste0('SELECT * FROM ', thisId))
  delThese <- synDeleteRows(delThese)
  thisFile <- as.tableColumns(storeThese[[i]]$vals)
  theEnd <- synStore(Table(synGet(thisId), thisFile$fileHandleId))
}


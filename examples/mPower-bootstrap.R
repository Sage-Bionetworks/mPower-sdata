require(synapseClient)
require(rjson)
synapseLogin()

## QUERY THE mPower PROJECT (syn4993293) FOR ALL OF THE TABLES
q <- synQuery('SELECT id, name FROM table WHERE parentId=="syn4993293"')
## SELECT ONLY THE SAMPLE TABLES, WHICH ARE PUBLICALLY AVAILABLE
q <- q[grep("Sample", q$table.name), ]

## READ IN THE SAMPLE DATA FROM EACH OF THE TABLES (RESULTS ARE CACHED LOCALLY)
allDat <- lapply(as.list(q$table.id), function(x){
  synTableQuery(paste0('SELECT * FROM ', x))
})
names(allDat) <- q$table.name

## LOOK AT THE VALUES FOR THE FIRST TWO OBSERVATIONS IN TAPPING TABLE
allDat$`Sample Tapping Activity`@values[1:2, ]

## FOR TABLES WITH COLUMNS THAT CONTAIN FILES, WE CAN BULK DOWNLOAD THE FILES AND STORE A MAPPING
## THE VALUE IN THE TABLE ABOVE IS CALLED A fileHandleId WHICH REFERENCES A FILE THAT CAN BE ACCESSED PROGRAMMATICALLY
## GET THE FILES THAT CONTAIN SCREEN TAP SAMPLES FROM THE TAPPING EXERCISE
## THIS CACHES THE RETRIEVED FILES AS WELL
tapMap <- synDownloadTableColumns(allDat$`Sample Tapping Activity`, "tapping_results.json.TappingSamples")

## THE NAMES OF tapMap ARE THE FILEHANDLES STORED IN THE COLUMN "tapping_results.json.TappingSamples" SO CAN ASSOCIATE WITH APPROPRIATE METADATA
## THESE ARE JSON FILES, SO READ THEM INTO MEMORY
tapResults <- lapply(as.list(tapMap), function(x){
  fromJSON(file=x)
})

require(synapseClient)
require(rjson)
synapseLogin()

firstDate <- as.Date("2015-03-09")
lastDate <- as.Date("2015-09-09")

## QUERY THE mPower PROJECT FOR ALL OF THE TABLES
q <- synQuery('SELECT id, name FROM table WHERE parentId=="syn4993293"')

## READ IN THE FIRST 20 OBSERVATIONS FROM EACH OF THE TABLES (RESULTS ARE CACHED LOCALLY)
allDat <- lapply(as.list(q$table.id), function(x){
  synTableQuery(paste0('SELECT * FROM ', x, ' LIMIT 20'))
})
names(allDat) <- q$table.name

## FOR TABLES WITH COLUMNS THAT CONTAIN FILES, WE CAN BULK DOWNLOAD THE FILES AND STORE A MAPPING
## GET THE FILES ASSOCIATED WITH THE TAPPING EXERCISE
## THIS CACHES THE RETRIEVED FILES AS WELL
tapMap <- synDownloadTableColumns(allDat$`Tapping Activity`, "tapping_results.json.TappingSamples")

## THE NAMES OF tapMap ARE THE FILEHANDLES STORED IN THE COLUMN "tapping_results.json.TappingSamples" SO CAN ASSOCIATE WITH APPROPRIATE METADATA
## THESE ARE JSON FILES, SO READ THEM INTO MEMORY
tapResults <- lapply(as.list(tapMap), function(x){
  fromJSON(file=x)
})

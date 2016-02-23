#####################################################################
## Extract Balance Features from mPower walking data
## 
## Authors: Elias Chaibub Neto, J. Christopher Bare, Brian M. Bot
## Sage Bionetworks (http://sagebase.org)
#####################################################################
require(githubr)
require(synapseClient)
synapseLogin()

## READ IN THE HELPER FUNCTIONS
mpowerRepo <- getRepo('brian-bot/mPower-sdata')
sourceRepoFile(mpowerRepo, 'featureExtraction/balanceHelpers.R')
helpCode <- getPermlink(mpowerRepo, 'featureExtraction/balanceHelpers.R')
execCode <- getPermlink(mpowerRepo, 'featureExtraction/balanceExtraction.R')

## GET BALANCE DATA
balanceTable <- synTableQuery('SELECT * FROM syn5511449 WHERE "deviceMotion_walking_rest.json.items" is not null')
balance <- balanceTable@values
balanceMap <- synDownloadTableColumns(balanceTable, "deviceMotion_walking_rest.json.items")

allFeatures <- lapply(as.list(balanceMap), function(x){
  rawDat <- fromJSON(file=x)
  tmp <- ShapeBalanceData(rawDat)
  GetBalanceFeatures(tmp)
})
names(allFeatures) <- names(balanceMap)

## SEVEN NULLS - OTHERWISE COMPLETE
featDf <- do.call(rbind, allFeatures)
featDf <- as.data.frame(featDf)
tmpNames <- colnames(featDf)
featDf$filehandle <- rownames(featDf)
featDf <- featDf[, c("filehandle", tmpNames)]

## WRITE OUT THE FEATURES
fPath <- file.path(tempdir(), "balanceFeatures.tsv")
write.table(featDf, file=fPath, sep="\t", row.names = FALSE, quote = FALSE)

## STORE AND RECORD PROVENANCE IN SYNAPSE
resFile <- synStore(File(path=fPath, parentId="syn5608426"),
                    activity=Activity(name="balance feature extraction",
                                      used=list(list(entity=balanceTable@schema, wasExecuted=FALSE),
                                                list(url=helpCode, name=basename(helpCode), wasExecuted=FALSE),
                                                list(url=execCode, name=basename(execCode), wasExecuted=TRUE))))

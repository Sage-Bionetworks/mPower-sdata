#####################################################################
## Extract Gait Features from mPower walking data
## 
## Authors: Elias Chaibub Neto, J. Christopher Bare, Brian M. Bot
## Sage Bionetworks (http://sagebase.org)
#####################################################################
require(githubr)
require(synapseClient)
synapseLogin()

## READ IN THE HELPER FUNCTIONS
mpowerRepo <- getRepo('brian-bot/mPower-sdata')
sourceRepoFile(mpowerRepo, 'featureExtraction/gaitHelpers.R')
helpCode <- getPermlink(mpowerRepo, 'featureExtraction/gaitHelpers.R')
execCode <- getPermlink(mpowerRepo, 'featureExtraction/gaitExtraction.R')

## GET GAIT DATA
gaitTable <- synTableQuery('SELECT * FROM syn5511449 WHERE "deviceMotion_walking_outbound.json.items" is not null')
gait <- gaitTable@values
gaitMap <- synDownloadTableColumns(gaitTable, "deviceMotion_walking_outbound.json.items")

allFeatures <- lapply(as.list(gaitMap), function(x){
  rawDat <- fromJSON(file=x)
  tmp <- ShapeGaitData(rawDat)
  GetGaitFeatures(tmp, alpha=1)
})
names(allFeatures) <- names(gaitMap)

## SEVEN NULLS - OTHERWISE COMPLETE
featDf <- do.call(rbind, allFeatures)
featDf <- as.data.frame(featDf)
tmpNames <- colnames(featDf)
featDf$filehandle <- rownames(featDf)
featDf <- featDf[, c("filehandle", tmpNames)]

## WRITE OUT THE FEATURES
fPath <- file.path(tempdir(), "gaitFeatures.tsv")
write.table(featDf, file=fPath, sep="\t", row.names = FALSE, quote = FALSE)

## STORE AND RECORD PROVENANCE IN SYNAPSE
resFile <- synStore(File(path=fPath, parentId="syn5608426"),
                    activity=Activity(name="gait feature extraction",
                                      used=list(list(entity=gaitTable@schema, wasExecuted=FALSE),
                                                list(url=helpCode, name=basename(helpCode), wasExecuted=FALSE),
                                                list(url=execCode, name=basename(execCode), wasExecuted=TRUE))))

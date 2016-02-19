require(synapseClient)
require(rGithubClient)
require(rjson)
require(fractal)
synapseLogin()

## READ IN THE HELPER FUNCTIONS
mpowerRepo <- getRepo('brian-bot/mPower-sdata')
sourceRepoFile(mpowerRepo, 'featureExtraction/tappingHelpers.R')
helpCode <- getPermlink(mpowerRepo, 'featureExtraction/tappingHelpers.R')
execCode <- getPermlink(mpowerRepo, 'featureExtraction/tappingExtraction.R')

## GET TAPPING DATA
tapTable <- synTableQuery('SELECT * FROM syn5511439 WHERE "tapping_results.json.TappingSamples" is not null')
tap <- tapTable@values
tapMap <- synDownloadTableColumns(tapTable, "tapping_results.json.TappingSamples")

allFeatures <- lapply(as.list(tapMap), function(x){
  rawDat <- fromJSON(file=x)
  tmp <- ShapeTappingData(rawDat)
  tmp <- CleanTappedButtonNone(tmp)
  extractTapping(tmp)
})
names(allFeatures) <- names(tapMap)

## SEVEN NULLS - OTHERWISE COMPLETE
featDf <- do.call(rbind, allFeatures)
featDf <- as.data.frame(featDf)
tmpNames <- colnames(featDf)
featDf$filehandle <- rownames(featDf)
featDf <- featDf[, c("filehandle", tmpNames)]

## WRITE OUT THE FEATURES
fPath <- file.path(tempdir(), "tapFeatures.tsv")
write.table(featDf, file=fPath, sep="\t", row.names = FALSE, quote = FALSE)

## STORE AND RECORD PROVENANCE IN SYNAPSE
resFile <- synStore(File(path=fPath, parentId="syn5608426"),
                    activity=Activity(name="tapping feature extraction",
                                      used=list(list(entity=tapTable@schema, wasExecuted=FALSE),
                                                list(url=helpCode, name=basename(helpCode), wasExecuted=FALSE),
                                                list(url=execCode, name=basename(execCode), wasExecuted=TRUE))))

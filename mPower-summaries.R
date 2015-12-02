require(synapseClient)
require(rGithubClient)
require(ggplot2)
synapseLogin()

firstDate <- as.Date("2015-03-09")
lastDate <- as.Date("2015-09-09")

## QUERY THE mPower PROJECT FOR ALL OF THE TABLES
q <- synQuery('SELECT id, name FROM table WHERE parentId=="syn4993293"')

## READ IN ALL OF THE TABLES
allDat <- lapply(as.list(q$table.id), function(x){
  synTableQuery(paste0('SELECT * FROM ', x))@values
})
names(allDat) <- q$table.name

## UNIQUE PARTICIPANTS AND TASKS
allParticipants <- lapply(lapply(allDat, "[[", "healthCode"), unique)
length(unique(unlist(allParticipants)))

## PARKINSON DIAGNOSIS
table(allDat$`Demographics Survey`$`professional-diagnosis`, useNA = 'always')

## PER TABLE METRICS
tableSummaries <- data.frame(uniqueParticipants = sapply(allParticipants, length),
                             uniqueTasks = sapply(allDat, nrow), stringsAsFactors = FALSE)
rownames(tableSummaries) <- names(allDat)
tableSummaries

## CREATE SOME PLOTS OF PARTICIPATION
plotDat <- lapply(allDat, function(x){
  x$date <- as.Date(x$createdOn)
  res1 <- x[, c("healthCode", "date")]
  res2 <- res1[!duplicated(x$healthCode), ]
  res1$Count <- "tasks"
  res2$Count <- "participants"
  return(rbind(res1, res2))
})

theseTasks <- c("Memory Activity", "Walking Activity", "Voice Activity", "Tapping Activity")
facetDat <- lapply(as.list(theseTasks), function(x){
  tmp <- plotDat[[x]]
  tmpTab <- as.data.frame(table(tmp$date, tmp$Count))
  names(tmpTab) <- c("date", "Count", "freq")
  tmpTab$freqCum[ tmpTab$Count=="participants" ] <- cumsum(tmpTab$freq[ tmpTab$Count=="participants" ])
  tmpTab$freqCum[ tmpTab$Count=="tasks" ] <- cumsum(tmpTab$freq[ tmpTab$Count=="tasks" ])
  tmpTab$taskName <- x
  return(tmpTab)
})
facetDat <- do.call(rbind, facetDat)
facetDat$date <- as.Date(facetDat$date)
facetDat$taskName <- factor(facetDat$taskName, levels = theseTasks)

fPlot <- ggplot(data=facetDat, aes(date, freqCum, fill=Count)) + 
  facet_grid(taskName ~ .) +
  geom_bar(alpha=.5, position="identity", stat="identity") + 
  xlim(firstDate, lastDate) +
  scale_fill_grey(start=0) +
  theme_bw() + 
  labs(x="Date", y="Cumulative Count") + 
  theme(legend.position="bottom")

fName <- file.path(tempdir(), "figure2-participantActivities.png")
png(fName, width = 5, height = 6, units = "in", res = 400)
show(fPlot)
dev.off()

## GET THIS CODE STORED IN GITHUB
mpowerRepo <- getRepo('brian-bot/mPower-sdata')
thisCode <- getPermlink(mpowerRepo, 'mPower-summaries.R')

## STORE THE PLOT IN SYNAPSE
finalOutput <- synStore(File(path=fName, parentId="syn5480005"), 
                        used=lapply(as.list(q$table.id), function(x){list(entity=x)}),
                        executed=list(list(url=thisCode, name=basename(thisCode))),
                        activityName="Figure Generation")


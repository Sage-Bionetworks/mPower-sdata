require(synapseClient)
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

finalPlots <- lapply(plotDat, function(x){
  x$Count <- factor(x$Count, levels=c("participants", "tasks"))
  tPlot <- ggplot(data=x, aes(date, fill=Count)) + 
    geom_histogram(data=x[x$Count=="tasks", ], aes(y=cumsum(..count..)), alpha=.5, position="identity", binwidth=1) +
    geom_histogram(data=x[x$Count=="participants", ], aes(y=cumsum(..count..)), alpha=.5, position="identity", binwidth=1) +
    xlim(firstDate, lastDate) +
    labs(x="Date", y="Cumulative Count")
  return(tPlot)
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
  labs(x="Date", y="Cumulative Count") + 
  theme(legend.position="bottom")

fName <- file.path(tempdir(), "facetPlot.png")
png(fName, width = 5, height = 6, units = "in", res = 400)
show(fPlot)
dev.off()


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
  return(x[, c("healthCode", "date")])
})

plotDat2 <- lapply(plotDat, function(x){
  res1 <- x
  res1$Count <- "tasks"
  res2 <- x[!duplicated(x$healthCode), ]
  res2$Count <- "participants"
  return(rbind(res1, res2))
})

finalPlots <- lapply(plotDat2, function(x){
  x$Count <- factor(x$Count, levels=c("participants", "tasks"))
  tPlot <- ggplot(data=x, aes(date, fill=Count)) + 
    geom_histogram(data=x[x$Count=="tasks", ], aes(y=cumsum(..count..)), alpha=.5, position="identity", binwidth=1) +
    geom_histogram(data=x[x$Count=="participants", ], aes(y=cumsum(..count..)), alpha=.5, position="identity", binwidth=1) +
    xlim(firstDate, lastDate) +
    labs(x="Date", y="Cumulative Count")
  return(tPlot)
})

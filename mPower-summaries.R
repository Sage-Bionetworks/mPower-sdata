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

#####
## CREATE SOME PLOTS OF PARTICIPATION
#####
partDat <- lapply(allDat, function(x){
  x$date <- as.Date(x$createdOn)
  res1 <- x[, c("healthCode", "date")]
  res2 <- res1[!duplicated(x$healthCode), ]
  res1$Count <- "tasks"
  res2$Count <- "participants"
  return(rbind(res1, res2))
})

theseTasks <- c("Memory Activity", "Walking Activity", "Voice Activity", "Tapping Activity")
facetDat <- lapply(as.list(theseTasks), function(x){
  tmp <- partDat[[x]]
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

pPlot <- ggplot(data=facetDat, aes(date, freqCum, fill=Count)) + 
  facet_grid(taskName ~ .) +
  geom_bar(alpha=.5, position="identity", stat="identity") + 
  xlim(firstDate, lastDate) +
  scale_fill_grey(start=0) +
  theme_bw() + 
  labs(x="Date", y="Cumulative Count") + 
  theme(legend.position="bottom")

fName2 <- file.path(tempdir(), "figure2-participantActivities.pdf")
pdf(fName2, width = 6, height = 8)
show(pPlot)
dev.off()

## PLOT USER PARTICIPATION OVER TIME
daysDat <- lapply(allDat, function(x){
  x$date <- as.Date(x$createdOn)
  res <- x[, c("healthCode", "date")]
  return(res)
})
daysDat <- do.call(rbind, daysDat)
daysDat <- daysDat[!duplicated(daysDat), ]

dd <- as.data.frame(table(daysDat$healthCode))
names(dd) <- c("healthCode", "days")
dd$park <- NA
tmpDiag <- allDat$`Demographics Survey`
rownames(tmpDiag) <- tmpDiag$healthCode
dd$park <- tmpDiag[dd$healthCode, "professional-diagnosis"]
## REMOVE THOSE WHO DO NOT HAVE DIAGNOSIS INFORMATION
dd <- dd[!is.na(dd$park), ]
dd$diagnosis <- "control"
dd$diagnosis[ dd$park ] <- "parkinson"
dd$log10Days <- log10(dd$days)
dd <- dd[ dd$days >= 5, ]
dd$diagnosis[ dd$diagnosis=="control" ] <- paste0("control (n=", sum(dd$diagnosis=="control"), ")")
dd$diagnosis[ dd$diagnosis=="parkinson" ] <- paste0("parkinson (n=", sum(dd$diagnosis=="parkinson"), ")")

dPlot <- ggplot(dd, aes(x=days, fill=diagnosis)) + 
  geom_histogram(aes(y=..density..*5), alpha=0.75, binwidth=5) +
  facet_wrap(~diagnosis, nrow=2) + 
  labs(x="Days on app", y="Density") + 
  guides(fill=FALSE)

fName3 <- file.path(tempdir(), "figure3-participantDays.pdf")
pdf(fName3, width = 8, height = 6)
show(dPlot)
dev.off()

#####
## STORE THE PLOTS IN SYNAPSE
#####

## GET THIS CODE STORED IN GITHUB
mpowerRepo <- getRepo('brian-bot/mPower-sdata')
thisCode <- getPermlink(mpowerRepo, 'mPower-summaries.R')

## CREATE THE PROVENANCE STEP (ACTIVITY) THAT GENERATES THESE PLOTS
act <- Activity(name="Figure Generation",
                used=lapply(as.list(q$table.id), function(x){list(entity=x)}),
                executed=list(list(url=thisCode, name=basename(thisCode))))
act <- synStore(act)

## STORE THE PLOTS IN SYNAPSE
fig2Output <- synStore(File(path=fName2, parentId="syn5480005"), activity=act)
fig3Output <- synStore(File(path=fName3, parentId="syn5480005"), activity=act)

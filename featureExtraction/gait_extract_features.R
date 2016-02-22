#####################################################################
## Extract Gait Features from mPower walking data
##
## Example to process first 10 rows from public walking table:
##   Rscript gait_extract_features.R syn5511449 1 10
## 
## Authors: Elias Chaibub Neto, J. Christopher Bare
## Sage Bionetworks (http://sagebase.org)
#####################################################################

source("gait_feature_extraction_helpers.R")

args <- commandArgs(trailingOnly=TRUE)
source_table <- args[1]
n_start <- as.integer(args[2])
n_end   <- as.integer(args[3])

require(synapseClient)
synapseLogin()

## syn5511449 = walking activity from public researcher portal
## syn4590866 = walking from mpower level 1
walk <- synTableQuery(sprintf("SELECT * FROM %s", source_table))
cat("dim(walk@values)=", dim(walk@values), "\n")

rnms <- rownames(walk@values)

alpha <- 1

filepath <- synDownloadTableFile(walk, rnms[1], "deviceMotion_walking_outbound.json.items")
ldat <- fromJSON(filepath)
gdat <- ShapeGaitData(ldat)
feat1 <- GetGaitFeatures(gdat, alpha)

feat <- matrix(NA, nrow(walk@values), length(feat1))
rownames(feat) <- walk@values$recordId
colnames(feat) <- names(feat1)
feat[1,] <- feat1

n <- nrow(walk@values)

## replace ntest by n to get all data
for (i in n_start:n_end) {
  cat(i, "\n")
  try({
    filepath <- synDownloadTableFile(walk, rnms[i], "deviceMotion_walking_outbound.json.items")
    ldat <- fromJSON(filepath)
    gdat <- ShapeGaitData(ldat)
    feat[i,] <- GetGaitFeatures(gdat, alpha)
  })
}

features <- feat[n_start:n_end,]
save(features, file=sprintf("gait_features_%d_%d.RData", n_start, n_end), compress=TRUE)


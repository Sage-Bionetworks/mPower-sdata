require(synapseClient)
require(tools)
synapseLogin()

#####
## INPUTS
##    userName: the Synapse user name for the hackathon participant (e.g. 'BrianMBot')
#####
mPowerHackathonAccess <- function(userName){
  
  ## LOOK UP THE USER AND GET THEIR PRINCIPAL ID
  findUser <- synRestGET(paste0('/userGroupHeaders?prefix=', userName))
  if( findUser$totalNumberOfResults > 1 ){
    whichOne <- sapply(findUser$children, function(x){ x$userName==userName })
    pId <- findUser$children[[which(whichOne)]]$ownerId
    # stop(paste0('There appear to be more than one profile associated with this prefix:  n = ', findUser$totalNumberOfResults))
  } else{
    pId <- findUser$children[[1]]$ownerId
  }

  ## GET ALL OF THE USER INFORMATION
  userInfo <- synRestGET(paste0('/user/', pId, '/bundle?mask=63'))
  
  ## CHECK IF THE USER IS VERIFIED AND CERTIFIED
  if( !(userInfo$isVerified & userInfo$isCertified) ){
    stop(paste0('USER IS NOT COMPLIANT:\n  isVerified = ', userInfo$isVerified, '\n  isCertified = ', userInfo$isCertified))
  }
  
  ## LIFT THE LEVEL 3 RESTRICTION
  accessRestrictionLevel3 <- "5549295"
  actApproval <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessApproval", 
                      requirementId=accessRestrictionLevel3, 
                      accessorId=pId, 
                      approvalStatus="APPROVED")
  actApproval<-synRestPOST("/accessApproval", actApproval)
  cat("HACKATHON PARTICIPANT ADDED AND USER RESTRICTION LIFTED")
  
  addThis <- data.frame(userName = userName,
                        principalId = as.integer(userInfo$userProfile$ownerId),
                        profileLink = paste0('https://www.synapse.org/#!Profile:', userInfo$userProfile$ownerId),
                        accepted = TRUE,
                        acceptedBy = synGetUserProfile()@userName,
                        acceptedDate = as.character(Sys.Date()),
                        stringsAsFactors = FALSE)
  rownames(addThis) <- NULL
  
  moreData <- Table("syn5964516", addThis)
  allData <- synStore(moreData, retrieveData=TRUE)
  
  return(cat("tracking table updated"))
}

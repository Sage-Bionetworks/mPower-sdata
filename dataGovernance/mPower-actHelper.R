require(synapseClient)
require(tools)
synapseLogin()

#####
## INPUTS
##    userName: the Synapse user name for the researcher who submitted the IDU (e.g. 'BrianMBot')
##    pathToIDU: the local path to the IDU submitted to the ACT (e.g. '~/Desktop/idu.doc')
#####
mPowerRegisterIDU <- function(userName, pathToIDU){
  
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
  
  ## GET THE REGISTRATION TABLE
  td <- synTableQuery('SELECT * FROM syn5586694')
  
  ## CHECK TO SEE IF THIS USER ALREADY HAS AN IDU REGISTERED
  if( any(td@values$principalId == userInfo$userId) ){
    message(paste0('User with principalId ', userInfo$userId, ' already has an IDU registered'))
  }
  
  ## CHECK IF THE USER IS VERIFIED AND CERTIFIED
  if( !(userInfo$isVerified & userInfo$isCertified) ){
    cat(paste0('USER IS NOT YET COMPLIANT:\n  isVerified = ', userInfo$isVerified, '\n  isCertified = ', userInfo$isCertified, '\nLOGGING THE IDU ANYWAY'))
  }
  
  ## MOVE THE IDU TO TEMP DIRECTORY AND RENAME
  newPathToIDU <- file.path(tempdir(), paste0(userName, "-IDU.", file_ext(pathToIDU)))
  if(!file.copy(pathToIDU, newPathToIDU, overwrite = TRUE)){
    stop("could not copy file")
  }
  
  ## STORE THE IDU AS A FILEHANDLE
  iduFh <- synapseClient:::uploadAndAddToCacheMap(newPathToIDU, synapseClient:::getUploadDestinations(synGet(td@schema)$properties$parentId)[[1]])
  
  addThis <- data.frame(userName = userName,
                        principalId = as.integer(userInfo$userProfile$ownerId),
                        profileLink = paste0('https://www.synapse.org/#!Profile:', userInfo$userProfile$ownerId),
                        registeredBy = synGetUserProfile()@userName,
                        registeredDate = as.character(Sys.Date()),
                        idu = iduFh$id,
                        iduId = as.character(iduFh$id),
                        accepted = as.logical(NA),
                        acceptedBy = as.character(NA),
                        acceptedDate = as.character(NA),
                        stringsAsFactors = FALSE)
  rownames(addThis) <- NULL
  
  moreData <- Table(td@schema, addThis)
  allData <- synStore(moreData, retrieveData=TRUE)
  return(onWeb(td@schema))
}


#####
## INPUTS
##    principalId: the principal ID for the Synapse user (e.g. '273979')
##    iduId: the iduId from the registered IDU for acceptance (e.g. '1234567')
#####
mPowerAcceptIDU <- function(principalId, iduId){
  
  ## GET THE REGISTRATION TABLE
  td <- synTableQuery(paste0("SELECT * FROM syn5586694 WHERE principalId='", principalId, "' AND iduId='", iduId, "'"))
  res <- td@values
  
  ## CHECK THE TABLE
  if(nrow(res) == 0){
    stop('There are no IDUs for user / iduId combination')
  }
  if(nrow(res) > 1){
    stop('Somehow there is more than 1 IDU for user / iduId combination')
  }
  if(!is.na(res$accepted)){
    stop(paste0('IDU status already set for user: ', principalId, '\n    accepted = ', res$accepted))
  }
  
  ## GET ALL OF THE USER INFORMATION
  userInfo <- synRestGET(paste0('/user/', principalId, '/bundle?mask=63'))
  
  ## CHECK IF THE USER IS VERIFIED AND CERTIFIED
  if( !(userInfo$isVerified & userInfo$isCertified) ){
    stop(paste0('USER IS NOT COMPLIANT:\n  isVerified = ', userInfo$isVerified, '\n  isCertified = ', userInfo$isCertified))
  }
  
  ## LIFT THE LEVEL 3 RESTRICTION
  accessRestrictionLevel3 <- "5549295"
  actApproval <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessApproval", 
                      requirementId=accessRestrictionLevel3, 
                      accessorId=principalId, 
                      approvalStatus="APPROVED")
  actApproval<-synRestPOST("/accessApproval", actApproval)
  
  cat("IDU ACCEPTED AND USER RESTRICTION LIFTED")
  res$accepted <- TRUE
  res$acceptedBy <- synGetUserProfile()@userName
  res$acceptedDate = as.character(Sys.Date())
  td@values <- res
  td <- synStore(td, retrieveData=TRUE)
  cat("tracking table updated")
  
  return(onWeb(td@schema))
}

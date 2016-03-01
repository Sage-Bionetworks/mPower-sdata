require(synapseClient)
require(RCurl)
synapseLogin()

td <- synTableQuery('SELECT * FROM syn5586694')
theseOnes <- td@values
theseOnes <- theseOnes[ which(theseOnes$accepted), ]
theseOnes <- theseOnes[ order(theseOnes$acceptedDate), ]

urlEncodings <- c('{' = "%7B",
                  '}' = "%7D",
                  '-' = "%2D",
                  '_' = "%5F",
                  '.' = "%2E",
                  '!' = "%21",
                  '~' = "%7E",
                  '*' = "%2A",
                  '`' = "%60",
                  '\'' = "%27",
                  '(' = "%28",
                  ')' = "%29",
                  '[' = "%5B",
                  ']' = "%5D",
                  ':' = "%3A",
                  ';' = "%3B",
                  '\n' = "%0A",
                  '\r' = "%0D",
                  '/' = "%2F",
                  '?' = "%3F",
                  '&' = "%26",
                  '=' = "%3D",
                  '+' = "%2B",
                  ',' = "%2C",
                  '#' = "%23",
                  '$' = "%24")

baseWiki <- "-----\n### Seeding a community in Parkinson research\nIt is the hope of Sage Bionetworks that by making the mPower data available to qualified researchers worldwide, that we can seed a community who will work together and share insights into Parkinson symptoms and modulators. Synapse Certified Users with verified profiles may request access to the mPower data set for research to benefit human health. Below are all of the data access requests that have been approved for the mPower data set. Take a look at what others are proposing to do, and reach out to any who you may be interested in working with!\n-----\n"
fullWiki <- baseWiki
for(i in 1:nrow(theseOnes)){
  userInfo <- synRestGET(paste0('/user/', theseOnes$principalId[i], '/bundle?mask=63'))
  
  iduLoc <- synDownloadTableFile(td, rownames(theseOnes)[i], "idu")
  iduText <- paste(readLines(iduLoc), collapse="\n")
  newEntry <- paste0("##### **Researcher**: ", userInfo$verificationSubmission$firstName, " ", userInfo$verificationSubmission$lastName, " ([profile](", theseOnes$profileLink[i], "))\n",
                     "##### **ORCID**: ", userInfo$verificationSubmission$orcid, "\n",
                     "##### **Affiliation**: ", userInfo$verificationSubmission$company, "\n",
                     "##### **Intended Data Use Statement (accepted on ", theseOnes$acceptedDate[i], "):**\n", iduText, "\n-----\n")
  fullWiki <- paste0(fullWiki, newEntry)
}

thisWiki <- synGetWiki(synGet("syn4993293"), "392026")
thisWiki@properties$markdown <- fullWiki
thisWiki <- synStore(thisWiki)

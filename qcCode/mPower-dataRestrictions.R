###########################################
##  mPower access restrictions
###########################################
require(synapseClient)
synapseLogin()

###########################################
# Set up the controlled access -tier 3
############################################
entityIds3 <- list("syn5511429","syn5511432","syn5511433","syn5511434","syn5511439","syn5511444","syn5511449")
subjectIds3 <- lapply(entityIds3, function(x){list(id=x,type="ENTITY")})
openJiraIssue3 <- FALSE # set to true or false to control whether the web portal allows to open a Jira issue
actContactInfo3 <- c("<div style=\"font-size:14px\" class=\"markdown margin-left-10 margin-right-15\"><p><b> To qualify for access to mPower data, you must:</b></p> 
                     <ul style=\"margin-top:15px\">
                     <li> Be a <a href=\"https://www.synapse.org/#!Synapse:syn2305384/wiki/61140\" target=\"_blank\">Synapse Certified User</a> with an enhanced and validated user profile.</li>
                     <li> Submit your intended Data Use statement to act@synapse.org (1-3 paragraphs) from the email address linked to your validated Synapse account</li>
                     <li> Agree to comply with the data-specific Terms of Use when prompted</li></ul>
                     <br/>
                     <p>See the full instructions for requesting data access on the <a href=\"https://www.synapse.org/#!Synapse:syn4993293/wiki/247860\">Accessing Data</a> page.</p>")

ar3 <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessRequirement", 
            subjectIds=subjectIds3, 
            accessType="DOWNLOAD", 
            actContactInfo=actContactInfo3, 
            openJiraIssue=openJiraIssue3)
ar3 <- synRestPOST("/accessRequirement", ar3)
ar3


#######################################
#Add Restricted (Tier 2) Access Control
########################################

entityIds2 <- list("syn5511429","syn5511432","syn5511433","syn5511434","syn5511439","syn5511444","syn5511449")
subjectIds2 <- lapply(entityIds2, function(x){list(id=x,type="ENTITY")})
ar2 <- list(entityType="org.sagebionetworks.repo.model.TermsOfUseAccessRequirement", concreteType="org.sagebionetworks.repo.model.TermsOfUseAccessRequirement",
           subjectIds=subjectIds2, accessType="DOWNLOAD",
           termsOfUse="<div style=\"font-size:14px\" class=\"markdown margin-left-10 margin-right-15\"> 
           <p><b> To qualify for access to mPower data, you must:</b></p> 
           <ul style=\"margin-top:15px\">
           <li> You confirm that you will not attempt to re-identify research participants for any reason, including for re-identification theory research.</li>
           <li> You reaffirm your commitment to the Synapse Awareness and Ethics Pledge.</li>
           <li> You agree to abide by the guiding principles for responsible research use and data handling as described in the <a href=\"https://www.synapse.org/#!Help:Governance\" target=\"_blank\">Synapse Governance documents</a>.
           <li> You commit to keeping these data confidential and secure.
           <li> You agree to use these data exclusively as described in your submitted Intended Data Use statement.
           <li> You understand that these data may not be used for commercial advertisement or to re-contact research participants
           <li> You agree to report any misuse or data release, intentional or inadvertent to the ACT within 5 business days by emailing act@sagebase.org
           <li> You agree to publish findings in open access publications.
           <li> You promise to acknowledge the research participants as data contributors and mPower study investigators on all publication or presentation resulting from using these data as follows: 
           <i><b>These data were contributed by users of the Parkinson mPower mobile application as part of the mPower study developed by Sage Bionetworks and described in Synapse [doi:10.7303/syn4993293].</b></i></li>
           ")

###Post access requirement on Synapse"         
ar2 <- synRestPOST("/accessRequirement", ar2)

#Return the access restriction ID
ar2

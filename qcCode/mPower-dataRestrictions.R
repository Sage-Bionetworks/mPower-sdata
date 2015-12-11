###########################################
##  mPower access restrictions
###########################################
require(synapseClient)
synapseLogin()


#######################################################################
## Set up the controlled access Tier 3 for MDS-UPDRS survey data table
#######################################################################
updrsId <- list(list(id="syn5511432", type="ENTITY"))
openJiraIssueUPDRS <- FALSE # set to true or false to control whether the web portal allows to open a Jira issue
actTextUPDRS <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15">
<h4>To qualify for access to the mPower UPDRS survey data:</h4>
<ul>
  <li>If you have not already done so:
    <ul>
      <li>Become a Certified User with a validate profile
      <li>Submit a 1-3 paragraph Intended Data Use statement to <b>act@synapse.org</b> from the email address you used to validate your Synapse profile. <b><i>Note that your Intended Data Use statement will be posted publically on Synapse</i></b>
      <li>Agree to comply with the data-specific Conditions for Use when prompted
    </ul>
  <li><b>Additionally</b>, you must receive written approval for use of these data from the International Parkinson and Movement Disorder Society (IPDMS). IPDMS will determine in its sole discretion whether the requested use is for commercial or non-commercial purposes. If the use is determined by IPDMS to be for commercial purposes, you will be required to pay an appropriate license fee for sue use in an amount determined by IPMDS. All usage requests should be submitted from the email address you used to validate your Synapse profile to: <b>mdsupdrs@synapse.org</b>
</ul>
<br>
<p>See the full instructions for requesting data access on the <a href="https://www.synapse.org/#!Synapse:syn4993293/wiki/247860">Accessing Data</a> page.</p>')
arUPDRS <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessRequirement", 
                subjectIds=updrsId, 
                accessType="DOWNLOAD", 
                actContactInfo=actTextUPDRS, 
                openJiraIssue=openJiraIssueUPDRS)
arUPDRS <- synRestPOST("/accessRequirement", arUPDRS)


#######################################################################
## Set up the controlled access Tier 3 for PDQ8 survey data table
#######################################################################
pdq8Id <- list(list(id="syn5511433", type="ENTITY"))
openJiraIssuePDQ8 <- FALSE
actTextPDQ8 <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15">
<h4>To qualify for access to the mPower PDQ-8 survey data:</h4>
<ul>
  <li>If you have not already done so:
    <ul>
      <li>Become a Certified User with a validate profile
      <li>Submit a 1-3 paragraph Intended Data Use statement to <b>act@synapse.org</b> from the email address you used to validate your Synapse profile. <b><i>Note that your Intended Data Use statement will be posted publically on Synapse</i></b>
      <li>Agree to comply with the data-specific Conditions for Use when prompted
    </ul>
  <li><b>Additionally</b>, you must TBD
</ul>
<br>
<p>See the full instructions for requesting data access on the <a href="https://www.synapse.org/#!Synapse:syn4993293/wiki/247860">Accessing Data</a> page.</p>')
arPDQ8 <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessRequirement", 
               subjectIds=pdq8Id, 
               accessType="DOWNLOAD", 
               actContactInfo=actTextPDQ8, 
               openJiraIssue=openJiraIssuePDQ8)
arPDQ8 <- synRestPOST("/accessRequirement", arPDQ8)



#######################################################################
## Set up the controlled access - tier 3 - all mPower datasets
#######################################################################
entityIds3 <- list("syn5511429","syn5511432","syn5511433","syn5511434","syn5511439","syn5511444","syn5511449")
subjectIds3 <- lapply(entityIds3, function(x){list(id=x,type="ENTITY")})
openJiraIssue3 <- FALSE # set to true or false to control whether the web portal allows to open a Jira issue
actText3 <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15"> 
<h4>To qualify for access to mPower data, you must:</h4>
<ul>
  <li> Become a <a href="https://www.synapse.org/#!Synapse:syn2305384/wiki/61140" target="_blank">Synapse Certified User</a> with a validated user profile
  <li> Submit a 1-3 paragraph Intended Data Use statement to the <b>act@synapse.org</b> from the email address you used to validate your Synapse profile. <b><i>Note that your Intended Data Use statement will be posted publically on Synapse
  <li> Agree to comply with the data-specific Conditions for Use when prompted
</ul>
<br>
<p>See the full instructions for requesting data access on the <a href="https://www.synapse.org/#!Synapse:syn4993293/wiki/247860">Accessing Data</a> page.</p>')
ar3 <- list(concreteType="org.sagebionetworks.repo.model.ACTAccessRequirement", 
            subjectIds=subjectIds3, 
            accessType="DOWNLOAD", 
            actContactInfo=actText3, 
            openJiraIssue=openJiraIssue3)
ar3 <- synRestPOST("/accessRequirement", ar3)


#######################################################################
## Add Restricted (Tier 2) Access Control to all mPower data tables
#######################################################################
entityIds2 <- list("syn5511429","syn5511432","syn5511433","syn5511434","syn5511439","syn5511444","syn5511449")
subjectIds2 <- lapply(entityIds2, function(x){list(id=x,type="ENTITY")})
actText2 <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15">
<h4>To qualify for access to mPower data:</h4>
<ul>
  <li> You confirm that you will not attempt to re-identify research participants for any reason, including for re-identification theory research
  <li> You reaffirm your commitment to the Synapse Awareness and Ethics Pledge
  <li> You agree to abide by the guiding principles for responsible research use and data handling as described in the <a href="https://www.synapse.org/#!Synapse:syn2502577/wiki" target="_blank">Synapse Governance documents</a>
  <li> You commit to keeping these data confidential and secure
  <li> You agree to use these data exclusively as described in your submitted Intended Data Use statement
  <li> You understand that these data may not be used for commercial advertisement or to re-contact research participants
  <li> You agree to report any misuse or data release, intentional or inadvertent to the ACT within 5 business days by emailing <b>act@sagebase.org</b>
  <li> You agree to publish findings in open access publications
  <li> You promise to acknowledge the research participants as data contributors and mPower study investigators on all publication or presentation resulting from using these data as follows: 
  <i><b>These data were contributed by users of the Parkinson mPower mobile application as part of the mPower study developed by Sage Bionetworks and described in Synapse [doi:10.7303/syn4993293].</b></i>
</ul>')
ar2 <- list(entityType="org.sagebionetworks.repo.model.TermsOfUseAccessRequirement", concreteType="org.sagebionetworks.repo.model.TermsOfUseAccessRequirement",
            subjectIds=subjectIds2, accessType="DOWNLOAD",
            termsOfUse=actText2)
ar2 <- synRestPOST("/accessRequirement", ar2)

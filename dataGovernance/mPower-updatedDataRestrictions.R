###########################################
##  mPower access restriction updates
###########################################
require(synapseClient)
synapseLogin()

## UPDATE UPDRS ACCESS RESTRICTION BEFORE LAUNCH
arUPDRS <- synRestGET('/accessRequirement/5549293')
arUPDRS$actContactInfo <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15">
<h4>To qualify for access to the mPower UPDRS survey data, you must:</h4>
<ul>
  <li>Become a <a href="https://www.synapse.org/#!Synapse:syn2305384/wiki/61140">Certified User</a> with a validate profile
  <li>Receive written approval for use of these data from the International Parkinson and Movement Disorder Society (IPMDS)
  <li>Following approval from IPDMS, submit your 1-3 paragraph Intended Data Use statement to the act@synapse.org from the email address you used to validate your Synapse profile. <b>Note that your Intended Data Use statement will be posted publically on Synapse</b>
  <li>Agree to comply with the data-specific Conditions for Use when prompted
</ul>
<br>
<p>See the full instructions for requesting data access on the <a href="https://www.synapse.org/#!Synapse:syn4993293/wiki/247860">Accessing the mPower data</a> page.</p>')
arUPDRS <- synRestPUT(paste0('/accessRequirement/', arUPDRS$id), arUPDRS)

## UPDATE PDQ8 ACCESS RESTRICTION BEFORE LAUNCH
arPDQ8 <- synRestGET('/accessRequirement/5549294')
arPDQ8$actContactInfo <- c('<div style="font-size:14px" class="markdown margin-left-10 margin-right-15">
<h4>To qualify for access to the mPower PDQ-8 survey data, you must:</h4>
<ul>
  <li>Become a <a href="https://www.synapse.org/#!Synapse:syn2305384/wiki/61140">Certified User</a> with a validate profile
  <li>Receive written approval for use of these data
  <li>Following approval, submit your 1-3 paragraph Intended Data Use statement to the act@synapse.org from the email address you used to validate your Synapse profile. <b>Note that your Intended Data Use statement will be posted publically on Synapse</b>
  <li>Agree to comply with the data-specific Conditions for Use when prompted
</ul>
<br>
<p>See the full instructions for requesting data access on the <a href="https://www.synapse.org/#!Synapse:syn4993293/wiki/247860">Accessing the mPower data</a> page.</p>')
arPDQ8 <- synRestPUT(paste0('/accessRequirement/', arPDQ8$id), arPDQ8)

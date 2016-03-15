## mPower mobile study of Parkinson Disease (PD)
In March 2015, Sage Bionetworks in partnership with the University of Rochester launched an observational smartphone-based study developed using Apple's ResearchKit library to evaluate the feasibility of remotely collecting frequent information about the daily changes in symptom severity and their sensitivity to medication in PD. These data provide the ability to explore classification of control participants and those who self-report having PD, as well as to begin to measure the severity of PD for those with the disease. There are myriad additional questions from each of the varying streams of data that will require a community of researchers to explore fully.

-----
### mPower Public Researcher Portal
The [mPower Public Researcher Portal](https://www.synapse.org/mpower) Synapse Project is the home for all documentation about the mPower study and instructions for the use of the coded data consisting of survey responses and mobile sensor measurements from active tasks.

[Synapse](https://www.synapse.org) is a general-purpose data and analysis sharing service where members can work collaboratively, analyze data, share insights and have attributions and provenance of those insights to share with others. Synapse is developed and operated by [Sage Bionetworks](http://sagebase.org/) as a service to the health research community.

-----
### mPower Data Governance
Due to the novel nature and collection method for these data, governance structures have been put in place in order to respect the balance between the desire of participants to share their data with qualified researchers and the respect for privacy of those participants.

Researchers who are interested in accessing these data need to complete the following steps:
  1. Have a Synapse account (https://www.synapse.org)
  2. Have their Synapse User Profile validated by the Synapse Access and Compliance Team (ACT)
  3. Become a Synapse Certified User by completing a short quiz (https://www.synapse.org/#!Quiz:Certification)
  4. Submit an Intended Data Use statement which will be **posted publicly**
  5. Agree to the Conditions for Use associated with each data source (see DOIs for each data source) - some data may have additional conditions due to survey licensing

Full instructions on data access are available on the [Synapse Project Wiki](https://www.synapse.org/#!Synapse:syn4993293/wiki/247860).

-----
### Example R code
Once the governance restrictions have been fulfulled, researchers can access the data through the [mPower Public Researcher Portal](https://www.synapse.org/mpower) or programmatically through the Synapse analytical clients (R, Python, command line). Example R code for interacting with these data are available [here](examples/mPower-bootstrap.R).

-----
### Nature Scientific Data reproducibility
Code used for reproducing the summary statistics and figures from the Nature Scientific Data Descriptor paper are available [here](mPower-summaries.R).

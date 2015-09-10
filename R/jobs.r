# set the working directory
setwd('C:/Users/ELise Rubat-Ciagnus/Documents/Angleterre/job-market-analytics/R')
# setwd("C:/Users/andres2/Dropbox/Research/2015/job-market-analytics/R")




library("stringr")
library("ggplot2")
#library("psych")
#library(arules)
#library("xlsx")
#library("WriteXLS")
library(scatterplot3d)
library(FactoMineR)
library(pca3d)
library(rgl)

# I put as comment the line that we don't working on for the moment.

options(scipen=5)
palette(c(colors()[30], colors()[84], colors()[51], colors()[76], colors()[554], colors()[161], colors()[301], colors()[567], colors()[47], colors()[128], colors()[53], colors()[116]))
source(file="jobMarketAnalysis.r")




 securityCities <- getCities(securityJobs)
# stop()
# the next 2 functions are hardcoded - not much we can do
# import the jobs
 jobs <- importJobs("C:/Users/Antoine/Desktop/CodePourACP/data/jobs.csv")

# set the keywords that will be used

 stop()

securityKeywords <- c("CISA", "CISM", "CCSP","CISSP","Security","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")
#securityNewKeywords <- c("CISA", "CISM", "CCSP","CISSP","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")
securityJobs <- printReport(securityKeywords, "Security jobs", jobs)

securityRules <- getAssociateRules(securityKeywords, securityJobs, "Yes")
#securityNewRules <- getAssociateRules(securityNewKeywords, securityJobs, "Yes")

# securityAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", securityJobs) 





linuxKeywords = c("Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "Linux", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")
#linuxNewKeywords = c("Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")
linuxJobs <- printReport(linuxKeywords, "Linux jobs", jobs)

linuxRules <- getAssociateRules(linuxKeywords, linuxJobs, "Yes")
#linuxNewRules <- getAssociateRules(linuxNewKeywords, linuxJobs, "Yes")

# linuxAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", linuxJobs)





datascienceKeywords = c("SAS", "SPSS", "Stata", "Matlab", "Tableau", "data science", "Qlikview", "Base SAS", "R programming")
datascienceJobs <- printReport(datascienceKeywords, "Data Science jobs", jobs)

datascienceRules <- getAssociateRules(datascienceKeywords, datascienceJobs, "Yes")

# datascienceAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", datascienceJobs)





dbKeywords = c("MySQL", "Oracle", "SQL Server", "SQLlight", "Postgresql", "db2", "Sybase")
dbJobs <- printReport(dbKeywords, "DB jobs", jobs)

dbRules <- getAssociateRules(dbKeywords, dbJobs, "Yes")

# dbAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", dbJobs)



#get PCA and clustering 




securityKeywords <- c("Security","CISA", "CISM", "CCSP","CISSP","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")
tmpTF <- getFrequencyMatrix(securityKeywords, securityJobs, "T")
securityPCA <- getPCA(securityKeywords, securityJobs, tmpTF, 2)

securityCluster <- getClustering(securityKeywords, securityJobs, tmpTF)



linuxKeywords = c("Linux","Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")
tmpTF <- getFrequencyMatrix(linuxKeywords, linuxJobs, "T")

for(i in 1:length(linuxKeywords)){
  PCA <- getPCA(linuxKeywords, linuxJobs, tmpTF, i)
}

linuxCluster <- getClustering(linuxKeywords, linuxJobs, tmpTF)



datascienceKeywords = c("SAS", "SPSS", "Stata", "Matlab", "Tableau", "data science", "Qlikview", "Base SAS", "R programming")
tmpTF <- getFrequencyMatrix(datascienceKeywords, datascienceJobs, "T")

for(i in 1:length(datascienceKeywords)){
  PCA <- getPCA(datascienceKeywords, datascienceJobs, tmpTF, i)
}

datascienceCluster <- getClustering(datascienceKeywords, datascienceJobs, tmpTF)



dbKeywords = c("MySQL", "Oracle", "SQL Server", "SQLlight", "Postgresql", "db2", "Sybase")
tmpTF <- getFrequencyMatrix(dbKeywords, dbJobs, "T")

for(i in 1:length(dbKeywords)){
  PCA <- getPCA(dbKeywords, dbJobs, tmpTF, i)
}

dbCluster <- getClustering(dbKeywords, dbJobs, tmpTF)




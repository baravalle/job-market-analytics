# set the working directory
setwd('C:/Users/ELise Rubat-Ciagnus/Documents/Angleterre/job-market-analytics/R')
# setwd("C:/Users/andres2/Dropbox/Research/2015/job-market-analytics/R")

library("stringr")
#library("ggplot2")
library("psych")
library(arules)
library("xlsx")


# I put as comment the line that we don't working on for the moment.

options(scipen=5)
source(file="jobMarketAnalysis.r")

# the next 2 functions are hardcoded - not much we can do
# import the jobs
#jobs <- importJobs("../data/jobs.csv")

#stop()

# set the keywords that will be used


securityKeywords <- c("CISA", "CISM", "CCSP","CISSP","Security","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")
#securityNewKeywords <- c("CISA", "CISM", "CCSP","CISSP","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")
#securityJobs <- printReport(securityKeywords, "Security jobs", jobs)

#securityRules <- getAssociateRules(securityKeywords, securityJobs, "Yes")
#securityNewRules <- getAssociateRules(securityNewKeywords, securityJobs, "Yes")

securityCluster <- getClustering(securityKeywords, securityJobs)
#securityJobs2 <- cbind(securityJobs, securityCluster$cluster)
#names(securityJobs2) <- c(names(securityJobs), "Cluster")

#securityAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", securityJobs) 
stop()




linuxKeywords = c("Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "Linux", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")
#linuxNewKeywords = c("Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")
#linuxJobs <- printReport(linuxKeywords, "Linux jobs", jobs)

#linuxRules <- getAssociateRules(linuxKeywords, linuxJobs, "Yes")
#linuxNewRules <- getAssociateRules(linuxNewKeywords, linuxJobs, "Yes")

linuxCluster <- getClustering(linuxKeywords, linuxJobs)
#linuxJobs2 <- cbind(linuxJobs, linuxCluster$cluster)
#names(linuxJobs2) <- c(names(linuxJobs), "Cluster")


#linuxAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", linuxJobs)




datascienceKeywords = c("SAS", "SPSS", "Stata", "Mathlab", "Tableau", "data science", "Qlikview", "Base SAS", "R programming")
#datascienceJobs <- printReport(datascienceKeywords, "Data Science jobs", jobs)

#datascienceRules <- getAssociateRules(datascienceKeywords, datascienceJobs, "Yes")

datascienceCluster <- getClustering(datascienceKeywords, datascienceJobs)
#datascienceJobs2 <- cbind(datascienceJobs, datascienceCluster$cluster)
#names(datascienceJobs2) <- c(names(datascienceJobs), "Cluster")


#datascienceAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", datascienceJobs)




dbKeywords = c("MySQL", "Oracle", "SQL Server", "SQLlight", "Postgresql", "db2", "Sybase")
#dbJobs <- printReport(dbKeywords, "DB jobs", jobs)

#dbRules <- getAssociateRules(dbKeywords, dbJobs, "Yes")

dbCluster <- getClustering(dbKeywords, dbJobs)
#dbJobs2 <- cbind(dbJobs, dbCluster$cluster)
#names(dbJobs2) <- c(names(dbJobs), "Cluster")


#dbAllKoccur<- getAllKeywordOccurencies("../data/Keywords.xlsx", dbJobs)



# still need something to show mean salary by function


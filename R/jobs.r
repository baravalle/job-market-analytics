# set the working directory
setwd("C:/Users/ELise Rubat-Ciagnus/Documents/Anleterre/job-market-analytics/R")
# setwd("C:/Users/andres2/Dropbox/Research/2015/job-market-analytics/R")

library("stringr")
#library("ggplot2")
library("psych")

options(scipen=5)
source(file="jobMarketAnalysis.r")

# the next 2 functions are hardcoded - not much we can do
# import the jobs
jobs <- importJobs("../data/jobs.csv")

#stop()

# set the keywords that will be used
securityKeywords <- c("CISA", "CISM", "CCSP","CISSP","Security","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")

# securityJobs <- getJobs(securityKeywords)
securityJobs <- printReport(securityKeywords, "Security jobs", jobs)

linuxKeywords = c("Centos", "Fedora", "Ubuntu", "Debian", "Slackware", "Linux", "RHCE", "RHCA", "RHCSA", "Comptia Linux+")

# get the Linux jobs
linuxJobs <- printReport(linuxKeywords, "Linux jobs", jobs)

datascienceKeywords = c("SAS", "SPSS", "Stata", "Mathlab", "Tableau", "data science", "Qlikview", "Base SAS", "R programming")

datascienceJobs <- printReport(datascienceKeywords, "Data Science jobs", jobs)

dbKeywords = c("MySQL", "Oracle", "SQL Server", "SQLlight", "Postgresql", "db2", "Sybase")

dbJobs <- printReport(dbKeywords, "DB jobs", jobs)
#the parameters of last line was wrong now its correct
# still need something to show mean salary by function

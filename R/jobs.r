# set the working directory
setwd("C:/Users/andres/Dropbox/Research/2015/job-market-analytics/R")
# setwd("C:/Users/andres2/Dropbox/Research/2015/job-market-analytics/R")

library(stringr)
source(file="jobMarketAnalysis.r")

# the next 2 functions are hardcoded - not much we can do
# import the jobs
jobs <- importJobs("../data/jobs.csv")

stop()

# exportJobs(jobs,"jobsClean.csv")

# set the keywords that will be used
securityKeywords <- c("CISA", "CISM", "CCSP","CISSP","Security","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")

# get the security jobs
securityJobs <- getSecurityJobs(securityKeywords)

# let's free some memory now
#rm(jobs)

# get the data
keywordsData <- getKeywordsData(securityKeywords, securityJobs)

keywordsDataPlot <- data.frame(keywordsData$Keyword, keywordsData$Jobs)
names(keywordsDataPlot) <- c("Skill", "Jobs")

# let's remove the generic keyword "security" - too many hits
keywordsDataPlot <- keywordsDataPlot[!keywordsDataPlot$Skill == "Security", ]

# let's remove any keywords with no jobs
keywordsDataPlot <- keywordsDataPlot[!keywordsDataPlot$Jobs == 0, ]

# plotting the keywords
barplot(keywordsDataPlot$Jobs,names.arg=keywordsDataPlot$Skill)

# getting occurencies on jobs
jobKeywords <- as.data.frame(getKeywordOccurencies(securityKeywords, securityJobs))

# cross tabulation
jobKeywordsTable <- table(jobKeywords[["JobId"]], jobKeywords[["Skill"]])

# correlation table
cor(jobKeywordsTable)

#salary <- str_extract(securityJobs$salary, "[^A-Za-z|&]{4,}")
#salary <- gsub("^\\s+|\\s+$", "", salary)
#salary


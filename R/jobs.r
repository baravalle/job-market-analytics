# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

#' Import jobs
#'
#' Imports a unclean jobs CVS dataset.
#' @param in.file Path for the CSV file
#' @keywords import
#' @export
#' @examples
#' importJob()
importJobs<-function(in.file) {
  jobs<-read.csv(in.file, header=FALSE)

  if(!length(jobs)>1) {
    stop()
  }
  else {
    # rename the columns
    names(jobs) <- c("id","title","location","title2", "location2", "company", "industry", "type", "career level", "url", "salary", "description")

    # date is missing!
    # will get it later on

    # remove columns not needed
    jobs <- subset(jobs, select=c("id","title", "title2", "location","location2", "company", "industry", "career level", "url", "salary", "description"))

    # now clean the salary!
	salary2 <- str_extract(jobs$salary, "[^A-Za-z|&]{4,}")
	salary2 <- gsub("^\\s+|\\s+$|Â|\\?|\\/|[\\]|\\n|\\+|\\.00|,|\"", "", salary2)

	#more cleaning TODO
	salaryClean <- vector("numeric", length(salary2))

	for(i in 1:length(salary2)){

		if (!( is.na(salary2[i]) | salary2[i]=="")){

			tab <- read.table(text=salary2[i], sep="-")

			if(length(tab) == 2){

				v1 <- tab$V1
				v2 <- tab$V2
				v1 <- as.numeric(v1)
				v2 <- as.numeric(v2)
				salaryClean[i] <- (v1+v2)/2

			}
			else{
				salaryClean[i] <- as.numeric(tab$V1)
			}
		}
		else{
			salaryClean[i] <- "NA"
		}
	}
	jobs <- cbind(jobs, salaryClean)
  }
}


exportJobs<-function(jobs, out.file) {
  # and export the data to CSV
  write.csv2(jobs, file=out.file)
}

getSecurityJobs<-function(securityKeywords) {
  # join title & description for next operations
  # we need this only for the security jobs
  jobs$"title and description" <- do.call(paste, c(jobs[c("title", "description")], sep = " "))

  #convert keywords into regexp
  pattern = paste(securityKeywords, collapse="|", sep="")
  pattern = paste("/",pattern, "/", collapse="", sep="")

  # identify security jobs by id
  securityJobsRows <- grep(pattern, jobs$"title and description", ignore.case=T)

  securityJobs <- jobs[securityJobsRows,]
}

getKeywordsData <- function (securityKeywords, securityJobs) {

  keyword <- vector("character");
  njobs <- integer();
  locations <- vector();
  salary <- vector();

  for (i in securityKeywords) {
    # extracting the keywords
    keywords_rows <- grep(i, securityJobs$"title and description", ignore.case=T)

    keyword <- c(keyword, i)
    njobs <- c(njobs, length(keywords_rows))

    # and now find the location
    # and store it in a proper data structure
    locations <- c(locations, "")

    # now find the salary!
    # and store it in a proper data structure
    salary <- c(salary, 0)

  }

  keywords <- data.frame("Keyword"=keyword, "Jobs"=njobs, "Locations"=locations, "Salary"=salary)

}

getKeywordOccurencies <- function(securityKeywords, securityJobs) {

  jobKeywords <- vector()

  for (i in securityKeywords) {
    keywords_rows <- grep(i, securityJobs$"title and description", ignore.case=T)

    jobKeywords2 <- cbind(keywords_rows,i)

    if(length(keywords_rows)> 0) {
      jobKeywords <- rbind(jobKeywords, jobKeywords2)
    }
  }

  colnames(jobKeywords) <- c("JobId", "Skill")
  jobKeywords

}


library(stringr)

# set the working directory
# setwd("C:/Users/andres/Dropbox/Research/2015/Jobs 2015/")
setwd("C:/Users/andres2/Dropbox/Research/2015/Jobs 2015/")

# the next 2 functions are hardcoded - not much we can do
# import the jobs
jobs <- importJobs("jobs.csv")
exportJobs(jobs,"jobsClean.csv")

# set the keywords that will be used
securityKeywords <- c("CISA", "CISM", "CCSP","CISSP","Security","QSA","Sabanes-Oxley","Penetration Testing","ISO 27001","IISP")

# get the security jobs
securityJobs <- getSecurityJobs(securityKeywords)

# let's free some memory now
# rm(jobs)

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

salary <- str_extract(securityJobs$salary, "[^A-Za-z|&]{4,}")
salary <- gsub("^\\s+|\\s+$", "", salary)
salary


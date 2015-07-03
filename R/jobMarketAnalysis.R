#' Job market analysis
#'
#' Job market analysis is used to clean and analyse job datasets.
#'
#' @name job-market-analysis
#' @docType package
#' @import stringr
NULL

jobMarketAnalysis <- function () {



}


#' Import jobs
#'
#' Imports a unclean jobs CVS dataset.
#' @param in.file Path for the CSV file
#' @keywords import
#' @return
#' @export
#' @examples
#' importJob()
importJobs <- function(in.file) {
  # rewrite using more parameters, to make it more generic - e.g. pass column titles.

  # something is wrong here - the import didn't import correctly many lines
  # check around line 2500
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

    # more cleaning TODO

    salaryClean <- sapply(strsplit(salary2, "\ *-\ *"), function(x) {x <- as.numeric(x); mean(x) })

    jobs <- cbind(jobs, salaryClean)
  }
}


#' exportJobs
#'
#' @param jobs Jobs dataset variable
#' @param out.file Destination file
#' @return
#' @export
#' @examples
exportJobs <- function(jobs, out.file) {
  # and export the data to CSV
  write.csv2(jobs, file=out.file)
}

getSecurityJobs <- function(securityKeywords) {
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
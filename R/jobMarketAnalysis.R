#' Job market analysis
#'
#' Job market analysis is used to clean and analyse job datasets.
#'
#' @name job-market-analysis
#' @docType package
#' @import stringr
NULL


#' Import jobs
#'
#' Imports a unclean jobs CVS dataset.
#' @param in.file Path for the CSV file
#' @keywords import
#' @return
#' @export
#' @examples
#' importJob()
#' 

jobMarketAnalysis <- function(){
  
}

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

    # there are some errors in the import
    # and jobs are spread over several rows. We need to 1) recheck the import 2) in the meanwhile, filter some of those jobs
    # currently this affects about 2/3 of the dataset import
    subset(jobs, !is.na(as.numeric(as.character(id))))
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

getJobs <- function(keywords, jobs) {
  # join title & description for next operations
  # we need this only for the security jobs
  jobs$"title and description" <- do.call(paste, c(jobs[c("title", "description")], sep = " "))

  #convert keywords into regexp
  pattern = paste(keywords, collapse="|", sep="")
  pattern = paste("/",pattern, "/", collapse="", sep="")

  # identify security jobs by id
  jobsListRows <- grep(pattern, jobs$"title and description", ignore.case=T)

  jobsList <- jobs[jobsListRows,]
}

selectJobsWithSalary <- function(jobs) {
  jobs2 <- subset(jobs, salaryClean > 5000 & salaryClean <= 150000)
  jobs2 <- subset(jobs2, !is.na(as.numeric(as.character(id))))
  jobs2$salaryClean <- as.integer(as.character(jobs2$salaryClean))
  jobs2
}

getKeywordsData <- function (keywords, jobsList) {

  keyword <- vector("character");
  njobs <- integer();
  locations <- vector();
  salary <- vector();
  
  for (i in keywords) {
    # extracting the keywords

    keywords_rows <- grep(i, jobsList$"title and description", ignore.case=T)

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

getSalaryData <- function(jobs) {





}

getKeywordOccurencies <- function(keywords, jobsList) {

  jobKeywords <- vector()

  for (i in keywords) {
    keywords_rows <- grep(i, jobsList$"title and description", ignore.case=T)

    jobKeywords2 <- cbind(keywords_rows,i)

    if(length(keywords_rows)> 0) {
      jobKeywords <- rbind(jobKeywords, jobKeywords2)
    }
  }

  colnames(jobKeywords) <- c("JobId", "Skill")
  jobKeywords

}

printReport <- function (keywords, title, jobs) {
  tmpjobs <- getJobs(keywords, jobs)

  tmpjobs <- selectJobsWithSalary(tmpjobs)

  # get the data
  keywordsData <- getKeywordsData(keywords, tmpjobs)
  keywordsDataPlot <- data.frame(keywordsData$Keyword, keywordsData$Jobs)
  names(keywordsDataPlot) <- c("Skill", "Jobs")

  # let's remove any keywords with no jobs
  keywordsDataPlot <- keywordsDataPlot[!keywordsDataPlot$Jobs == 0, ]

  # plotting the keywords
  barplot(keywordsDataPlot$Jobs,names.arg=keywordsDataPlot$Skill,main=title)

  # need to use proper labels here
  hist(tmpjobs$salaryClean, breaks = 10, main=title, xlab="Salary", ylab="Jobs")
  boxplot(as.integer(tmpjobs$salaryClean), main=title)
  plot(tmpjobs$salaryClean, main=title)

  jobKeywords <- as.data.frame(getKeywordOccurencies(keywords, tmpjobs))
  # cross tabulation
  jobKeywordsTable <- table(jobKeywords[["JobId"]], jobKeywords[["Skill"]])

  cor(jobKeywordsTable)

  psych::describe(tmpjobs$salaryClean)
  
  #k-mean
  km <- kmeans(tmpjobs$salaryClean, 4, iter.max = 100, nstart = 30)
  plot(tmpjobs$salaryClean, main = title, col=km$cluster)

  tmpjobs
}

getAssociateRules <- function (keywords, jobsList, allKeyword) {
  
  tmpOccur <- data.frame(getKeywordOccurencies(keywords, jobsList))
  
  # We can do two different associate rules:
  # one with all the keywords (allKeyword == "Yes") 
  # and the other with only the keyword which are inside description and title (allKeyword == "No")
  
  
  if (allKeyword == "No"){
    
    # get the data
    keywordsData <- getKeywordsData(keywords, jobsList)
    keywordsDataPlot <- data.frame(keywordsData$Keyword, keywordsData$Jobs)
    names(keywordsDataPlot) <- c("Skill", "Jobs")
    
    # let's remove any keywords with no jobs
    keywordsDataPlot <- keywordsDataPlot[!keywordsDataPlot$Jobs == 0, ]
    
    #making data frame of true/false, 0 = false ; 1 = true
    tmpTF <- matrix("0", ncol = length( keywordsDataPlot$Skill) , nrow = nrow(jobsList))
    colnames(tmpTF) <- keywordsDataPlot$Skill
    
  }else{
    
    tmpTF <- matrix("0", ncol = length( keywords) , nrow = nrow(jobsList))
    colnames(tmpTF) <- keywords
    
  }
  
  #fill in the T/F data frame
  
  for ( k in 1:nrow(tmpOccur)){
    
    i <- as.numeric(tmpOccur[k, "JobId"])
    j <- as.character(tmpOccur[k, "Skill"])
    
    tmpTF[i,j] <- "1"
    
  }
  
  #Final data frame T/F
  tmpTF <- data.frame(tmpTF, stringsAsFactors = TRUE)
  
  #we applied associate rules algorithm of R
  rules <- apriori(tmpTF, parameter=list(support=0.50,confidence=0.50))
  inspect(rules)
  rules 
}

getDegree <- function(jobsList){
  
  level <- c("Student","Entry","Experienced","Supervisor of Staff","Executive")
  
  vect <- vector("numeric", nrow(jobsList))
  k<- 1
  
  for(i in level){
    
    vect_rows <- grep(i, jobsList$"career level", ignore.case=T)
    for(j in vect_rows){
      vect[j] <- k
    }
    k<-k+1
  }
  
  vect
}


getLocation <- function(jobsList){
  
  # locations <- c("London", "Birmingham", "Manchester", "Leeds", "Bristol", "Sheffield", "Coventry", "Cardiff", "Reading", "Nottingham")
  locations <- c("Nottingham", "Reading", "Cardiff", "Coventry", "Sheffield", "Bristol", "Leeds", "Manchester", "Birmingham", "London")
  
  vect <- vector("numeric", nrow(jobsList))
  k<- 1
  
  for(i in locations){
    
    vect_rows <- grep(i, jobsList$"location2", ignore.case=T)
    for(j in vect_rows){
      vect[j] <- k
    }
    k<-k+1
  }
  
  vect
}


getFrequencyMatrix <- function(keywords, jobsList, discret){
  
  
  #tmpTF is the frequency matrix like a true-false matrix but with the number of occurence of each keywords for each jobs
  
  if(discret=="T"){
    tmpTF <- matrix(0, ncol = length( keywords) + 3 , nrow = nrow(jobsList))
    colnames(tmpTF) <- c(keywords, "lowSalary", "mediumSalary", "highSalary")
  }else{
    tmpTF <- matrix(0, ncol = length( keywords) , nrow = nrow(jobsList))
    colnames(tmpTF) <- c(keywords)
  }
 
  
  for(j in 1:nrow(jobsList)){

    for(i in keywords){
      
      kw <- paste("[,. ]",i,"[,. ]", sep="")
      
      if( attr(regexpr(kw, jobsList$"title and description"[j], ignore.case = T),"match.length") != -1){
        tmpTF[j,i] <- length(gregexpr(kw, jobsList$"title and description"[j], ignore.case = T)[[1]])
      }else{
        tmpTF[j,i] <- 0
      }
    }
  }
  
  #normalize the frequency matrix
  sum <- rowSums(tmpTF)
  for(j in 1:nrow(jobsList)){
    
    for(i in keywords){
      
      if (sum[j] != 0){
        tmpTF[j,i] <- tmpTF[j,i]/sum[j]
      }
    }
  }
    
  
if(discret == "T"){
  
   
    #k-mean on salary
    km <- kmeans(jobsList$salaryClean, 3, iter.max = 100, nstart = 50)
    
    #determine which cluster of salary corresponding to low medium or hight salary
    i <- 1
    j <- 1
    while (km$cluster[i] == km$cluster[j]){
      j <- j+1
    }
    
    k <- j
    
    while ((km$cluster[i] == km$cluster[k]) || (km$cluster[j] == km$cluster[k]) ){
      k <- k+1
    }
    
    vectSalijk <- c(jobsList$salaryClean[i],jobsList$salaryClean[j],jobsList$salaryClean[k])
    
    if (jobsList$salaryClean[i] == max(vectSalijk)){
      high <- km$cluster[i]
    }else{
      if(jobsList$salaryClean[j] == max(vectSalijk)){
        high <- km$cluster[j]
      }else{
        high <- km$cluster[k]
      }
    }
    
    if (jobsList$salaryClean[i] == min(vectSalijk)){
      low <- km$cluster[i]
    }else{
      if(jobsList$salaryClean[j] == min(vectSalijk)){
        low <- km$cluster[j]
      }else{
        low <- km$cluster[k]
      }
    }
    
    
    #fill in the low medium and high salary with 0 and 1
    for ( i in 1:nrow(jobsList) ){
      
      if (km$cluster[i] == low){
        
        tmpTF[i,"lowSalary"]=1
        tmpTF[i,"mediumSalary"]=0
        tmpTF[i,"highSalary"]=0
        
      }else{
        
        if (km$cluster[i] == high){
          
          tmpTF[i,"lowSalary"]=0
          tmpTF[i,"mediumSalary"]=0
          tmpTF[i,"highSalary"]=1
          
        }else{
          
          tmpTF[i,"lowSalary"]=0
          tmpTF[i,"mediumSalary"]=1
          tmpTF[i,"highSalary"]=0
          
        }
      }
    }
    
    tmpTF <- data.frame(tmpTF)
    
}else{
  tmp <- data.frame(tmpTF)
  tmpTF <- data.frame(tmpTF, jobsList$salaryClean)
  names(tmpTF) <- c(names(tmp),"salary")
}

  
  vect <- getLocation(jobsList)
  tmpTF2 <- data.frame(cbind(tmpTF, vect))
  names(tmpTF2) <- c(names(tmpTF), "location")
  
  vect <- getDegree(jobsList)
  tmpTF3 <- data.frame(cbind(tmpTF2, vect))
  names(tmpTF3) <- c(names(tmpTF2), "level")
  
  
  tmpTF3
}



getClustering <- function(keywords, jobsList,tmpTF2){
  

  
  km2 <- kmeans(tmpTF2, 6, iter.max = 1000, nstart = 200)
#  plot(jobsList$salaryClean, tmpTF$Security, main = "Security Frequency versus Security", col=km2$cluster)
 
  
     plot3d(tmpTF2$location, tmpTF2$level, jobsList$salaryClean, col=km2$cluster, type="s", size=0.5, axes=F)
     axes3d(edges=c("x--", "y--", "z"), lwd=3, axes.len=2, labels=FALSE)
     grid3d("x")
     grid3d("y")
     grid3d("z")
  
  
 scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$level, tmpTF2$Security, main = " x = Continuous Salary, y = Level, z = Security Frequency", color = km2$cluster)
#     scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$location, tmpTF2$Oracle, main = " x = Continuous Salary, y = Location, z = Oracle Frequency", color = km2$cluster)

#    scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$location, tmpTF2$Penetration.Testing, main = " x = Continuous Salary, y = Location, z = Penetration Testing Frequency", color = km2$cluster)
#    scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$location, tmpTF2$Security, main = " x = Continuous Salary, y = Location, z = Security Frequency", color = km2$cluster)
#    
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$Centos, main = " x = Continuous Salary, y = LondonOrNot, z = Centos Frequency", color = km2$cluster)
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$Debian, main = " x = Continuous Salary, y = LondonOrNot, z = Debian Frequency", color = km2$cluster)
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$Linux, main = " x = Continuous Salary, y = LondonOrNot, z = Linux Frequency", color = km2$cluster)
#    
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$Matlab, main = " x = Continuous Salary, y = LondonOrNot, z = Matlab Frequency", color = km2$cluster)
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$SAS, main = " x = Continuous Salary, y = LondonOrNot, z = SAS Frequency", color = km2$cluster)
#   scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$SPSS, main = " x = Continuous Salary, y = LondonOrNot, z = SPSS Frequency", color = km2$cluster)
#   
#  scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$MySQL, main = " x = Continuous Salary, y = LondonOrNot, z = MySQL Frequency", color = km2$cluster)
#  scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$Oracle, main = " x = Continuous Salary, y = LondonOrNot, z = Oracle Frequency", color = km2$cluster)
#  scatterplot3d::scatterplot3d(jobsList$salaryClean, tmpTF2$londonOrNot, tmpTF2$SQL.Server, main = " x = Continuous Salary, y = LondonOrNot, z = SQL.Server Frequency", color = km2$cluster)
#   
 km2
}

getAllKeywordOccurencies <- function(in.file, jobsList){
  
  AllKeywords <- read.xlsx(in.file, 1,header = TRUE)
  AllKeywords <- AllKeywords$Skill
  AllKeywords <- as.character(AllKeywords)
  AllKeywords <- gsub("\\+", "[+]", AllKeywords)
  AllKeywordsData <- getKeywordsData(AllKeywords, jobsList)
  OccurPercent <- AllKeywordsData$Jobs / nrow(jobsList)
  AllKeywordsData <- data.frame(AllKeywordsData$Keyword, AllKeywordsData$Jobs, OccurPercent)
  names(AllKeywordsData) <- c("Skill", "nJobs", "Percent")
  AllKeywordsData
  
}

getPCA <- function(keywords, jobsList, tmpTF, nb){
  
  
  tmpTF2 <- data.frame(cbind(tmpTF$location, jobsList$salaryClean, tmpTF$level)) 
  names(tmpTF2) <- c("location", "salary", "level")
  
  for(i in 1:nb){
    
    tmp <- tmpTF2
    tmpTF2 <- data.frame(cbind(tmpTF2, tmpTF[,i]))
    names(tmpTF2) <- c(names(tmp),keywords[i])
    
  }

  
  #############  METHOD 1 http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
  # Installation du paquet FactoMine R
  #apply pca
    secPCA <- PCA(tmpTF2, graph=TRUE)
    plot(secPCA, cex=0.3, select = 0 , shadow= TRUE)  # cex taille des labels, select= selectionner selon crit?re, ici pour retirer labels, shadow: qd ya labels, axes dessin?es shadow si labels chevauchent
    secPCA
  #http://www.inside-r.org/packages/cran/pca3d/docs/pca3d
  
    
}



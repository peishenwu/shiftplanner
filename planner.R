##by Pei-shen Wu, MD (2015)
##version 2015-12-21

options(stringsAsFactors = FALSE, 
        scipen=999) ##disable scientific number notation
set.seed(1234)
library(xlsx)
#library(utils)
library(compiler)
#
##do Iterations
iter_max = 300000
#
config.data <- read.xlsx("planner_config.xlsx", 1, row.names = NULL)
contraspace <- config.data[2:nrow(config.data),-c(1,2,ncol(config.data), (ncol(config.data)-1))]

persondata <- config.data[2:nrow(config.data),c(1,2)]
Encoding(persondata$name) <- "UTF-8"

strucdata <- config.data[2:nrow(config.data),c(ncol(config.data), (ncol(config.data)-1))]
holidays <- config.data[1,-c(1,2,ncol(config.data), (ncol(config.data)-1))]
holidays <- as.numeric(gsub("X","",names(holidays[,holidays == 1])))

contraspace_days <- as.numeric(gsub("X","",names(contraspace)))
junior_rowindex <- c(1:length(persondata$level))[persondata$level %in% c("R1","R2")]
senior_rowindex <- c(1:length(persondata$level))[persondata$level %in% c("R3","R4")]

##sort by complexity
sort.config.data <- data.frame(persondata, strucdata,
                               flexible_holidays = rep(Inf,nrow(contraspace)),
                               flexible_workdays = rep(Inf,nrow(contraspace)), 
                               contraspace)

for(irow in 1:nrow(contraspace)){
  availabledays <- contraspace[irow,]
  availabledays <- contraspace_days[availabledays == 0]
  available.holidays <- availabledays[(availabledays %in% holidays)]
  available.workdays <- availabledays[!(availabledays %in% holidays)]
  #
  sort.config.data$flexible_holidays[irow] <- length(available.holidays) - strucdata$holidays[irow]
  sort.config.data$flexible_workdays[irow] <- length(available.workdays) - strucdata$workdays[irow]
}#end for

sort.config.data <- sort.config.data[order(sort.config.data$flexible_holidays,
                                           sort.config.data$flexible_workdays,
                                           decreasing = F),]

contraspace <- sort.config.data[,-c(1:6)]
appointspace <- 1*(contraspace == 2)
contraspace <- contraspace - 2*appointspace
##
strucdata <- sort.config.data[, c(3,4)]
persondata <- sort.config.data[, c(1,2)]

##
appoint_holidays <- apply(appointspace[,holidays],1,sum)
appoint_workdays <- apply(appointspace[,-holidays],1,sum)
appoint_struc <- data.frame(workdays = appoint_workdays,
                            holidays = appoint_holidays)

## check for configuration error
if ((sum(sort.config.data$flexible_holidays < 0) + sum(sort.config.data$flexible_workdays < 0)) != 0){
  stop("Error !! Someone set too much contras, leading to not enough available workdays or holidays")
}#end if

count_holidays <- length(holidays)
count_workdays <- length(contraspace_days) - count_holidays
if (sum(strucdata$workdays) != 2*count_workdays | sum(strucdata$holidays) != 2*count_holidays){
  stop("Error in holidays or workdays configuration")
}#end if

## if more than two persons appoint the same day for on-duty, then it's an error
if (sum(apply(contraspace, 2, function(x){sum(x == 2)}) > 2) != 0){
  stop("Error !! more than two persons appoint the same day for on-duty")
}#end if

##if appoint structure disobeys strucdata then it's an error
if ((sum(appoint_struc$holidays > strucdata$holidays)!=0) | (sum(appoint_struc$workdays > strucdata$workdays)!=0)){
  message(paste("\nThere are more appointed holidays than previously determined in: ",
                paste(persondata$name[appoint_struc$holidays > strucdata$holidays], collapse = ","), sep=""))
  message(paste("\nThere are more appointed workdays than previously determined in: ",
                paste(persondata$name[appoint_struc$workdays > strucdata$workdays], collapse = ","), sep=""))
  stop("Process halted")
}#end if


##worker functions
getintervals <- function(x){
  result = Inf
  if (length(x) >1){
    result = (sapply(2:length(x),function(i){x[i]-x[i-1]}))
  }
  result
}#end function

repeatdetect <- function(x, target){
  result = 0
  if (length(x)>1){
    result <- sum(sapply(2:length(x),
                         function(i){
                           (x[i] == x[i-1]) * (x[i] == target)
                         }))
  }
  result
}#end function

#pb <- txtProgressBar(min = 1, max = iter_max, style=3)
#progress <- function(n){ setTxtProgressBar(pb, n) }

##
Algorithm <- function(contraspace_days, contraspace, iter_max, strucdata, holidays, appointspace){
  count_iter = 0
  results <- list()
  iter_log <- rep(0,iter_max %/% 250)
  
  ##worker functions
  mininterval <- function(x){
    result = Inf
    if (length(x) >1){
      #result = min(sapply(2:length(x),function(i){x[i]-x[i-1]}))
      result = min(x[2:length(x)] - x[1:length(x)-1]) #vectorized version
    }
    result
  }#end function
  
  ##
  init_time = Sys.time() #set time at beginning
  avg_trials = c() ## for recording of HOW MUCH TRIALS ON AVERAGE FOR A SINGLE SOLUTION
  ##
  while(T){ #do many iterations
    count_iter = count_iter + 1
    ##
    if (count_iter %% 250 == 0){
      remaining.time <- round(((iter_max-count_iter)/count_iter)*as.numeric(difftime(Sys.time(),init_time,units="secs")))
      progress.status <- round(count_iter*100/iter_max)
      #cat("\014") #ctrl+L to clear screen in Rstudio console
      if(.Platform$OS.type == "unix"){system('clear')} #to clear screen in mac OSX console
      if(.Platform$OS.type == "windows"){
        #system('cls')
        cat(rep("\n",64))
        } #to clear screen in windows DOS console
      
      avg_trials <- c(avg_trials, (count_iter %/% length(results)))
      avg_trials <- avg_trials[avg_trials != Inf]
      
      cat(paste("\nIteration: ",count_iter," of total ",iter_max,
                "\nRemaining time: ", 
                (remaining.time%/%3600)," hrs ",
                (remaining.time%%3600%/%60)," min ",
                (remaining.time%%3600%%60)," sec ",
                "\nObtained: ",length(results),
                "\nAverage yield: 1 solution per ",round(mean(avg_trials))," trials",
                "\n",paste("|",paste(rep("=",round(progress.status*0.6)),collapse=""),
                           paste(rep(" ",round((100-progress.status)*0.6)),collapse=""),"| ",
                           progress.status,"%",sep=""),
                sep=""))
      #progress(count_iter) #update progress bar status
      iter_log[count_iter %/% 250+1] <- length(results)
    }
    ##
    if(count_iter > iter_max){break} #exit iteration if completed
    
    #renew workspace and contraspace.updated
    #workspace <- matrix(data=0, nrow = nrow(contraspace), ncol = ncol(contraspace))
    workspace <- appointspace ##initialize with appointment space
    contraspace.updated <- (contraspace | appointspace)*1
    
    ##update contraspace if 2 duties exist per day (do this first, because it can be filled by appointment)
    contraspace.updated[,(apply(workspace,2,sum) == 2)] <- 1 
    
    ##Planner algorithm
    skip = F
    for(irow in 1:nrow(workspace)){
      availabledays <- contraspace.updated[irow,]
      availabledays <- contraspace_days[availabledays != 1]      
      
      ##identify which workday/holiday had been appointed prior
      holiday.appointed <- contraspace_days[appointspace[irow,]==1][holidays]
      holiday.appointed <- holiday.appointed[complete.cases(holiday.appointed)]
      workday.appointed <- contraspace_days[appointspace[irow,]==1][-holidays]
      workday.appointed <- workday.appointed[complete.cases(workday.appointed)]
      
      ##reset
      holiday.to.fill <- NULL
      workday.to.fill <- NULL
      
      ##
      if (strucdata$holidays[irow]!=0){
        available.holidays <- availabledays[(availabledays %in% holidays)]
        if((length(available.holidays) == 0) | (length(available.holidays) < strucdata$holidays[irow])){
          skip <- T
          break #exit for loop
        }else{
          holiday.to.fill <- c(holiday.appointed, available.holidays)
          if(length(available.holidays)>1){ #only sample when there is more than one choice
            holiday.to.fill <- c(holiday.appointed, 
                                 sample(available.holidays, strucdata$holidays[irow] - appoint_struc$holidays[irow]))
          }#end if          
        }#end if
      }#end if
      
      if (strucdata$workdays[irow]!=0){
        available.workdays <- availabledays[!(availabledays %in% holidays)]
        if((length(available.workdays) == 0) | (length(available.workdays) < strucdata$workdays[irow])){
          skip <- T
          break #exit for loop
        }else{
          workday.to.fill <- c(workday.appointed, available.workdays)
          if(length(available.workdays)>1){ #only sample when there is more than one choice
            workday.to.fill <- c(workday.appointed,
                                 sample(available.workdays, strucdata$workdays[irow] - appoint_struc$workdays[irow]))
          }#end if
        }#end if
      }#end if
      ##
      
      if (length(holiday.to.fill)!=0){
        workspace[irow, c(holiday.to.fill)] <- 1
      }#end if
        
      if (length(workday.to.fill)!=0){
        workspace[irow, c(workday.to.fill)] <- 1
      }#end if
      
      ##update contraspace if 2 duties exist per day
      contraspace.updated[,(apply(workspace,2,sum) == 2)] <- 1 
      
      ##update contraspace according to strucdata status      
      contraspace.updated[apply(workspace[,c(holidays)],1,sum) == strucdata$holidays, c(holidays)] <- 1 ##if already match holiday struc for every person
      contraspace.updated[apply(workspace[,-c(holidays)],1,sum) == strucdata$workdays, -c(holidays)] <- 1 ##if already match workday struc for every person
      ##
    #  
    }#end for
    #
    if(skip){  
      next  ##next iteration
    }else{
      ##check for errors and contraindications
      error = F
      
      ## every shift interval is at least >= 1 to prevent QD, can manually adjust)
      min.int <- sapply(1:nrow(workspace), 
                        function(x){
                          mininterval(c(1:ncol(workspace))[c(workspace[x,]==1)])
                        })
      
      if (sum(min.int>1) != nrow(workspace)){error = T}
      
      ## check if holidays and workdays all follow strucdata
      #if (sum(apply(workspace[,c(holidays)],1,sum) != strucdata$holidays) != 0 
      #    | sum(apply(workspace[,-c(holidays)],1,sum) != strucdata$workdays) != 0){error = T}
      
      ##if no error then aggregate result
      if (!error){
        results <- c(results, list(workspace))
      }#end if
    }#end if
  }#end of iteration
  ##
  list(results = results, 
       iter_log = iter_log,
       avg_trials = avg_trials)
}#end of Algorithm

##
cAlgorithm <- cmpfun(Algorithm) ##use compiler for faster performance
output <- cAlgorithm(contraspace_days, contraspace, iter_max, strucdata, holidays, appointspace)
results <- output[[1]]
iter_log <- output[[2]]
avg_trials <- output[[3]]
##calculate quality metrics
## 1. counts of QODs
## 2. discreteness of shifts
## 3. number of days where "senior residents on duty = 2 personale" 
## 4. number of continuous QODs
if (length(results)!=0){
  quality <- data.frame(resultIndex = 1:length(results),
                        cQODs = rep(NA,length(results)),
                        discrete = rep(NA,length(results)),
                        cSenior = rep(NA,length(results)),
                        continQODs = rep(NA,length(results)),
                        NoContras = rep(NA, length(results)),
                        NotFollowHoliday = rep(NA, length(results)),
                        NotFollowWorkday = rep(NA, length(results)))
  
  for (item in 1:length(results)){
    quality$cSenior[item] <- sum(apply(results[[item]][senior_rowindex,],2,sum) == 2)
    quality$discrete[item] <- min(apply(results[[item]],1,
                                        function(x){
                                          mean(getintervals(c(1:ncol(results[[item]]))[x == 1]))
                                        }))
    quality$cQODs[item] <- sum(apply(results[[item]],1,
                                     function(x){
                                       sum(getintervals(c(1:ncol(results[[item]]))[x == 1]) == 2)
                                     }))
    quality$continQODs[item] <- sum(apply(results[[item]],1,
                                          function(x){
                                            repeatdetect(getintervals(c(1:ncol(results[[item]]))[x == 1]),2)
                                          }))
    quality$NoContras[item] <- (sum((contraspace + results[[item]]) == 2) == 0)
    quality$NotFollowHoliday[item] <- sum(apply(results[[item]][,c(holidays)],1,sum) != strucdata$holidays)
    quality$NotFollowWorkday[item] <- sum(apply(results[[item]][,-c(holidays)],1,sum) != strucdata$workdays)
  }#end for
  
  quality <- quality[order(quality$continQODs,
                           quality$cQODs,
                           -quality$discrete,
                           quality$cSenior,
                           decreasing = F),]
  
  ##plot iteration obtains
  #plot(x = c(1:length(iter_log)), y = iter_log, type = "s", ylab = "Obtained", xlab = "Iterations", xaxt = "n")
  #axis(1, at = c(1:length(iter_log)), labels = 250*unique(1:iter_max %/% 250))
  
  
  ##Append each resulting table with personal data
  for (item in 1:length(results)){
    results[[item]] <- cbind(persondata, results[[item]])
  }#end for
  
  contraspace <- cbind(persondata, contraspace)
  strucdata <- cbind(persondata, strucdata)
  
  ##save results
  saveRDS(list(contraspace = contraspace,
               strucdata = strucdata,
               appoint_struc = appoint_struc,
               results = results,
               quality = quality,
               iter_log = iter_log,
               iter_max = iter_max,
               holidays = holidays,
               avg_trials = avg_trials,
               appointspace = appointspace,
               contraspace_days=contraspace_days),
          "planner_output.rds")
  
  ## start swapper...
  message("\nStarting swapper...")
  source("swapper.R")
  
}else{
  stop("No results were obtained...")
}


options(stringsAsFactors = FALSE, 
        scipen=999)
library(utils)
library(doSNOW)
message("initalizing parallel threads...")
cl <- makeSOCKcluster(20)
registerDoSNOW(cl)
##
data <- readRDS("planner_output.rds")
results <- data$results
contraspace <- data$contraspace[,-c(1,2)] #remove names, levels columns
holidays <- data$holidays
contraspace_days <- data$contraspace_days
persondata <- data$contraspace[, c(1,2)]
strucdata <- data$strucdata

junior_rowindex <- c(1:length(persondata$level))[persondata$level %in% c("R1","R2")]
senior_rowindex <- c(1:length(persondata$level))[persondata$level %in% c("R3","R4")]

##worker functions
mininterval <- function(x){
  result = Inf
  if (length(x) >1){
    #result = min(sapply(2:length(x),function(i){x[i]-x[i-1]}))
    result = min(x[2:length(x)] - x[1:length(x)-1]) #vectorized version
  }
  result
}#end function

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

#
swap.Agg <- c()
for(ritem in 1:length(results)){
  message(paste("\nWorking on result ",ritem,"/",length(results),sep=""))
  ##Prepare swap space
  swapspace <- data.frame(p.index = NA, day = NA, HorW = NA)
  for(p.index in 1:nrow(results[[ritem]])){
    dutydays <- contraspace_days[results[[ritem]][p.index,-c(1,2)] == 1]
    dutyholidays <- dutydays[dutydays %in% holidays]
    dutyworkdays <- dutydays[!(dutydays %in% holidays)]
    #
    swapspace <- rbind(swapspace,
                       data.frame(p.index = rep(p.index, length(dutyholidays)), 
                                  day = dutyholidays, 
                                  HorW = rep(1, length(dutyholidays))),
                       data.frame(p.index = rep(p.index, length(dutyworkdays)), 
                                  day = dutyworkdays, 
                                  HorW = rep(2, length(dutyworkdays))))
  }#end for
  swapspace <- swapspace[complete.cases(swapspace),]
  
  ##1 for Holiday, 2 for Workday
  for(HW.index in 1:2){
    #message(paste("Working on ",c("holidays","workdays")[HW.index],sep=""))
    swapwork <- swapspace[swapspace$HorW == HW.index,]
    swapcomb <- combn(1:nrow(swapwork),2)
    #
    pb <- txtProgressBar(min = 1, max = ncol(swapcomb), style=3)
    progress <- function(n){ setTxtProgressBar(pb, n) }
    opts <- list(progress=progress)
    
    #parallel approach for faster performance...
    swap.result  <- foreach(comb.index = 1:ncol(swapcomb), .options.snow=opts) %dopar% {
      foreach.result = NA
      #skip if the days or person are the same
      if((swapwork$day[swapcomb[1,comb.index]] != swapwork$day[swapcomb[2,comb.index]])
         &(swapwork$p.index[swapcomb[1,comb.index]] != swapwork$p.index[swapcomb[2,comb.index]])){
        #check if swap contra exists
        if((contraspace[swapwork$p.index[swapcomb[1,comb.index]],swapwork$day[swapcomb[2,comb.index]]]!=1)
           &(contraspace[swapwork$p.index[swapcomb[2,comb.index]],swapwork$day[swapcomb[1,comb.index]]]!=1)){
          #swap if not skipped
          workspace <- results[[ritem]][,-c(1,2)]
          workspace[swapwork$p.index[swapcomb[1,comb.index]],swapwork$day[swapcomb[2,comb.index]]] <- 1
          workspace[swapwork$p.index[swapcomb[2,comb.index]],swapwork$day[swapcomb[1,comb.index]]] <- 1
          workspace[swapwork$p.index[swapcomb[1,comb.index]],swapwork$day[swapcomb[1,comb.index]]] <- 0
          workspace[swapwork$p.index[swapcomb[2,comb.index]],swapwork$day[swapcomb[2,comb.index]]] <- 0
          
          #check for any errors such as QDs
          min.int <- sapply(1:nrow(workspace), 
                            function(x){
                              mininterval(c(1:ncol(workspace))[c(workspace[x,]==1)])
                            })
          if (sum(min.int>1) == nrow(workspace)){
            #if no errors then save swapped result
            foreach.result <- workspace
          }#end if
        }#end if
      }#end if
      foreach.result
    }#end foreach combination
    
    ##remove NAs
    rswap.result <- c()
    for (rswap.index in 1:length(swap.result)){
      if(identical(swap.result[[rswap.index]], NA)){next}
      rswap.result <- c(rswap.result, list(swap.result[[rswap.index]]))
    }#end for if NAs
    
    ##
    swap.Agg <- c(swap.Agg, rswap.result)
  }#end for HW.index
}#end for every results

##calculate qualities of post-swapped results and pre-swapped results
##remove names and level columns
r.results <- c()
for(r.index in 1:length(results)){
  r.results <- c(r.results, list(results[[r.index]][,-c(1,2)]))
}
results <- c(r.results,swap.Agg)
##calculate quality metrics
## 1. counts of QODs
## 2. discreteness of shifts
## 3. number of days where "senior residents on duty = 2 personale" 
## 4. number of continuous QODs

  quality <- data.frame(resultIndex = 1:length(results),
                        cQODs = rep(NA,length(results)),
                        discrete = rep(NA,length(results)),
                        cSenior = rep(NA,length(results)),
                        continQODs = rep(NA,length(results)),
                        NoContras = rep(NA, length(results)),
                        NotFollowHoliday = rep(NA, length(results)),
                        NotFollowWorkday = rep(NA, length(results)),
                        OnlyTwoDutiesPerDay = rep(NA, length(results)))
  
  pb <- txtProgressBar(min = 1, max = length(results), style=3)
  message("\nCalculating quality...")
  for (item in 1:length(results)){
    progress(item)
    #
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
    quality$OnlyTwoDutiesPerDay[item] <- sum(apply(results[[item]],2,sum) != 2)
  }#end for
  
  #remove those which don't obey strucdata or only two duties per day or has contras
  #also only show those without continuous QODs
  quality <- quality[quality$NoContras == T &
                       quality$NotFollowHoliday == 0 &
                       quality$NotFollowWorkday == 0 &
                       quality$OnlyTwoDutiesPerDay == 0 &
                       quality$continQODs == 0,]

  quality <- quality[order(quality$cQODs,
                           quality$cSenior,
                           -quality$discrete,
                           decreasing = F),]

##save results
saveRDS(list(contraspace = contraspace,
             strucdata = strucdata,
             results = results,
             quality = quality,
             holidays = holidays,
             persondata = persondata,
             contraspace_days=contraspace_days),
        "swapper_output.rds")

##release resources
stopCluster(cl)






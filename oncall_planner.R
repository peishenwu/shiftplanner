options(stringsAsFactors = FALSE)
set.seed(1234)
library(utils)

##
OncallPlanner <- function(index=1, iter_max=20000){
  data <- readRDS("swapper_output.rds")
  #
  pb <- txtProgressBar(min = 1, max = iter_max, style=3)
  progress <- function(n){ setTxtProgressBar(pb, n) }
  #
  result.index <- data$quality$resultIndex[index]
  persondata <- data$persondata
  contraspace <- data$contraspace
  result <- data$results[[result.index]]
  holidays <- data$holidays
  worksrc <- data.frame(persondata, senior = rep(NA,nrow(persondata)),
                        result)
  worksrc$senior <- sapply(worksrc$level,function(x){c(1:4)[c("R1","R2","R3","R4") %in% gsub(" ","",x)]})
  ##
  oncall.needed <- data$contraspace_days[sapply(data$contraspace_days, 
                                                function(x){sum(worksrc[worksrc[,-c(1:3)][,x] == 1,]$senior>2) == 0})]
  oncall.persons <- c(1:nrow(persondata))[worksrc$senior > 2]
  
  ##check for error --> no available persons on some certain day??
  if(sum(apply(contraspace[oncall.persons,oncall.needed], 2, sum) == length(oncall.persons)) != 0){
    stop(paste("No available senior duties for oncall on day:",
               paste(data$contraspace_days[apply(contraspace[oncall.persons,oncall.needed], 2, sum) == length(oncall.persons)],
                     collapse=","),
               sep=" "))
  }#end if
  
  ##sort days according to contra limitations
  sort.order <- data.frame(days = oncall.needed,
                           contras = apply(contraspace[oncall.persons,oncall.needed],2,sum))
  sort.order <- sort.order[order(sort.order$contras, decreasing = T),]
  oncall.needed <- sort.order$days
  oncall.holidays <- oncall.needed[oncall.needed %in% holidays]
  oncall.workdays <- oncall.needed[!(oncall.needed %in% holidays)]
  ##run many times
  oncall.results <- c()
  oncall.quality <- c()
  count_iter = 0
  ##
  message("\nPlanning oncall schedule...")
  while(T){
    count_iter <- count_iter + 1
    progress(count_iter)
    if(count_iter>iter_max){break}
    ##
    workspace <- matrix(data=0, nrow = nrow(contraspace), ncol = ncol(contraspace)) #renew workspace
    for(dayindex in oncall.needed){
      available.oncall <- oncall.persons[contraspace[oncall.persons,dayindex] != 1]
      choose.oncall <- available.oncall
      if (length(available.oncall)>1){ choose.oncall <- sample(available.oncall, 1) }
      #
      workspace[choose.oncall,dayindex] <- 1
    }#end for
    ##
    oncall.results <- c(oncall.results, list(workspace))
    ##
    #calculate quality of obtained oncall schedule
    loading <- 2*apply(workspace[oncall.persons,oncall.holidays],1,sum) + apply(workspace[oncall.persons,oncall.workdays],1,sum)
    oncall.quality <- c(oncall.quality, sd(loading))
  }#end while
  ##
  oncall.quality <- data.frame(result.index = c(1:length(oncall.results)),
                               quality = oncall.quality)
  oncall.quality <- oncall.quality[order(oncall.quality$quality, decreasing = F),]
  best.result <- oncall.results[[oncall.quality$result.index[1]]]
  ##save results
  saveRDS(list(contraspace = contraspace,              
               best.result = best.result,
               persondata = persondata,
               oncall.results = oncall.results,
               oncall.quality = oncall.quality, 
               oncall.needed = oncall.needed,
               oncall.persons = oncall.persons,
               oncall.holidays = oncall.holidays,
               oncall.workdays = oncall.workdays,
               iter_max = iter_max,
               holidays = holidays,
               contraspace_days=data$contraspace_days,
               useindex = index),
          "oncallplanner_output.rds")
}#end function


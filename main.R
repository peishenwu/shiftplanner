##Basic I/O settings
setwd("/Users/peishenwu/Google 雲端硬碟/【01】醫學/PMR/R2/排班/Planner")  ##modify the path here...

##check for installed packages, if missing then download it
packagedata <- installed.packages()
packagename <- packagedata[,1]
requiredpackage <- c("xlsx","compiler","doSNOW","utils","ReporteRs")
notinstalled <- requiredpackage[!(requiredpackage %in% packagename)]
if(length(notinstalled)!=0){
  #install missing packages
  for(index in 1:length(notinstalled)){
    install.packages(notinstalled[index])
  }
}#end if

##starting every module: 
#planner -> swapper
msg <- try({source("planner.R")},F)[1]
if(length(msg)!=0){
  if(grepl("error",tolower(msg))){
    stop("\nNo results were obtained... thus process halted") 
  }
}

#oncall_planner
source("oncall_planner.R")
data <- readRDS("swapper_output.rds")
for(index in 1:length(data$results)){
  msg <- try(OncallPlanner(index),F)[1]
  if(length(msg)==0){break}
}#end for

#output_toWord
source("output_toWord.R")
data <- readRDS("oncallplanner_output.rds")
index <- data$useindex
WordOutput(index)

##
message("\nProcess completed")
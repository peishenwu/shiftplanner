options(stringsAsFactors = FALSE)
Sys.setlocale("LC_TIME", "C")
library(ReporteRs)

WordOutput <- function(index=1){
  message("\nOutputting results to Word...")
  ##
  data <- readRDS("swapper_output.rds")
  result.index <- data$quality$resultIndex[index]
  persondata <- data$persondata
  holidays <- data$holidays
  result <- data$results[[result.index]]
  worksrc <- data.frame(persondata, senior = rep(NA,nrow(persondata)),
                        result)
  worksrc$senior <- sapply(worksrc$level,function(x){c(1:4)[c("R1","R2","R3","R4") %in% gsub(" ","",x)]})
  #
  oncall.data <- readRDS("oncallplanner_output.rds")
  oncall.bestresult <- oncall.data$best.result
  oncall.persons <- oncall.data$oncall.persons
  
  #get the weekday of the next month's first day
  year = as.numeric(format(Sys.time(), "%Y"))
  next.month = as.numeric(format(Sys.time(), "%m"))+1
  if(next.month>12){year <- year+1 ; next.month = 1} ##if next year
  weekday_index <- c(1:7)[c("Monday","Tuesday","Wednesday","Thursday",
                            "Friday","Saturday","Sunday") %in% weekdays(as.Date(paste(year,"/",next.month,"/1",sep="")))]
  workspace <- as.data.frame(matrix(data="", nrow = 37, ncol = 8))
  workspace[1,] <- c("","一","二","三","四","五","六","日")
  workspace[as.vector(sapply(seq(0,30,6),function(x){x+c(3:7)})),1] <- c("4E1","4W1","Intern","On call","主治醫師")
  #
  workspace[2,(weekday_index+1):8] <- 1:(8-weekday_index)
  workspace[8,2:8]  <- c(1:7) + as.numeric(workspace[2,8])
  workspace[14,2:8] <- c(1:7) + as.numeric(workspace[8,8])
  workspace[20,2:8] <- c(1:7) + as.numeric(workspace[14,8])
  workspace[26,2:8] <- c(1:7) + as.numeric(workspace[20,8])
  workspace[32,2:8] <- c(1:7) + as.numeric(workspace[26,8])
  #
  for (weekrow in c(2,8,14,20,26,32)){
    monthdays <- as.numeric(unlist(workspace[weekrow,workspace[weekrow,]!=""]))
    monthdays <- monthdays[monthdays <= 31]
    weekdata <- worksrc[,-c(1:3)][,monthdays]
    oncall.weekdata <- oncall.bestresult[,monthdays]
    #
    if (length(weekdata)!=0){
      indexend <- ncol(weekdata)
      if(length(indexend)==0){
        indexend <- 1
        weekdata <- cbind(weekdata, rep(NA, length(weekdata)))
      }#end if
      
      for (weekdayindex in 1:indexend){
        #fill in duties
        dutydata <- worksrc[weekdata[,weekdayindex] == 1,1:3]
        dutydata <- dutydata[order(dutydata$senior, decreasing = F),]
        workspace[c(weekrow+c(1:2)),workspace[weekrow,]!=""][,weekdayindex] <- dutydata$name
        #fill in oncalls
        workspace[(weekrow+4),workspace[weekrow,]!=""][,weekdayindex] <- if(sum(dutydata$senior>2)==0){
          persondata$name[oncall.weekdata[,weekdayindex]==1]
        }else{" "}
      }#end for
    }#end if not empty
  }#end for weekrow
  
  #remove redundant columns
  workspace[26,workspace[26,]>31] <- ""
  workspace[32,workspace[32,]>31] <- ""
  if(sum(workspace[32,] == "")==0){ workspace[32:37,1:8] <- NA }
  workspace <- workspace[complete.cases(workspace),]
  
  #
  doc = docx(title = 'Duty schedule output')
  MyFTable = FlexTable(data = workspace, 
                       add.rownames = F,
                       header.columns = F)
  
  doc = addFlexTable(doc, MyFTable)
  
  ## add statistical data output
  stattable <- as.data.frame(matrix(data="",ncol=10,nrow=14))
  stattable[1,1:5]  <- c("一線","假日","平時","點數","累計")
  stattable[1,6:10] <- c("一線","假日","平時","點數","累計")
  stattable[8,1:5]  <- c("二線","假日","平時","點數","累計")
  stattable[8,6:10] <- c("二線","假日","平時","點數","累計")
  ##
  stattable[1+c(1:sum(worksrc$senior==1)),1] <- worksrc$name[worksrc$senior==1]
  stattable[1+c(1:sum(worksrc$senior==1)),2] <- apply(worksrc[worksrc$senior==1,][,-c(1:3)][,holidays],1,sum)
  stattable[1+c(1:sum(worksrc$senior==1)),3] <- apply(worksrc[worksrc$senior==1,][,-c(1:3)][,-holidays],1,sum)
  stattable[1+c(1:sum(worksrc$senior==1)),4] <- 2*apply(worksrc[worksrc$senior==1,][,-c(1:3)][,holidays],1,sum) + apply(worksrc[worksrc$senior==1,][,-c(1:3)][,-holidays],1,sum)
  #
  stattable[1+c(1:sum(worksrc$senior==2)),6] <- worksrc$name[worksrc$senior==2]
  stattable[1+c(1:sum(worksrc$senior==2)),7] <- apply(worksrc[worksrc$senior==2,][,-c(1:3)][,holidays],1,sum)
  stattable[1+c(1:sum(worksrc$senior==2)),8] <- apply(worksrc[worksrc$senior==2,][,-c(1:3)][,-holidays],1,sum)
  stattable[1+c(1:sum(worksrc$senior==2)),9] <- 2*apply(worksrc[worksrc$senior==2,][,-c(1:3)][,holidays],1,sum) + apply(worksrc[worksrc$senior==2,][,-c(1:3)][,-holidays],1,sum)
  #
  stattable[8+c(1:sum(worksrc$senior==3)),1] <- worksrc$name[worksrc$senior==3]
  stattable[8+c(1:sum(worksrc$senior==3)),2] <- apply(worksrc[worksrc$senior==3,][,-c(1:3)][,holidays],1,sum)
  stattable[8+c(1:sum(worksrc$senior==3)),3] <- apply(worksrc[worksrc$senior==3,][,-c(1:3)][,-holidays],1,sum)
  stattable[8+c(1:sum(worksrc$senior==3)),4] <- 2*apply(worksrc[worksrc$senior==3,][,-c(1:3)][,holidays],1,sum) + apply(worksrc[worksrc$senior==3,][,-c(1:3)][,-holidays],1,sum)
  #
  stattable[8+c(1:sum(worksrc$senior==4)),6] <- worksrc$name[worksrc$senior==4]
  stattable[8+c(1:sum(worksrc$senior==4)),7] <- apply(worksrc[worksrc$senior==4,][,-c(1:3)][,holidays],1,sum)
  stattable[8+c(1:sum(worksrc$senior==4)),8] <- apply(worksrc[worksrc$senior==4,][,-c(1:3)][,-holidays],1,sum)
  stattable[8+c(1:sum(worksrc$senior==4)),9] <- 2*apply(worksrc[worksrc$senior==4,][,-c(1:3)][,holidays],1,sum) + apply(worksrc[worksrc$senior==4,][,-c(1:3)][,-holidays],1,sum)
  
  #oncall
  oncalltable <- as.data.frame(matrix(data="",ncol=10,nrow=7))
  oncalltable[1,1:5]  <- c("二線","假日","平時","點數","累計")
  oncalltable[1,6:10] <- c("二線","假日","平時","點數","累計")
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 3)),1] <- worksrc$name[oncall.persons][worksrc$senior[oncall.persons] == 3]
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 3)),2] <- apply(oncall.bestresult[worksrc$senior == 3,holidays],1,sum)
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 3)),3] <- apply(oncall.bestresult[worksrc$senior == 3,-holidays],1,sum)
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 3)),4] <- apply(oncall.bestresult[worksrc$senior == 3,-holidays],1,sum) + 2*apply(oncall.bestresult[worksrc$senior == 3,holidays],1,sum)
  #
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 4)),6] <- worksrc$name[oncall.persons][worksrc$senior[oncall.persons] == 4]
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 4)),7] <- apply(oncall.bestresult[worksrc$senior == 4,holidays],1,sum)
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 4)),8] <- apply(oncall.bestresult[worksrc$senior == 4,-holidays],1,sum)
  oncalltable[1+c(1:sum(worksrc$senior[oncall.persons] == 4)),9] <- apply(oncall.bestresult[worksrc$senior == 4,-holidays],1,sum) + 2*apply(oncall.bestresult[worksrc$senior == 4,holidays],1,sum)
  #
  MyFTable = FlexTable(data = stattable, 
                       add.rownames = F,
                       header.columns = F)
  
  ##next page
  doc = addPageBreak(doc)
  doc = addFlexTable(doc, MyFTable)
  
  ##add line
  doc = addParagraph( doc, " ", stylename = "Normal" )
  MyFTable = FlexTable(data = oncalltable, 
                       add.rownames = F,
                       header.columns = F)
  doc = addFlexTable(doc, MyFTable)
  ##
  writeDoc(doc, 'duty_schedule.docx')
  system('open duty_schedule.docx')
}#end function


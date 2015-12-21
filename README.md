# NTUH PMR shift schedule planner
### by Pei-shen Wu, MD (2015)

This program was created and tested in R version 3.1.0 (2014-04-10) running under Mac OS Mavericks 10.9.5 Traditional Chinese version. The R code were saved under UTF-8 format, FYI if you want to try the files in MS Windows.

# To get started...
1. Modify the setwd path in the main.R file, this file holds the main program
2. Put all the other files under the same path defined in the main.R file
3. The duty personale, monthly workdays/holidays and all constraint information are defined in the planner_config.xlsx file by setting the cells 2,1,0
The definition for {2,1,0} are the following:
  1. "0" means available for duty
  2. "1" means not available for duty
  3. "2" means appointing that day for duty

4. By executing main.R, the process will run as follows:
  1. planner.R will read-in the data from planner_config.xlsx, and use the monte carlo method to solve for possible solutions (iteration max 300000 times), the results will be saved in the planner_output.rds file
  2. then swapper.R will be executed, trying for more possible solutions according to planner's results, the results will be saved in the swapper_output.rds file
  3. then oncall_planner.R will be executed to determine for oncall duties schedule, results will be saved in the oncallplanner_output.rds file
  4. Finally, all results will be outputted to MS Word by running output_toWord.R

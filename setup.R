###################### ###
### COVID TRACKING    ####
### SETUP SCRIPT       ###
### see processing.R   ###
### for script list    ###
###################### ###

# capture start time -- calculates run time in output script
s.tm <- Sys.time()

#install libraries
library(readr)
library(wbstats)
library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(usmap)
library(ggthemes)
library(gridExtra)

# ensure proper working directory
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("correct directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)

# make sure population table is available
ifelse(file.exists("worldpop.csv"),
       message("World Population file exist"),
       ifelse(exists("world.pop"),
              fwrite(world.pop,"worldpop.csv"), 
              { world.pop <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)
              fwrite(world.pop,"worldpop.csv") }
       )
      )

# set world.pop as data.table, if it is not 
if(!is.data.table(world.pop)) setDT(world.pop)



message("End of SETUP")

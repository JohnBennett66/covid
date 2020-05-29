
library(readr)

library(dplyr)
library(data.table)
library(lubridate)
library(ggplot2)
library(gridExtra)



if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("correct directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)


st <- Sys.time()

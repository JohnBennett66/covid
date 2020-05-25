### OUTPUT FILES FROM VISUALIZATIONS  ####

### SETUP LOCATION FOR VISUALS
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID/visual_artifacts"), 
        print("wrong directory"), 
        missing = NULL)

### REMOVE EXITING FILES FROM VISUALS DIRECTORY -- ARCHIVE EXISTING FILES 
currentdir <- getwd()
files <- list.files(path = currentdir, full.names = TRUE)
for (i in 2:length(files)) {
    file.copy(files[i], files[1], overwrite = TRUE)
  }
for (i in 2:length(files)) {
  file.remove(files[i])
}

### PRINT VISUALS
dly.d.us.chrt  # to screen

ggsave(file = "daily_deaths_us_chart.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file

pdf("daily_deaths_us_chart.pdf")  # to PDF file
print(dly.d.us.chrt)
dev.off()



### RESET WORKING DIRECTORY
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("wrong directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)


getwd()

et <- Sys.time()
tdiff <- et-st
print(tdiff)









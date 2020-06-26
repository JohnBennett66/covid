###################### ###
### COVID TRACKING    ####
### SETUP SCRIPT       ###
### see processing.R   ###
### for script list    ###
###################### ###



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
# US daily chart
dly.d.us.chrt  # to screen
ggsave(file = "daily_deaths_us_chart.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf("daily_deaths_us_chart.pdf")  # to PDF file
print(dly.d.us.chrt)
dev.off()

# Single State daily chart
dly.d.single.state.chrt  # to screen
filename <- paste0("daily_deaths_",st,"_chart.png")
ggsave(file = filename, dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf("daily_deaths_singlestate_chart.pdf")  # to PDF file
print(dly.d.single.state.chrt)
dev.off()

# Single State daily chart
q6
ggsave(file = "top_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
q5
ggsave(file = "second_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
q4
ggsave(file = "three_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
q3
ggsave(file = "four_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
q2
ggsave(file = "five_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
q1
ggsave(file = "bottom_tier.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file


# Single State daily chart
dly.trend.up.states.chrt  # to screen
ggsave(file = "daily_trend_up_chart.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf("daily_trend_up_chart.pdf")  # to PDF file
print(dly.d.single.state.chrt)
dev.off()

### RESET WORKING DIRECTORY
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("wrong directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)


getwd()

e.tm <- Sys.time()
tdiff <- e.tm-s.tm
print(tdiff)









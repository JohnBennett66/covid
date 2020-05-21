### OUTPUT FILES FROM VISUALIZATIONS  ####

### SETUP LOCATION FOR VISUALS
wd <- getwd()
setwd(paste0(wd,"/visual_artifacts"))
getwd()


### PRINT VISUALS
dly.d.us.chrt  # to screen

ggsave(file = "daily_deaths_us_chart.png", dpi = 600, width = 20, height = 8, units = "in")  # to image file

pdf("daily_deaths_us_chart.pdf")  # to PDF file
print(dly.d.us.chrt)
dev.off()



### RESET WORKING DIRECTORY
setwd(wd)
getwd()













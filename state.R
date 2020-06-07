###  SINGLE STATE CHART #####
###  BASED ON 6 TIER STATE CHARTS

### Pick State
st <- readline(prompt = "Which State?")


### SETUP LOCATION FOR VISUALS
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID/visual_artifacts"), 
        print("wrong directory"), 
        missing = NULL)


# SINGLE STATE
single.state <- state.date.d[state == st]
# variables for limits, placements, etc
when <- df[,date(mdy_hms(max(runtime, na.rm = TRUE)))]
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- single.state[,max(diff)]
hgt.ma <- single.state[,max(diff)]
ma.date <- single.state[diff==max(diff),date]
outlier.date <- ymd(20200331)
max.date <- single.state[,max(date)]
# spacing calcs
sp.one <- round(hgt * 0.025)
# calculations for labelling
dly.avg.d <- round(single.state[,mean(diff)],0)
dly.avg.d.or <- round(single.state[between(single.state$date,outlier.date,end.date),mean(diff)],0)
dly.max.d <- round(single.state[,max(diff)],0)
dly.avg.d.rcnt <- round(single.state[between(single.state$date,end.date-14,end.date),mean(diff)],0)
half.days <- round(as.integer(count(single.state[between(single.state$date,outlier.date,end.date)]))/2,0)
dly.avg.d.half <- round(single.state[between(single.state$date,end.date-half.days,end.date),mean(diff)],0)
tot.d <- format(single.state[,max(deaths)], big.mark = ",")
max.col <- single.state[diff==dly.max.d,date]
below.cnt <- as.integer(count(single.state[date>outlier.date][diff<single.state[date==outlier.date,diff]]))
below2.cnt <- as.integer(count(single.state[date>start.date][diff<single.state[date==start.date,diff]]))
first.diff <- single.state[date==outlier.date,diff]
start.two <- nrow(single.state)-14
end.two <- nrow(single.state)
start.half <- nrow(single.state)-half.days
end.half <- end.two #same end point
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:7,mavg:=0]
for (i in 7:end.two) {
  list.a[i] <- single.state[(i-6):i,round(mean(diff),digits = 0)]
}
single.state[,basic:=list.a[,mavg]]
# two weeks
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:(start.two - 1),mavg:=0]
for (i in start.two:end.two) {
  list.a[i] <- single.state[(i-13):i,round(mean(diff),digits = 0)]
}
single.state[,twoweek:=list.a[,mavg]]
# half of no outliers
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:(start.half - 1),mavg:=0]
for (i in start.half:end.two) {
  list.a[i] <- single.state[(i-half.days):i,round(mean(diff),digits = 0)]
}
single.state[,halfdays:=list.a[,mavg]]



### THE PLOT :: SINGLE STATE :: Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.d.single.state.chrt <- ggplot(data = single.state, aes(x = date, y = diff)) +
  geom_col(fill = "darkred") + scale_x_date(limits = c(start.date, end.date)) +
  labs(title = "Daily Deaths Covid-19 in US by Date", 
       subtitle = paste0("Focus on 29 February to current (",max.date,")"),
       y = "Cumulative Deaths",
       x = "2020",
       #tag = "tag",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett", 
                       sep = "\n")) + 
  geom_hline(aes(yintercept = dly.avg.d.or), colour = 'blue', size = 1) +
  geom_hline(aes(yintercept = dly.avg.d), colour = 'grey2', size = 1) +
  geom_hline(aes(yintercept = first.diff), colour = 'darkslateblue', size = 1) + 
  geom_line(data = single.state[start.two:end.two], aes(x = date, y = twoweek), colour = 'orangered', size = 2) +
  geom_line(data = single.state[start.half:end.two], aes(x = date, y = halfdays), colour = 'goldenrod', size = 2) +
  geom_line(data = single.state[7:end.two], aes(x = date, y = basic), colour = 'greenyellow', size = 2) +
  geom_label(label = paste(paste0("Total Deaths = ",tot.d), 
                           paste0(st), sep = "\n"),
             x = place.one, y = hgt - sp.one,   #label.padding = unit(0.4, "lines"), label.size = 0.3,
             color = "black", fill="#ffeeee", size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d.or - sp.one, label = "Before 31 March are outliers", 
            color = 'blue', size = 3) +
  geom_text(x = start.date + 5, y = dly.avg.d.or + 1, label = paste0("Daily Average = ", dly.avg.d.or), 
            color = 'blue', size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d + sp.one, label = paste("Uncorrected Average = ", dly.avg.d),
            color = 'grey2', size = 3) +
  geom_text(x = start.date + 6, y = dly.avg.d - sp.one, label = paste("Days below= ", below2.cnt),
            color = 'grey2', size = 3) + 
  geom_text(x = max.col, y = dly.max.d + sp.one, label = paste("Max Daily = ", dly.max.d),
            color = 'blue', size = 3) +
  geom_text(x = outlier.date - 4, y = single.state[date==outlier.date,diff] + sp.one, label = paste("First Date for Average = ", single.state[date==outlier.date,diff]),
            color = 'darkslateblue', size = 3) +
  geom_text(x = start.date + 6, y = first.diff + sp.one, label = paste("Days below corrected average = ", below.cnt),
            color = 'darkslateblue', size = 3) + 
  geom_segment(x = Sys.Date()-14, y = dly.avg.d.rcnt, xend = end.date, yend = dly.avg.d.rcnt,
               color = 'orangered', size = 1) + 
  geom_segment(x = Sys.Date()-half.days, y = dly.avg.d.half, xend = end.date, yend = dly.avg.d.half,
               color = 'goldenrod', size = 1) + 
  geom_label(label = paste0("7 Day Moving Average"), x = end.date - 15, y = dly.avg.d.or + sp.one * 7, 
             color = "greenyellow", fill="#333333", size = 5) + 
  geom_label(label = paste0("Half of the Included Days Moving Average"), x = end.date - 5, y = dly.avg.d.or + sp.one * 3,
             color = "goldenrod", fill="#444444", size = 3) + 
  geom_label(label = paste0("14 Day Moving Average"), x = end.date, y = dly.avg.d.or + sp.one,
             color = "orangered", fill="#444444", size = 3)





# Single State daily chart
dly.d.single.state.chrt  # to screen
filename <- paste0("daily_deaths_",st,"_chart.png")
ggsave(file = filename, dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf("daily_deaths_singlestate_chart.pdf")  # to PDF file
print(dly.d.single.state.chrt)
dev.off()



### RESET WORKING DIRECTORY
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("wrong directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)

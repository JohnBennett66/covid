###  SINGLE STATE CHART #####
###  BASED ON 6 TIER STATE CHARTS

### Pick State
st <- readline(prompt = "Which State?")

### SETUP LOCATION FOR VISUALS
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID/visual_artifacts"), 
        print("wrong directory"), 
        missing = NULL)

# SINGLE STATE :: DEATH CHART
single.state <- us.state[state == st]
# variables for limits, placements, etc
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- single.state[,max(new_deaths)]
hgt.ma <- single.state[date == end.date - 5,new_deaths]
ma.date <- end.date - 5
outlier.date <- ymd(20200325)
max.date <- single.state[,max(date)]
adj <- as.integer(single.state[,max(new_deaths)] / 10)
# spacing calcs
sp.one <- round(hgt * 0.025)
# calculations for labelling
dly.avg.d <- round(single.state[,mean(new_deaths)],0)
dly.avg.d.or <- round(single.state[between(single.state$date,outlier.date,end.date),mean(new_deaths)],0)
dly.max.d <- round(single.state[,max(new_deaths)],0)
dly.avg.d.rcnt <- round(single.state[between(single.state$date,end.date-14,end.date),mean(new_deaths)],0)
half.days <- round(as.integer(count(single.state[between(single.state$date,outlier.date,end.date)]))/2,0)
dly.avg.d.half <- round(single.state[between(single.state$date,end.date-half.days,end.date),mean(new_deaths)],0)
tot.d <- format(single.state[,max(cum_deaths)], big.mark = ",")
max.col <- single.state[new_deaths==dly.max.d,date]
below.cnt <- as.integer(count(single.state[date>outlier.date][new_deaths<single.state[date==outlier.date,new_deaths]]))
below2.cnt <- as.integer(count(single.state[date>start.date][new_deaths<single.state[date==start.date,new_deaths]]))
first.new_deaths <- single.state[date==outlier.date,new_deaths]
start.two <- nrow(single.state)-14
end.two <- nrow(single.state)
start.half <- nrow(single.state)-half.days
end.half <- end.two #same end point
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:5,mavg:=0]
for (i in 5:end.date) {
  list.a[i] <- single.state[(i-5):i,round(mean(new_deaths),digits = 0)]
}
single.state[,basic:=list.a[,mavg]]
# two weeks
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:14,mavg:=0]
for (i in 14:nrow(single.state)) {
  list.a[i] <- single.state[(i-13):i,round(mean(new_deaths),digits = 0)]
}
single.state[,twoweek:=list.a[,mavg]]


### THE PLOT :: SINGLE STATE :: Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.d.single.state.chrt <- ggplot(data = single.state, aes(x = date, y = new_deaths)) +
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
  # geom_hline(aes(yintercept = dly.avg.d.or), colour = 'blue', size = 1) +
  # geom_hline(aes(yintercept = dly.avg.d), colour = 'grey2', size = 1) +
  # geom_text(x = start.date + 5, y = dly.avg.d.or + adj, label = paste0("Daily Average = ", dly.avg.d.or), 
  #           color = 'blue', size = 4) +
  # geom_text(x = start.date + 5, y = dly.avg.d + adj, label = paste("Uncorrected Average = ", dly.avg.d),
  #           color = 'grey2', size = 3) +
  #geom_hline(aes(yintercept = first.new_deaths), colour = 'darkslateblue', size = 1) +
  # geom_text(x = start.date + 5, y = dly.avg.d.or - adj, label = "Before 31 March are outliers", 
  #           color = 'blue', size = 3) + 
  geom_line(data = single.state, aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = single.state, aes(x = date, y = basic), colour = 'blue', size = 1) +
  geom_label(label = paste(paste0("Total Deaths = ",tot.d), 
                           paste0(st), sep = "\n"), 
             x = place.one, y = hgt,   
             color = "black", fill="#ffeeee", size = 4) + 
  # geom_label(label = "First non-Outlier Day", 
  #            x = outlier.date - 4, y = single.state[date == outlier.date, new_deaths],   
  #            color = "black", fill="#ffeeee", size = 3) + 
  geom_label(label = paste0("5 Day Moving Average"), x = place.one, y = hgt - adj, 
             color = "blue", fill="#BBBBBB", size = 5) + 
  geom_label(label = paste0("14 Day Moving Average"), x = place.one, y = hgt - (adj *2),
             color = "orangered", fill="#333333", size = 4)



# Single State daily chart
dly.d.single.state.chrt  # to screen
filename <- paste0("daily_deaths_",st,"_chart.png")
filename2 <- paste0("daily_deaths_",st,"_chart.pfd")
ggsave(file = filename, dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf(filename2)  # to PDF file
print(dly.d.single.state.chrt)
dev.off()



# SINGLE STATE :: CASES CHART

# variables for limits, placements, etc
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- single.state[,max(new_cases)]
hgt.ma <- single.state[date == end.date - 5,new_cases]
ma.date <- end.date - 5
outlier.date <- ymd(20200325)
max.date <- single.state[,max(date)]
adj <- as.integer(single.state[,max(new_cases)] / 10)
# spacing calcs
sp.one <- round(hgt * 0.025)
# calculations for labelling
dly.avg.d <- round(single.state[,mean(new_cases)],0)
dly.avg.d.or <- round(single.state[between(single.state$date,outlier.date,end.date),mean(new_cases)],0)
dly.max.d <- round(single.state[,max(new_cases)],0)
dly.avg.d.rcnt <- round(single.state[between(single.state$date,end.date-14,end.date),mean(new_cases)],0)
half.days <- round(as.integer(count(single.state[between(single.state$date,outlier.date,end.date)]))/2,0)
dly.avg.d.half <- round(single.state[between(single.state$date,end.date-half.days,end.date),mean(new_cases)],0)
tot.d <- format(single.state[,max(cum_cases)], big.mark = ",")
max.col <- single.state[new_cases==dly.max.d,date]
below.cnt <- as.integer(count(single.state[date>outlier.date][new_cases<single.state[date==outlier.date,new_cases]]))
below2.cnt <- as.integer(count(single.state[date>start.date][new_cases<single.state[date==start.date,new_cases]]))
first.new_cases <- single.state[date==outlier.date,new_cases]
start.two <- nrow(single.state)-14
end.two <- nrow(single.state)
start.half <- nrow(single.state)-half.days
end.half <- end.two #same end point
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:5,mavg:=0]
for (i in 5:end.date) {
  list.a[i] <- single.state[(i-5):i,round(mean(new_cases),digits = 0)]
}
single.state[,basic:=list.a[,mavg]]
# two weeks
list.a <- data.table(mavg=seq(1:nrow(single.state)))
list.a[1:14,mavg:=0]
for (i in 14:nrow(single.state)) {
  list.a[i] <- single.state[(i-13):i,round(mean(new_cases),digits = 0)]
}
single.state[,twoweek:=list.a[,mavg]]

### THE PLOT :: SINGLE STATE :: Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.c.single.state.chrt <- ggplot(data = single.state, aes(x = date, y = new_cases)) +
  geom_col(fill = "darkred") + scale_x_date(limits = c(start.date, end.date)) +
  labs(title = "Daily Cases Covid-19 in US by Date", 
       subtitle = paste0("Focus on 29 February to current (",max.date,")"),
       y = "Cumulative Cases",
       x = "2020",
       #tag = "tag",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett", 
                       sep = "\n")) + 
  # geom_hline(aes(yintercept = dly.avg.d.or), colour = 'blue', size = 1) +
  # geom_hline(aes(yintercept = dly.avg.d), colour = 'grey2', size = 1) +
  # geom_text(x = start.date + 5, y = dly.avg.d.or + adj, label = paste0("Daily Average = ", dly.avg.d.or), 
  #           color = 'blue', size = 4) +
  # geom_text(x = start.date + 5, y = dly.avg.d + adj, label = paste("Uncorrected Average = ", dly.avg.d),
  #           color = 'grey2', size = 3) +
  #geom_hline(aes(yintercept = first.new_cases), colour = 'darkslateblue', size = 1) +
  # geom_text(x = start.date + 5, y = dly.avg.d.or - adj, label = "Before 31 March are outliers", 
  #           color = 'blue', size = 3) + 
  geom_line(data = single.state, aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = single.state, aes(x = date, y = basic), colour = 'blue', size = 1) +
  geom_label(label = paste(paste0("Total Cases = ",tot.d), 
                           paste0(st), sep = "\n"), 
             x = place.one, y = hgt,   
             color = "black", fill="#ffeeee", size = 4) + 
  # geom_label(label = "First non-Outlier Day", 
  #            x = outlier.date - 4, y = single.state[date == outlier.date, new_cases],   
  #            color = "black", fill="#ffeeee", size = 3) + 
  geom_label(label = paste0("5 Day Moving Average"), x = place.one, y = hgt - adj, 
             color = "blue", fill="#BBBBBB", size = 5) + 
  geom_label(label = paste0("14 Day Moving Average"), x = place.one, y = hgt - (adj *2),
             color = "orangered", fill="#333333", size = 4)



# Single State daily chart
dly.c.single.state.chrt  # to screen
filename <- paste0("daily_cases_",st,"_chart.png")
filename2 <- paste0("daily_cases_",st,"_chart.pfd")
ggsave(file = filename, dpi = 600, width = 20, height = 8, units = "in")  # to image file
pdf(filename2)  # to PDF file
print(dly.c.single.state.chrt)
dev.off()


### RESET WORKING DIRECTORY
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        print("wrong directory"), 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID"), 
        missing = NULL)

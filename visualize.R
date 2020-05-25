


###  US BY DATE -- COLUMNS  ####

###  Deaths - DAILY CHANGE  ####
# variables for limits, placements, etc
when <- df[,date(mdy_hms(max(runtime, na.rm = TRUE)))]
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- us.date.d[,max(diff) + 100]
hgt.ma <- us.date.d[,max(diff) + 100]
ma.date <- us.date.d[diff==max(diff),date]
outlier.date <- ymd(20200331)
max.date <- us.date.d[,max(date)]
# calculations for labelling
dly.avg.d <- round(us.date.d[,mean(diff)],0)
dly.avg.d.or <- round(us.date.d[between(us.date.d$date,outlier.date,end.date),mean(diff)],0)
dly.max.d <- round(us.date.d[,max(diff)],0)
dly.avg.d.rcnt <- round(us.date.d[between(us.date.d$date,end.date-14,end.date),mean(diff)],0)
half.days <- round(as.integer(count(us.date.d[between(us.date.d$date,outlier.date,end.date)]))/2,0)
dly.avg.d.half <- round(us.date.d[between(us.date.d$date,end.date-half.days,end.date),mean(diff)],0)
tot.d <- format(us.date.d[,max(deaths)], big.mark = ",")
max.col <- us.date.d[diff==dly.max.d,date]
below.cnt <- as.integer(count(us.date.d[date>outlier.date][diff<us.date.d[date==outlier.date,diff]]))
below2.cnt <- as.integer(count(us.date.d[date>start.date][diff<us.date.d[date==start.date,diff]]))
first.diff <- us.date.d[date==outlier.date,diff]
start.two <- nrow(us.date.d)-14
end.two <- nrow(us.date.d)
start.half <- nrow(us.date.d)-half.days
end.half <- end.two #same end point
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(us.date.d)))
list.a[1:7,mavg:=0]
for (i in 7:end.two) {
  list.a[i] <- us.date.d[(i-6):i,round(mean(diff),digits = 0)]
}
us.date.d[,basic:=list.a[,mavg]]
# two weeks
list.a <- data.table(mavg=seq(1:nrow(us.date.d)))
list.a[1:(start.two - 1),mavg:=0]
for (i in start.two:end.two) {
  list.a[i] <- us.date.d[(i-13):i,round(mean(diff),digits = 0)]
}
us.date.d[,twoweek:=list.a[,mavg]]
# half of no outliers
list.a <- data.table(mavg=seq(1:nrow(us.date.d)))
list.a[1:(start.half - 1),mavg:=0]
for (i in start.half:end.two) {
  list.a[i] <- us.date.d[(i-half.days):i,round(mean(diff),digits = 0)]
}
us.date.d[,halfdays:=list.a[,mavg]]
### THE PLOT :: US Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.d.us.chrt <- ggplot(data = us.date.d, aes(x = date, y = diff)) +
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
  geom_line(data = us.date.d[start.two:end.two], aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = us.date.d[start.half:end.two], aes(x = date, y = halfdays), colour = 'goldenrod', size = 1) +
  geom_line(data = us.date.d[7:end.two], aes(x = date, y = basic), colour = 'greenyellow', size = 1) +
  geom_label(label = paste0("Total Deaths = ",tot.d), x = place.one, y = hgt - 150,   #label.padding = unit(0.4, "lines"), label.size = 0.3,
    color = "black", fill="#ffeeee", size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d.or - 100, label = "Before 31 March are outliers", 
            color = 'blue', size = 3) +
  geom_text(x = start.date + 5, y = dly.avg.d.or + 100, label = paste0("Daily Average = ", dly.avg.d.or), 
            color = 'blue', size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d + 75, label = paste("Uncorrected Average = ", dly.avg.d),
            color = 'grey2', size = 3) +
  geom_text(x = start.date + 6, y = dly.avg.d - 75, label = paste("Days below= ", below2.cnt),
            color = 'grey2', size = 3) + 
  geom_text(x = max.col, y = dly.max.d + 75, label = paste("Max Daily = ", dly.max.d),
            color = 'blue', size = 3) +
  geom_text(x = outlier.date - 3.25, y = us.date.d[date==outlier.date,diff] + 75, label = paste("First Date for Average = ", us.date.d[date==outlier.date,diff]),
            color = 'darkslateblue', size = 3) +
  geom_text(x = start.date + 6, y = first.diff + 75, label = paste("Days below corrected average = ", below.cnt),
            color = 'darkslateblue', size = 3) + 
  geom_segment(x = Sys.Date()-14, y = dly.avg.d.rcnt, xend = end.date, yend = dly.avg.d.rcnt,
               color = 'orangered', size = 1) + 
  geom_segment(x = Sys.Date()-half.days, y = dly.avg.d.half, xend = end.date, yend = dly.avg.d.half,
               color = 'goldenrod', size = 1) + 
  geom_label(label = paste0("7 Day Moving Average"), x = ma.date, y = hgt.ma, 
    color = "greenyellow", fill="#333333", size = 5) + 
  geom_label(label = paste0("Half of the Included Days Moving Average"), x = end.date - 4, y = dly.avg.d.or + 300,
    color = "goldenrod", fill="#444444", size = 3) + 
  geom_label(label = paste0("14 Day Moving Average"), x = end.date, y = dly.avg.d.or + 100,
    color = "orangered", fill="#444444", size = 3)



###  US BY WEEKDAY -- BAR/FACETS  ####
###  Deaths - DAILY CHANGE by WEEKDAY  ####
# calculate means
means <- us.weekday.d
means <- means[,mean.tot := mean(diff), by=.(weekday)]
means <- means[week >= 14, mean.or := mean(diff), by=.(weekday)]

  
ggplot(data = us.weekday.d, aes(x = week, y = diff)) +
  geom_col(fill = "darkred") + 
  coord_flip() + 
  facet_grid(rows = vars(weekday)) + 
  geom_hline(aes(yintercept = mean.tot), means, colour = "grey2") + 
  geom_hline(aes(yintercept = mean.or), means, colour = "blue") +  
  labs(title = "Daily Deaths Covid-19 in US by Date", 
       subtitle = paste(paste0(start.date, " to current (",max.date,")"),
                        "Overall Average (black line)",
                        "Average without Outliers (blue line)",
                        sep = "\n"),
       y = "Cumulative Deaths",
       x = "2020",
       #tag = "tag",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett", 
                       sep = "\n"))



              
  
  
# Confirmed Cases
ggplot(data = us.date.c) + 
  geom_col(aes(x = date, y = cases))


# Deaths - ALL
ggplot(data = us.date.d, aes(x = date, y = deaths)) + 
  geom_col() + ylim(0, 100000) + scale_x_date(limits = c(ymd(20200229),NA)) +
  scale_fill_date("blue") +
  labs(title = "Daily Cumulative Covid-19 Deaths in US", 
       subtitle = "Focus on 29 February to current",
       y = "Cumulative Deaths",
       x = "2020",
       caption = paste("data from John Hopkins, downloaded from data.world", 
                       "visualization by John Bennett", 
                       sep = "\n"))

# focus early
ggplot(data = us.date.d, aes(x = date, y = deaths)) + 
  geom_col() +
  labs(title = "Daily Cumulative Covid-19 Deaths in US", 
       subtitle = "Focus on 1 April to current",
       y = "Cumulative Deaths",
       x = "2020",
       caption = "data from John Hopkins, downloaded from data.world") + 
  scale_x_date(limits = c(ymd(20200122),ymd(20200415))) 

# focus late
ggplot(data = us.date.d, aes(x = date, y = deaths)) + 
  geom_col() +
  scale_x_date(limits = c(ymd(20200401),NA))




# early focus
ggplot(data = us.date.d, aes(x = date, y = diff)) + 
  geom_col(color = "blue", fill = "blue") + 
  scale_x_date(limits = c(ymd(20200320),ymd(20200505))) +
  labs(title = "Daily Deaths Covid-19 in US by Date", 
       subtitle = "Focus on 29 February to current",
       y = "Cumulative Deaths",
       x = "2020",
       caption = paste("data from John Hopkins, downloaded from data.world", 
                       "visualization by John Bennett", 
                       sep = "\n")) +
  geom_text(aes(label = diff), vjust = -0.5)
  














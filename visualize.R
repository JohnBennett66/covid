###  VARIOUS VISUALIZATIONS AND ACCOMPANYING DATA BITS  ####


###  US BY DATE -- COLUMNS  ####

###  Deaths - DAILY CHANGE  ####
# variables for limits, placements, etc
when <- us.date[,max(date)]
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- us.date[,max(new_deaths) + 100]
hgt.ma <- us.date[date == end.date - 5,new_deaths] + 300
ma.date <- end.date - 5
outlier.date <- ymd(20200331)
max.date <- us.date[,max(date)]
adj <- as.integer(us.date[,max(new_deaths)] * .01)
# calculations for labelling
dly.avg.d <- round(us.date[,mean(new_deaths)],0)
dly.avg.d.or <- round(us.date[between(us.date$date,outlier.date,end.date),mean(new_deaths)],0)
dly.max.d <- round(us.date[,max(new_deaths)],0)
dly.avg.d.rcnt <- round(us.date[between(us.date$date,end.date-14,end.date),mean(new_deaths)],0)
half.days <- round(as.integer(count(us.date[between(us.date$date,outlier.date,end.date)]))/2,0)
dly.avg.d.half <- round(us.date[between(us.date$date,end.date-half.days,end.date),mean(new_deaths)],0)
tot.d <- format(us.date[,max(cum_deaths)], big.mark = ",")
max.col <- us.date[new_deaths==dly.max.d,date]
below.cnt <- as.integer(count(us.date[date>outlier.date][new_deaths<us.date[date==outlier.date,new_deaths]]))
below2.cnt <- as.integer(count(us.date[date>start.date][new_deaths<us.date[date==start.date,new_deaths]]))
first.new_deaths <- us.date[date==outlier.date,new_deaths]
start.two <- nrow(us.date)-14
end.two <- nrow(us.date)
start.half <- nrow(us.date)-half.days
end.half <- end.two #same end point
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(us.date)))
list.a[1:5,mavg:=0]
for (i in 5:end.two) {
  list.a[i] <- us.date[(i-5):i,round(mean(new_deaths),digits = 0)]
}
us.date[,basic:=list.a[,mavg]]
# two weeks
list.a <- data.table(mavg=seq(1:nrow(us.date)))
list.a[1:14,mavg:=0]
for (i in 14:end.two) {
  list.a[i] <- us.date[(i-13):i,round(mean(new_deaths),digits = 0)]
}
us.date[,twoweek:=list.a[,mavg]]

### THE PLOT :: US Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.d.us.chrt <- ggplot(data = us.date, aes(x = date, y = new_deaths)) +
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
  geom_text(x = start.date + 5, y = dly.avg.d.or + adj, label = paste0("Daily Average = ", dly.avg.d.or), 
            color = 'blue', size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d + adj, label = paste("Uncorrected Average = ", dly.avg.d),
            color = 'grey2', size = 3) +
  geom_hline(aes(yintercept = first.new_deaths), colour = 'darkslateblue', size = 1) +
  geom_text(x = start.date + 5, y = dly.avg.d.or - adj, label = "Before 31 March are outliers", 
            color = 'blue', size = 3) + 
  geom_line(data = us.date, aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = us.date, aes(x = date, y = basic), colour = 'blue', size = 1) +
  geom_label(label = paste(paste0("Total Deaths = ",tot.d), 
                           paste0("US Overall"), sep = "\n"), 
             x = place.one, y = hgt - 150,   
    color = "black", fill="#ffeeee", size = 4) + 
  geom_label(label = "First non-Outlier Day", 
             x = outlier.date - 4, y = us.date[date == outlier.date, new_deaths],   
             color = "black", fill="#ffeeee", size = 3) + 
  geom_label(label = paste0("5 Day Moving Average"), x = ma.date, y = hgt.ma, 
    color = "blue", fill="#BBBBBB", size = 5) + 
  geom_label(label = paste0("14 Day Moving Average"), x = end.date -2, y = hgt.ma + 200,
    color = "orangered", fill="#222222", size = 4)

### THE SECOND PLOT :: US Daily Cases & Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####
dly.b.us.chrt <- ggplot(data = us.date, aes(x = date, y = new_deaths)) +
  geom_col(fill = "darkred") + geom_col(data = us.date.c, aes(x = date, y = new_deaths), fill = "dodgerblue") +
  scale_x_date(limits = c(start.date, end.date)) +
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
  geom_hline(aes(yintercept = first.new_deaths), colour = 'darkslateblue', size = 1) + 
  geom_line(data = us.date[start.two:end.two], aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = us.date[start.half:end.two], aes(x = date, y = halfdays), colour = 'goldenrod', size = 1) +
  geom_line(data = us.date[7:end.two], aes(x = date, y = basic), colour = 'greenyellow', size = 1) +
  geom_label(label = paste(paste0("Total Deaths = ",tot.d), 
                           paste0("US Overall"), sep = "\n")
             x = place.one, y = hgt - 150,   #label.padding = unit(0.4, "lines"), label.size = 0.3,
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
  geom_text(x = outlier.date - 3.25, y = us.date[date==outlier.date,new_deaths] + 75, label = paste("First Date for Average = ", us.date[date==outlier.date,new_deaths]),
            color = 'darkslateblue', size = 3) +
  geom_text(x = start.date + 6, y = first.new_deaths + 75, label = paste("Days below corrected average = ", below.cnt),
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



###  SINGLE STATE :: COMPLEX CHART -- COLUMNS  ####
###  Deaths - DAILY CHANGE  ####

# SINGLE STATE
st <- "Washington"
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
  geom_hline(aes(yintercept = dly.avg.d.or), colour = 'blue', size = 1) +
  geom_hline(aes(yintercept = dly.avg.d), colour = 'grey2', size = 1) +
  geom_text(x = start.date + 5, y = dly.avg.d.or + adj, label = paste0("Daily Average = ", dly.avg.d.or), 
            color = 'blue', size = 4) +
  geom_text(x = start.date + 5, y = dly.avg.d + adj, label = paste("Uncorrected Average = ", dly.avg.d),
            color = 'grey2', size = 3) +
  #geom_hline(aes(yintercept = first.new_deaths), colour = 'darkslateblue', size = 1) +
  geom_text(x = start.date + 5, y = dly.avg.d.or - adj, label = "Before 31 March are outliers", 
            color = 'blue', size = 3) + 
  geom_line(data = single.state, aes(x = date, y = twoweek), colour = 'orangered', size = 1) +
  geom_line(data = single.state, aes(x = date, y = basic), colour = 'blue', size = 1) +
  geom_label(label = paste(paste0("Total Deaths = ",tot.d), 
                           paste0(st), sep = "\n"), 
             x = place.one, y = hgt,   
             color = "black", fill="#ffeeee", size = 4) + 
  geom_label(label = "First non-Outlier Day", 
             x = outlier.date - 4, y = single.state[date == outlier.date, new_deaths],   
             color = "black", fill="#ffeeee", size = 3) + 
  geom_label(label = paste0("5 Day Moving Average"), x = place.one, y = hgt - adj, 
             color = "blue", fill="#BBBBBB", size = 5) + 
  geom_label(label = paste0("14 Day Moving Average"), x = place.one, y = hgt - (adj *2),
             color = "orangered", fill="#333333", size = 4)



###  US BY WEEKDAY -- BAR/FACETS  ####
###  Deaths - DAILY CHANGE by WEEKDAY  ####
# calculate means
means <- us.weekday.d
means <- means[,mean.tot := mean(new_deaths), by=.(weekday)]
means <- means[week >= 14, mean.or := mean(new_deaths), by=.(weekday)]

  
ggplot(data = us.weekday.d, aes(x = week, y = new_deaths)) +
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



              
###  US BY STATE BY DATE -- COLUMNS  ####

###  Deaths - DAILY CHANGE BY STATE  ####
# variables for limits, placements, etc
when <- df[,date(mdy_hms(max(runtime, na.rm = TRUE)))]
start.date <- ymd(20200229)
end.date <- Sys.Date()
place.one <- start.date + 6
hgt <- state.date.d[,max(new_deaths) + 100]
hgt.ma <- state.date.d[,max(new_deaths) + 100]
ma.date <- state.date.d[new_deaths==max(new_deaths),date]
outlier.date <- ymd(20200331)
max.date <- state.date.d[,max(date)]
# calculations for labelling
# states by tier -- quartile, total deaths
tot.d.st <- state.date.d[,sum(new_deaths), by = state]
colnames(tot.d.st) <- c("state","agg_deaths")
tot.d.st[,quartile := as.integer(cut(tot.d.st$agg_deaths, quantile(tot.d.st$agg_deaths, probs =c(0.0,0.18,0.59,0.72,0.85,0.97,1)),include.lowest = TRUE))]
setorder(tot.d.st,quartile,agg_deaths)
state.date.d <- state.date.d[tot.d.st,on=.(state=state)]
state.date.d[,quartile:=as.factor(quartile)]
setorder(state.date.d,-quartile)
# moving averages
list.s <- data.table(state.date.d[,unique(state)])
for (j in 1:length(unique(state.date.d[,state]))) {
  sdd.s <- state.date.d[state == list.s[j]]
  list.a <- data.table(mavg=seq(1:nrow(sdd.s)))
  list.a[,mavg:=0]
    for (i in 7:nrow(list.a)) {
      list.a[i-6] <- sdd.s[(i-6):i,round(mean(new_deaths),digits = 0)]
    }
  state.date.d[state==sdd.s[,unique(state)],basic:=list.a[,mavg]]
}

### THE PLOT :: US Daily Deaths :: AGREGGATE :: DAILY CHANGE :: ANALYSIS/DETAILS  ####

q6 <- ggplot(data = state.date.d[quartile == 6], aes(x = date, y = new_deaths)) +
        geom_col(fill = "darkred")  + 
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
        labs(title = paste("Daily Deaths Covid-19 in US Daily by State", 
                           "Top Tier States", sep = "\n"),
              subtitle = paste("Out of 6 tiers",
                                paste0("Focus on 29 February to current (",max.date,")"),
                                sep = "\n"))


q5 <- ggplot(data = state.date.d[quartile == 5], aes(x = date, y = new_deaths)) +
        geom_col(fill = "orangered3")  +
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
        labs(title = "Second Tier States")


q4 <- ggplot(data = state.date.d[quartile == 4], aes(x = date, y = new_deaths)) +
        geom_col(fill = "orangered1")  +
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
        labs(title = "Third Tier States")


q3 <- ggplot(data = state.date.d[quartile == 3], aes(x = date, y = new_deaths)) +
        geom_col(fill = "orange2")  +
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
        labs(title = "Fourth Tier States")


q2 <- ggplot(data = state.date.d[quartile == 2], aes(x = date, y = new_deaths)) +
        geom_col(fill = "orange")  +
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
        labs(title = "Fifth Tier States")


q1 <- ggplot(data = state.date.d[quartile == 1], aes(x = date, y = new_deaths)) +
        geom_col(fill = "darkgoldenrod1")  +
        scale_x_date(limits = c(start.date, end.date)) + 
        facet_wrap(vars(state)) + 
        geom_line(aes(x = date, y = basic), size = 1) + 
        labs(title = "Bottom Tier States",
             caption = paste("data from John Hopkins, downloaded from data.world",
                              paste0("data last updated: ", day(when), " ", 
                                     lubridate::month(when, label = TRUE), " ", year(when)), 
                              "visualization by John Bennett", 
                              sep = "\n"))


grid.arrange(q6,q5,q4,q3,q2,q1)

###  STATES TRENDING UP :: FACETS :: COLUMNS  ####
###  Deaths - DAILY CHANGE  ####
state.lst <- c("Maryland", "Ohio", "Virginia", "Florida", "Georgia", "Indiana", "North Carolina", 
               "Maine", "Oklahoma", "South Dakota", "Illinois", "Rhode Island", "California", 
               "Wisconsin", "Arizona", "Texas", "Pennsylvania", "Minnesota")
trend.up <- state.date.c[state %in% state.lst]
# moving averages
# basic
state.lst <- trend.up[,unique(state)]
list.a <- data.table(mavg=seq(1:nrow(trend.up[state == state.lst[1]])))
list.a[1:5,mavg:=0]
for (j in 1:length(trend.up[,unique(state)])) {
  for (i in 5:end.two) {
    list.a[i] <- trend.up[state == state.lst[j]][(i-5):i,round(mean(new_deaths),digits = 0)]
  } 
  trend.up[state == state.lst[j],basic:=list.a[,mavg]]
}

### THE PLOT ####
dly.trend.up.states.chrt <- ggplot(data = trend.up, aes(x = date, y = new_deaths)) + 
  geom_col(fill = "blue") + 
  facet_wrap(vars(state), scales = "free") + 
  geom_line(data = trend.up, aes(x = date, y = basic)) + 
  labs(title = "Daily Cases Covid-19 for States Trending Up* by Date", 
       subtitle = paste(paste0("Focus on 29 February to current (",max.date,")"),
                        paste0("*are, or might be, or were recently trending upward, at least for recents week(s)"),
                        paste0("PLEASE NOTE: The left axis scale varies from chart to chart, so overall cases are quite new_deathserent between these states"),
                        sep = "\n"),
       y = "Cumulative Cases",
       x = "2020",
       #tag = "tag",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett", 
                       sep = "\n"))






  
# Confirmed Cases
ggplot(data = us.date.c) + 
  geom_col(aes(x = date, y = new_deaths))


# Deaths - ALL
ggplot(data = state.date.d, aes(x = date, y = deaths)) + 
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
ggplot(data = us.date, aes(x = date, y = deaths)) + 
  geom_col() +
  labs(title = "Daily Cumulative Covid-19 Deaths in US", 
       subtitle = "Focus on 1 April to current",
       y = "Cumulative Deaths",
       x = "2020",
       caption = "data from John Hopkins, downloaded from data.world") + 
  scale_x_date(limits = c(ymd(20200122),ymd(20200415))) 

# focus late
ggplot(data = us.date, aes(x = date, y = deaths)) + 
  geom_col() +
  scale_x_date(limits = c(ymd(20200401),NA))




# early focus
ggplot(data = us.date, aes(x = date, y = new_deaths)) + 
  geom_col(color = "blue", fill = "blue") + 
  scale_x_date(limits = c(ymd(20200320),ymd(20200505))) +
  labs(title = "Daily Deaths Covid-19 in US by Date", 
       subtitle = "Focus on 29 February to current",
       y = "Cumulative Deaths",
       x = "2020",
       caption = paste("data from John Hopkins, downloaded from data.world", 
                       "visualization by John Bennett", 
                       sep = "\n")) +
  geom_text(aes(label = new_deaths), vjust = -0.5)
  














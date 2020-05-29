###  SINGLE STATE CHART #####
###  BASED ON 6 TIER STATE CHARTS

state.name <- "Ohio"
stn <- state.date.d[state == state.name]
stn[(nrow(stn)-5):nrow(stn), basic:=stn[nrow(stn)-6,basic]]

state.chrt <- ggplot(data = stn, aes(x = date, y = diff)) +
                geom_col(fill = "darkred")  + 
                scale_x_date(limits = c(start.date, end.date)) + 
                facet_wrap(vars(state)) + 
                geom_line(aes(x = date, y = basic), size = 1, colour = "blue") + 
                geom_smooth(method = "lm", colour = "orange3") + 
                geom_smooth(method = "gam", colour = "green4") + 
                labs(title = paste("Daily Deaths Covid-19 in US Daily by State", 
                                   "Top Tier States", sep = "\n"),
                     subtitle = paste("Out of 6 tiers",
                                      paste0("Focus on 29 February to current (",max.date,")"),
                                      sep = "\n")) +
                geom_text(x = end.date - 5, 
                          y = stn[,max(diff)], 
                          label = "Linear Trendline", 
                                        color = 'orange3', size = 5) +
                geom_text(x = end.date - 5, 
                          y = stn[,max(diff)] - (stn[,max(diff)]/10), 
                          label = "Non-Linear Trendline", 
                          color = 'green4', size = 5)
              


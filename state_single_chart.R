###  SINGLE STATE :: SINGLE CHART :: CASES AND DEATHS  ####
###  
###  

### Pick State
st <- readline(prompt = "Which State?")

### SETUP LOCATION FOR VISUALS
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID/visual_artifacts"), 
        print("wrong directory"), 
        missing = NULL)

############################ ###
############################ ###
###  Make all the charts
###  Arrange with arrangeGrob() 
###  for complex arrangements
###  Arrange or "print" with
###  grid.arrange() 
############################ ###
############################ ###



j2 <- ggplot(data = us.state[state == st]) + 
  geom_line(aes(x = date, y = cum_cases_1000), colour = "blue") +
  scale_x_date(limits = c(start.date - 10, end.date)) +
  labs(title = paste0("Daily Deaths Covid-19 in ",st," by Date"), 
       subtitle = paste0("Focus on 29 February to Current (",us.state[state == st, max(date)],")"),
       y = "Daily Counts",
       x = "2020",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett ©", 
                       sep = "\n")) + 
  theme(legend.position = "right")



j1 <- ggplot(data = us.state[state == st]) + 
  geom_line(aes(x = date, y = cum_deaths_1000), fill = "darkred") +
  scale_x_date(limits = c(start.date - 10, end.date)) +
  labs(title = paste0("Daily Deaths Covid-19 in ",st," by Date"), 
       subtitle = paste0("Focus on 29 February to Current (",us.state[state == st, max(date)],")"),
       y = "Daily Counts",
       x = "2020",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett ©", 
                       sep = "\n")) + 
  theme(legend.position = "right")


jj <- grid.arrange(j1,j2)


ggsave(jj, filename = "test.png", dpi = 600, width = 20, height = 8, units = "in")















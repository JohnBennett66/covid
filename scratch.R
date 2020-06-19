





ggplot(data = us.date, aes(x = date, y = new_deaths)) +
  geom_col(fill = "darkred") + scale_x_date(limits = c(start.date, end.date)) +
  labs(title = "Daily Deaths Covid-19 in US by Date", 
       subtitle = paste0("Focus on 29 February to current (",max.date,")"),
       y = "Cumulative Deaths",
       x = "2020",
       #tag = "tag",
       caption = paste("data from John Hopkins, downloaded from data.world",
                       paste0("data last updated: ", day(when), " ", lubridate::month(when, label = TRUE), " ", year(when)),
                       "visualization by John Bennett", 
                       sep = "\n"))

















us.order <- us.state[,max(cum_deaths), by = state]
colnames(us.order) <- c("state","max")

us.state <- us.state[us.order, on = .(state=state)]





wd <- getwd()
iso2 <- fread(file = paste0(wd,"/data/country_continent.csv"))
countries <- df[,c(10,7:8)] %>% unique()
colnames(iso2) <- c("iso2","continent")
iso2 <- iso2[countries,on=.(iso2=iso2), nomatch = NA]
colnames(iso2) <- c("iso2","continent","country","state")
fwrite(iso2, file = "iso2.csv")





t <- ggplot(data=state.date.d[quartile %in% c(4,5,6)], aes(x = date, y = diff)) + 
  geom_col(colour = "darkred") + 
  facet_grid(rows = vars(state))

t

ggsave(file = "test.png", dpi = 600, width = 3, height = 12, units = "in", limitsize = FALSE)    








### TRENDLINE FUNCTION
### 

n = number of data points  
slope : 
alpha = n sum of xy - sum x sum y "all over" n sum x squared = (sum of x) squared
offset : 
beta = sum of y = alpha sum x "all over" n
trendline formula : 
  y = alpha x + beta 





ggplot(data = df[country == "US"], aes(x = date, y = diff), fill = type) +
  geom_col() + facet_grid(cols = vars(type))


ggplot(data = df[country == "US"], aes(x = date, y = diff), fill = type) +
  geom_col() + scale_y_log10()












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





p <- ggplot(mtcars, aes(cyl, mpg)) + geom_point()
# Create a simple secondary axis 
p + scale_y_continuous(sec.axis = sec_axis(~ . + 10))
# Inherit the name from the primary axis 
p + scale_y_continuous("Miles/gallon", sec.axis = sec_axis(~ . + 10, name = derive()))
# Duplicate the primary axis 
p + scale_y_continuous(sec.axis = dup_axis())
# You can pass in a formula as a shorthand 
p + scale_y_continuous(sec.axis = ~ .^2)


ggplot(df, aes(x = dx, y = price)) + geom_line() + 
  scale_x_datetime("GMT", date_labels = "%b %d %I %p", sec.axis = sec_axis(~ . + 8 * 3600, name = "GMT+8", labels = scales::time_format("%b %d %I %p")))



jj <-arrangeGrob(j1,j2)


library(grid) 
d <- head(iris, 3) 
g <- tableGrob(d) 
grid.newpage() 
grid.draw(g)



vars <- lapply(names(crimes)[-1], function(j) { 
  data.frame(state = crimes$state, variable = j, 
             value = crimes[[j]]) }) 

crimes_long <- do.call("rbind", vars)

states_map <- map_data("state") 
ggplot(crimes, aes(map_id = state)) + 
  geom_map(aes(fill = Murder), map = states_map) + 
  expand_limits(x = states_map$long, y = states_map$lat)

ggplot(geo.test, aes(map_id = state)) + 
  geom_map(aes(fill = count), map = states_map) + 
  expand_limits(x = states_map$long, y = states_map$lat)


ggplot(crimes_long, aes(map_id = state)) + 
  geom_map(aes(fill = value), map = states_map) + 
  expand_limits(x = states_map$long, y = states_map$lat) + 
  facet_wrap( ~ variable)




world_map <- map_data("world")
ggplot(world_map) +
  geom_map(aes(map_id = world_map), map = world_map, fill="lightgray", colour = "white")


# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy", 
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))


ggplot(some.eu.maps, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = region))+
  geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

















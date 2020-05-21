
library(dplyr,readr)

cdf <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv", header = TRUE)
ddf <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv", header = TRUE)

for (i in 1:nrow(cdf)) {
  cdf[i,"cumulative"] <- sum(cdf[i,5:88])
}
for (i in 1:nrow(ddf)) {
  ddf[i,"cumulative"] <- sum(ddf[i,5:88])
}




st <- readline(prompt = "Which State?")


# SINGLE STATE :: DEATH CHART
single.state <- us.state[state == st]


jj <- ggplot(data = single.state, aes(x = date, y = cum_cases)) +
  geom_col(fill = "darkred") + scale_x_date(limits = c(start.date, end.date)) +
  geom_label(label = paste0(st), 
             x = place.one, y = single.state[,mean(new_cases)],   
             color = "black", fill="#ffeeee", size = 4)
 
jj



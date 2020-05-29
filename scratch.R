



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









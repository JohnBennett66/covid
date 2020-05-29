

ggplot(data = tst, aes(x = date, y = diff)) + 
  geom_col() + 
  facet_grid(cols = vars(state))



ggplot(data_long, aes(type, wert)) + geom_bar(aes(fill = region, alpha = factor(kat)), 
                                              position = "dodge", 
                                              stat = "identity") + 
  scale_alpha_manual(values = c(0.6, 1)) +
  facet_grid(. ~ region) +
  theme_bw() + theme( strip.background  = element_blank(),
                      panel.grid.major = element_line(colour = "grey80"),
                      panel.border = element_blank(),
                      axis.ticks = element_blank(),
                      panel.grid.minor.x=element_blank(),
                      panel.grid.major.x=element_blank() ) +
  theme(legend.position="bottom")



ggplot(data = tot.d.st) + geom_col(aes(x=state, y=agg_deaths))






list.s <- data.table(state.date.d[,unique(state)])
for (j in 1:length(unique(state.date.d[,state]))) {
  sdd.s <- state.date.d[state == list.s[j]]
  list.a <- data.table(mavg=seq(1:nrow(sdd.s)))
  list.a[1:7,mavg:=0]
  for (i in 7:nrow(list.a)) {
    list.a[i] <- sdd.s[(i-6):i,round(mean(diff),digits = 0)]
  }
  state.date.d[state==sdd.s[,unique(state)],basic:=list.a[,mavg]]
}




df <- data.frame( 
  x = rep(c(2, 5, 7, 9, 12), 2), 
  y = rep(c(1, 2), each = 5), 
  z = factor(rep(1:5, each = 2)), 
  w = rep(diff(c(0, 4, 6, 8, 10, 14)), 2) ) 

ggplot(df, aes(x, y)) +
  geom_tile(aes(fill = z), colour = "grey") 

ggplot(df, aes(x, y, width = w)) + geom_tile(aes(fill = z), colour = "grey50") 

ggplot(df, aes(xmin = x - w / 2, xmax = x + w / 2, ymin = y, ymax = y + 1)) + geom_rect(aes(fill = z), colour = "grey50")


ggplot(df[1,], aes(xmin = x, xmax = x + 2, ymin = y, ymax = y + 1)) + geom_rect(aes(fill = z), colour = "grey50")


### TRENDLINE FUNCTION
### 

n = number of data points  
slope : 
alpha = n sum of xy - sum x sum y "all over" n sum x squared = (sum of x) squared
offset : 
beta = sum of y = alpha sum x "all over" n
trendline formula : 
  y = alpha x + beta 









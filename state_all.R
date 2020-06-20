###  ALL STATES CHART #####

### SETUP LOCATION FOR VISUALS
if_else(wd == "C:/Users/Newtboy/Documents/R4FUN/COVID", 
        setwd("C:/Users/Newtboy/Documents/R4FUN/COVID/visual_artifacts"), 
        print("wrong directory"), 
        missing = NULL)

# vars
start.date = ymd(20200229)
end.date = us.state[,max(date)]
# transform -- concat label text
labels <- us.state[,.(state,paste0(us.state[,state]," - ",format(us.state[,cum],big.mark = ",")))]
labels <- unique(labels)
us.state <- us.state[labels, on = .(state = state)]
names <- as.list(colnames(us.state))
names[16] <- "facet_label"
colnames(us.state) <- as.character(names)
# moving averages
# basic
list.a <- data.table(mavg=seq(1:nrow(us.state)))
list.a[1:5,mavg:=0]
for (i in 5:nrow(us.state)) {
  list.a[i] <- us.state[(i-5):i,round(mean(new_deaths),digits = 0)]
}
us.state[,basic:=list.a[,mavg]]

### THE PLOT  ####
all.states <- ggplot(data = us.state, aes(x = date, y = new_deaths)) + 
  geom_col(fill = "darkred") + 
  geom_line(data = us.state, aes(x = date, y = basic), colour = "blue") +
  scale_x_date(limits = c(start.date, end.date)) +
  facet_wrap(vars(factor(facet_label, levels = us.state[,unique(facet_label)])), ncol = 5, scales = "free") 

             
# chart output
all.states  # to screen
filename <- paste0("daily_deaths_allstates_facets_chart.png")
ggsave(file = filename, dpi = 600, width = 10, height = 15, units = "in")  # to image file
pdf("daily_deaths_singlestate_chart.pdf")  # to PDF file
print(dly.d.single.state.chrt)
dev.off()

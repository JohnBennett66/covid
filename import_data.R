###################### ###
### COVID TRACKING    ####
### SETUP SCRIPT       ###
### see processing.R   ###
### for script list    ###
###################### ###



# data.world covid-19 datasource
df.jh <- read_csv("https://query.data.world/s/cgpumxcw4ajvqt6334dwjhay6uhact", 
                  col_names = TRUE, na = c("","N/A"));  
setDT(df.jh)

df.tb <- read.csv("https://query.data.world/s/33aafdu2yb5fx4arlb66xhdawgkav3", 
                  header=TRUE, stringsAsFactors=FALSE);
setDT(df.tb)

df.eu <- read.csv("https://query.data.world/s/feumtwinost7egnxw7ms6advsr6c66", 
                  header=TRUE, stringsAsFactors=FALSE);
setDT(df.eu)

# adjust structure
df.jh[,Date:=mdy(Date)]
# add record id
df.jh[,rec_num := seq(1:nrow(df.jh))]





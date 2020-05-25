

# data.world covid-19 datasource
df <- read_csv("https://query.data.world/s/7f4asihgscqblwxjas2lociayh7odt", col_names = TRUE, col_types = "fiiiccfffffc??iicc", na = c("","N/A"));
setDT(df)
# adjust structure
df[,Date:=mdy(Date)]
# add record id
df[,rec_num := seq(1:nrow(df))]



# # contintent to country mapping
# iso2.cont.map <- read_csv("data/country_continent.csv")
# setDT(iso2.cont.map)
# 
# #contintent code to name mapping
# cont.code.name.map <- unique(iso2.cont.map[,2])
# colnames(cont.code.name.map) <- "cc"
# cont.code.name.map[,2] <- c("--", "Europe", "Asia", NA, "Africa", "North America", "South America", "Oceania")
# colnames(cont.code.name.map) <- c("cc","continent name")
# setDT(cont.code.name.map)


iso2 <- read_csv(file = "iso2.csv", col_names = TRUE, col_types = "ifffff", na = c("","N/A"))
setDT(iso2)
iso2[,1:=NULL]


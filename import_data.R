

# data.world covid-19 datasource
df <- read_csv("https://query.data.world/s/7f4asihgscqblwxjas2lociayh7odt", col_names = TRUE, col_types = "fiiiccfffffc??iicc", na = c("","N/A"));
setDT(df)
# adjust structure
df[,Date:=mdy(Date)]
# add record id
df[,rec_num := seq(1:nrow(df))]



# # contintent to country mapping
# the "iso2.cvs" was manually completed and verified
iso2 <- read_csv(file = "iso2.csv", col_names = TRUE, col_types = "ffff", na = c("","N/A"))
setDT(iso2)



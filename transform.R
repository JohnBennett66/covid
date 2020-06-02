### DATA TRANSFORMATIONS  ####

# keep original, create working file
data <- df
i2 <- iso2

#use 'df' dataframe
####
# str(df)
# 'data.frame':	823914 obs. of  18 variables:
#   $ ï..Case_Type                        : chr  "Confirmed" "Confirmed" "Deaths" "Deaths" ...
# $ People_Total_Tested_Count           : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Cases                               : int  0 23 0 56 0 0 1 2710 0 1 ...
# $ Difference                          : int  0 0 0 0 0 0 0 19 0 0 ...
# $ Date                                : chr  "2/3/2020" "4/21/2020" "3/1/2020" "5/11/2020" ...
# $ Combined_Key                        : chr  "Switzerland" "Antigua and Barbuda" "Cyprus" "Thailand" ...
# $ Country_Region                      : chr  "Switzerland" "Antigua and Barbuda" "Cyprus" "Thailand" ...
# $ Province_State                      : chr  "N/A" "N/A" "N/A" "N/A" ...
# $ Admin2                              : chr  "" "" "" "" ...
# $ iso2                                : chr  "CH" "AG" "CY" "TH" ...
# $ iso3                                : chr  "CHE" "ATG" "CYP" "THA" ...
# $ FIPS                                : int  NA NA NA NA NA NA NA NA NA NA ...
# $ Lat                                 : num  46.8 17.1 35.1 15.9 18.1 ...
# $ Long                                : num  8.23 -61.8 33.43 100.99 -77.3 ...
# $ Population_Count                    : int  8654618 97928 1207361 69799978 2961161 397621 4829764 10423056 26221 4829764 ...
# $ People_Hospitalized_Cumulative_Count: int  NA NA NA NA NA NA NA NA NA NA ...
# $ Data_Source                         : chr  "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE" "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE" "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE" "2019 Novel Coronavirus COVID-19 (2019-nCoV) Data Repository by Johns Hopkins CSSE" ...
# $ Prep_Flow_Runtime                   : chr  "5/17/2020 11:31:55 PM" "5/17/2020 11:31:55 PM" "5/17/2020 11:31:55 PM" "5/17/2020 11:31:55 PM" ...
####
# colnames(df)
# [1] "ï..Case_Type"                         "People_Total_Tested_Count"           
# [3] "Cases"                                "Difference"                          
# [5] "Date"                                 "Combined_Key"                        
# [7] "Country_Region"                       "Province_State"                      
# [9] "Admin2"                               "iso2"                                
# [11] "iso3"                                 "FIPS"                                
# [13] "Lat"                                  "Long"                                
# [15] "Population_Count"                     "People_Hospitalized_Cumulative_Count"
# [17] "Data_Source"                          "Prep_Flow_Runtime"  
##########

### TRANSFORM  ####
#simplify colnames
colnames(df) <- c("type", "tested", "cases", "diff", "date", "key", "country", "state", "city", "iso2", "iso3", "fips", "lat", "long", "pop", "hospital", "source", "runtime", "rec_num")


# fix "Cruise Ship"
df[country=="Cruise Ship",iso2:="CS"]

### CONTINENT CODE --- JOIN 
df <- df[iso2,on=.(country=country,state=state,iso2=iso2)]


### SPLIT DATA - CONFIRMED/DEATHS ####

# split on :: i..Case_Type
# [1] "ï..Case_Type" {Confirmed|Deaths}

# SPLIT CONFIRMED CASES AND DEATHS
cases <- df[type == "Confirmed"]
deaths <- df[type == "Deaths"]
cases[,state := as.character(state)]
deaths[,state := as.character(state)]

# SPLIT OUT US DATA :: CASES AND DEATHS 
cases.us <- cases[iso2 == "US"]
deaths.us <- deaths[iso2 == "US"]

### AGGREGATIONS BY STATE  ####

# aggregate totals by state, totals by day by state

# AGGREGATE BY STATE
us.state.c <- cases.us[,.(cases=sum(cases),tested=sum(tested),hospital=sum(hospital),pop=max(pop)),by = "state"]
us.state.d <- deaths.us[,.(cases=sum(cases),tested=sum(tested),hospital=sum(hospital),pop=max(pop)),by = "state"]
setnames(us.state.d,"cases","deaths")
setorder(us.state.c,state)
setorder(us.state.d,state)

# AGGREGATE BY DATE
us.date.c <- cases.us[,.(cases=sum(cases),tested=sum(tested),hospital=sum(hospital),diff=sum(diff)),by = "date"]
us.date.d <- deaths.us[,.(cases=sum(cases),tested=sum(tested),hospital=sum(hospital),diff=sum(diff)),by = "date"]
setnames(us.date.d,"cases","deaths")
setorder(us.date.c,date)
setorder(us.date.d,date)

us.date.d[is.na(tested),tested:=0]
us.date.d[is.na(hospital),hospital:=0]
# by weekday
us.weekday.d <- us.date.d
us.weekday.d$weekday <- lubridate::wday(as.POSIXlt(lubridate::as_date(us.weekday.d$date)),label=TRUE,abbr=FALSE)
us.weekday.d$week <- lubridate::week(as.POSIXlt(lubridate::as_date(us.weekday.d$date)))


# AGGREGATE BY DATE BY STATE
state.date.c <- cases.us[,.(cases=sum(cases),diff=sum(diff),tested=sum(tested),hospital=sum(hospital),pop=max(pop)),by = c("date","state")]
state.date.d <- deaths.us[,.(deaths=sum(cases),diff=sum(diff),tested=sum(tested),hospital=sum(hospital),pop=max(pop)),by = c("date","state")]
setorder(state.date.c,state, date)
setorder(state.date.d,state, date)










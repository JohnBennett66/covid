### DATA TRANSFORMATIONS  ####

###  US STATES  ####
#simplify colnames
colnames(df.jh) <- c("state", "date", "pop", "cum_cases", "cum_cases_1000", "cum_deaths", 
                     "cum_deaths_1000", "new_cases", "new_cases_roll_7", "new_deaths", 
                     "new_deaths_roll_7", "new_deaths_1000", "new_cases_1000")

# rename
us.state <- df.jh

# change NA to zero
us.state[is.na(new_deaths_roll_7), new_deaths_roll_7 := 0]
us.state[is.na(new_deaths_1000), new_deaths_1000 := 0]
us.state[is.na(new_deaths), new_deaths := 0]
us.state[is.na(new_cases_1000), new_cases_1000 := 0]
us.state[is.na(new_cases_roll_7), new_cases_roll_7 := 0]
us.state[is.na(new_cases), new_cases := 0]

us.state[,case_toll := max(cum_cases), by = state]
us.state[,death_toll := max(cum_deaths), by = state]

setorder(us.state, -death_toll, date)

us.state[,cases_per_capita := (cum_cases/pop) * 100]
us.state[,deaths_per_capita := (cum_deaths/pop) * 100]





###  US ALL BY DATE  ####
us.date <- df.jh[
                  ,.(pop = sum(pop),cum_cases = sum(cum_cases), cum_cases_1000 = sum(cum_cases_1000), 
                    cum_deaths = sum(cum_deaths), cum_deaths_1000 = sum(cum_deaths_1000), 
                    new_cases = sum(new_cases), new_cases_roll_7 = sum(new_cases_roll_7), 
                    new_deaths = sum(new_deaths), new_deaths_roll_7 = sum(new_deaths_roll_7), 
                    new_deaths_1000 = sum(new_deaths_1000), new_cases_1000 = sum(new_cases_1000))
                  , by = .(date)]


us.date[,case_toll := max(cum_cases)]
us.date[,death_toll := max(cum_deaths)]

setorder(us.date, -death_toll, date)

us.date[,cases_per_capita := (cum_cases/pop)]
us.date[,deaths_per_capita := (cum_deaths/pop)]

us.date[,cases_per_mill := (cum_cases/(pop/1000000))]
us.date[,deaths_per_mill := (cum_deaths/(pop/1000000))]

us.date[,cases_per_10mill := (cum_cases/(pop/10000000))]
us.date[,deaths_per_10mill := (cum_deaths/(pop/10000000))]

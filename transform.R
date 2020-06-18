### DATA TRANSFORMATIONS  ####

###  US STATES  ####
#simplify colnames
colnames(df.jh) <- c("state", "date", "pop", "cum_cases", "cum_cases_1000", "cum_deaths", 
                     "cum_deaths_1000", "new_cases", "new_cases_roll_7", "new_deaths", 
                     "new_deaths_roll_7", "new_deaths_1000", "new_cases_1000")

# rename
us.state <- df.jh





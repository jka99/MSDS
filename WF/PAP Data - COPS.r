library(tidyverse)
library(readxl)

# Read in old values
# no idea why this is necessary, but...
oldvalues = read_excel("P/jyoung/Accreditation/SPOC_SPAC/COPS Data/PAP Data.csv")

# Insert months names for the previous Quarter
# Q1 = c("January", "February", "March")
# Q2 = c("April", "May", "June")
# Q3 = c("July", "August", "September")
# Q4 = c("October", "November", "December")
newmonths = c("October", "November", "December")

column_names = c("Medication", "NDC", "Qty Dispensed", "U & U Price", "Unit Price", "Total") # NDC is variable currently

# These are the paths for reading in the new data
MCPath22 = "P/Medication Access Center/PAP-(Patient Assistance program)/PAP Values/2022 PAP Values.xlsx"
DHPPath22 = "P/Medication Access Center/PAP-DHP/DHP-PAP Values/2022 DHP-PAP Values.xlsx"
LexPath22 = "P/Medication Access Center/Lexington/PAP-Lexington/2022 Lexington PAP Values.xlsx"

# this will read in the data for the months of the last quarter
# each year the `year` column in each function must be updated
# *** will fix this programatically later
MClist22 = lapply(newmonths, function(x) {
    dat = read_excel(MCPath22, sheet = x, skip = 1)
    names(dat) = column_names
    dat$Month = x
    dat$Year = 2022
    dat$Location = "Winston"
    dat = select(dat, Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)
    return(dat)
})

DHPlis22 = lapply(newmonths, function(x) {
    dat = read_excel(DHPPath22, sheet = x, skip = 1)
    names(dat) = column_names
    dat$Month = x
    dat$Year = 2022
    dat$Location = "DHP"
    dat = select(dat, Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)
    return(dat)
})

Lexlist22 = lapply(newmonths, function(x) {
    dat = read_excel(LexPath22, sheet = x, skip = 1)
    names(dat) = column_names
    dat$Month = x
    dat$Year = 2022
    dat$Location = "Lexington"
    dat = select(dat, Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)
    return(dat)
})

MC_newdata = do.call(rbind, MClist22) %>% 
    select(Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)
DHP_newdata = do.call(rbind, DHPlis22) # %>% 
    # select(Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)
Lex_newdata = do.call(rbind, Lexlist22) # %>%
    # select(Medication, NDC, `Qty Dispensed`, `U & U Price`, `Unit Price`, Total, Month, Year, Location)

data = rbind(MC_newdata, DHP_newdata, Lex_newdata, oldvalues) %>% 
    filter(!is.na(Medication))

write.csv(data, "P/jyoung/Accreditation/SPOC_SPAC/COPS Data/PAP Data.csv")
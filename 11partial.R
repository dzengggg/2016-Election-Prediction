census.del <- census

# removes rows with missing data
census.del <- census.del[complete.cases(census.del),]

# converts {`Men`, `Employed`, `Citizen`} to percentages
census.del <- census.del %>% 
  mutate(Men = 100*Men/TotalPop, 
         Employed = 100*Employed/TotalPop,
         Citizen = 100*Citizen/TotalPop)

# Combines {Hispanic, Black, Native, Asian, Pacific} into `Minority` attribute and removes them.
census.del <- census.del %>% mutate(Minority = Hispanic + Black + Native + Asian + Pacific)%>% select(-Hispanic, -Black, -Native, -Asian, -Pacific)

# Moves Minority attribute  to a more ergonomic index
census.del <- census.del[c(1:7, ncol(census.del), 8:(ncol(census.del)-1))]

#Removes {`Walk`, `PublicWork`, `Construction`} attributes
census.del <- select(census.del, -Walk, -PublicWork, -Construction)

# Removes "redundant" variables
census.del <- census.del %>% select(-Women,-White)



# Creates census sub-county variable and groups tibble by
# State and County
census.subct <- group_by(census.del,State, County)

# Tallys how many subcounties are in each county of each state and
# names the column of tallies "CountyTotal"
census.subct <- add_tally(census.subct)
names(census.subct)[ncol(census.subct)] <- "CountyTotal"

# Finds weight for each subcounty defines as Population size with
# respect to how many subcounties are in the county.
census.subct <- mutate(census.subct, CountyWeight = TotalPop/CountyTotal)
census.subct
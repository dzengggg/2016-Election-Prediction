census = read.csv("~/data/census/census.csv") %>% as.tbl
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


census.ct <- census.subct

# Creates a total weight of the county weight for averaging
CountyWeightSum <- summarise_at(census.ct, .funs = funs(sum), .vars = vars("CountyWeight"))

# Renames County Weight Total for easier reading
names(CountyWeightSum)[ncol(CountyWeightSum)] <- "CountyWeightSum"

#Attaches CountyWeightSum variable to census.ct
census.ct <- left_join(census.ct,CountyWeightSum , by = c("State", "County"))

# Revalues CountyWeight to reflect its percentage of county total weight
census.ct <- mutate(census.ct, CountyWeight = CountyWeight/CountyWeightSum)

# Removes Unnecessary Variables
# information is already found in CountyWeight
census.ct <- select(census.ct, -CountyWeightSum, - CountyTotal)

# Applies Weightes to SubCounty Data
census.ct[5:28] <- census.ct[5:28]*census.ct$CountyWeight

# Aggregates population weighted subcounty data into County data
census.ct_temp <- census.ct %>% summarise_all(sum)

# Removes CountyWeight variable: it's no longer necessary
census.ct <- select(census.ct, -CountyWeight)


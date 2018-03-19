election.raw = read.csv("C:/Users/hillb/OneDrive/Desktop/PSTAT 131 HW/final-project/data/election/election.csv") %>% as.tbl
census_meta = read.csv("C:/Users/hillb/OneDrive/Desktop/PSTAT 131 HW/final-project/data/census/metadata.csv", sep = ";") %>% as.tbl
census = read.csv("C:/Users/hillb/OneDrive/Desktop/PSTAT 131 HW/final-project/data/census/census.csv") %>% as.tbl
census$CensusTract = as.factor(census$CensusTract)

#4.
election_federal <- filter(election.raw, fips == "US")
election_state <- filter(election.raw, fips != "US" & is.na(county)) 
election <- filter(election.raw, !is.na(county))
#Adding missing county observations to election data frame
election <- rbind(election, election_state[309:312,])
#Taking out bad observations from state data frame
election_state <- filter(election_state, fips != 46102 & fips != 2000)

adjusted.county <- election %>% spread(key = candidate, value = votes)
adjusted.county[is.na(adjusted.county)] <- 0 
adjusted.county <- adjusted.county %>% mutate(total = rowSums(adjusted.county[,4:35]))

county.group <- group_by(election, fips)
total.group <- summarize(county.group, total = sum(votes))


       
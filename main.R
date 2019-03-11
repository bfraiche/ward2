library(tidyverse)
library(lubridate)

### LOAD DATA FROM FILE ----
csv_files <- as.array(list.files(pattern = "\\.csv$"))

for(value in csv_files) {
  name_i <- paste(sub(pattern = "(.*)\\..*$", replacement = "\\1", value))
  csv_i <- read_csv(value)
  assign(name_i, csv_i)
}

rm(name_i, csv_i)

assign("pri08", readxl::read_xls(list.files(pattern = "\\.xls$")))

elec_smry <- tribble(
           ~Election, ~gen18, ~pri18, ~pri16, ~pri12, ~pri08,
        "Jack Evans",      0L,      0L,      1L,      1L,      1L,
     "Initiative 77",      0L,      1L,      0L,      0L,      0L,
  "Elissa Silverman",      1L,      0L,      0L,      0L,      0L,
      "Robert White",      0L,      0L,      1L,      0L,      0L,
       "Anita Bonds",      1L,      0L,      0L,      0L,      0L
  )

### BUILD DATA MODEL ----
## At Large Council + Initiative 77 '18
elec.18 <- rbind(pri18, gen18) %>%
  filter(
    (ContestNumber == 304 & ElectionName == 'General Election') | (ContestNumber == 384 & ElectionName == 'Primary Election')
  ) %>% 
  mutate(
    ElectionYear = ElectionDate %>% mdy_hms %>% year
  ) %>%
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber,
    Candidate, 
    Party
  ) %>% 
  summarise(
    Votes = sum(Votes)
  )

## At Large Council '16 + Ward 2 '12, '16
elec.16.12 <- rbind(pri16, pri12) %>% 
  filter(
    (CONTEST_ID == 115 & ELECTION_DATE == '4/3/2012') | ((CONTEST_ID == 303 | CONTEST_ID == 304) & ELECTION_DATE == '6/14/2016')
  ) %>%
  select(
    ELECTION_DATE,
    ElectionName = ELECTION_NAME,
    ContestName = CONTEST_NAME,
    WardNumber = WARD,
    PrecinctNumber = PRECINCT_NUMBER,
    Candidate = CANDIDATE, 
    Party = PARTY,
    Votes = VOTES
  ) %>% 
  mutate(
    ElectionYear = ELECTION_DATE %>% mdy %>% year
  ) %>%
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber,
    Candidate, 
    Party
  ) %>% 
  summarise(
    Votes = sum(Votes)
  )

## Ward 2 '08
elec.08 <- pri08 %>% 
  filter(contest_id==9) %>%
  select(
    ContestName = CONTEST_FULL_NAME,
    PrecinctNumber = PRECINCT_ID,
    Candidate = CANDIDATE_FULL_NAME,
    Votes = TOTAL
  ) %>% 
  mutate(
    ElectionYear = 2008,
    ElectionName = 'Primary Election',
    WardNumber = 2,
    Party = 'DEM'
  ) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber,
    Candidate, 
    Party
  ) %>% 
  summarise(
    Votes = sum(Votes)
  )

elec.all <- rbind(elec.18, elec.16.12, elec.08)

### AGGREGATE RESULTS
by_precinct <- elec.all %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName
  ) %>% 
  mutate(
    CityTotalVotes = sum(Votes)
  ) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    Candidate
  ) %>% 
  mutate(
    CityPercent = sum(Votes) / first(CityTotalVotes)
  ) %>% 
  filter(WardNumber==2) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber
  ) %>% 
  mutate(
    WardTotalVotes = sum(Votes)
  ) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    Candidate
  ) %>% 
  mutate(
    WardPercent = sum(Votes) / first(WardTotalVotes)
  ) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber
  ) %>% 
  mutate(
    PrecinctTotalVotes = sum(Votes)
  ) %>% 
  group_by(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber,
    Candidate
  ) %>% 
  mutate(
    PrecinctPercent = sum(Votes) / first(PrecinctTotalVotes),
    WardPerformance = PrecinctPercent - WardPercent,
    CityPerformance = PrecinctPercent - CityPercent
  ) %>% 
  ungroup() %>% 
  select(
    ElectionYear,
    ElectionName,
    ContestName,
    WardNumber,
    PrecinctNumber,
    Candidate, 
    Party,
    Votes,
    PrecinctPercent,
    WardPerformance,
    CityPerformance
  )

## FILTER RESULTS
by_precinct <- by_precinct %>% 
  filter(
    !(PrecinctNumber %in% c(18,21)), 
    Candidate %in% c('Anita Bonds', 'Jack Evans', 'Elissa Silverman', 'Robert White', 'YES, to approve')
  )

### ANALYZE DATA ----
by_precinct %>% 
  distinct(ElectionYear, ElectionName, ContestName, Candidate) %>% 
  arrange(ElectionYear, ElectionName, ContestName)

by_precinct %>% 
  mutate(
    WardPerformance = if_else(Candidate %in% c('Jack Evans', 'Anita Bonds'), -WardPerformance, WardPerformance),
    CityPerformance = if_else(Candidate %in% c('Jack Evans', 'Anita Bonds'), -CityPerformance, CityPerformance)
  ) %>% 
  group_by(PrecinctNumber) %>% 
  summarise(
    WardPerformance = mean(WardPerformance),
    CityPerformance = mean(CityPerformance)
  )

# # Votes
# by_precinct %>% 
#   ggplot(aes(Votes, fill=Candidate)) +
#   geom_histogram() +
#   facet_wrap( ~ PrecinctNumber) + 
#   ggtitle("Votes") +  
#   xlab("# of Votes") + 
#   ylab("")
# 
# # PrecinctPercent
# by_precinct %>% 
#   ggplot(aes(PrecinctPercent, fill=Candidate)) +
#   geom_histogram() +
#   facet_wrap( ~ PrecinctNumber) + 
#   ggtitle("Percentage Vote") +  
#   xlab("") + 
#   ylab("")
# 
# # WardPerformance
# by_precinct %>% 
#   ggplot(aes(WardPerformance, fill=Candidate)) +
#   geom_histogram() +
#   facet_wrap( ~ PrecinctNumber) + 
#   ggtitle("Performance v. Ward") +  
#   xlab("% Performance v. Ward") + 
#   ylab("")
# 
# # CityPerformance
# by_precinct %>% 
#   ggplot(aes(CityPerformance, fill=Candidate)) +
#   geom_histogram() +
#   facet_wrap( ~ PrecinctNumber) + 
#   ggtitle("Performance v. City") +  
#   xlab("% Performance v. City") + 
#   ylab("")
# 
# # save(by_precinct, file = 'ward2_by_precinct.RData')

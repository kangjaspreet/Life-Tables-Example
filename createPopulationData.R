# 0 Script Documentation ------------------------------------------------------------------------------------------------------------------------------

# Authors: Michael Samuel, DrPH, Jaspreet Kang (Fusion Center/Office of Policy and Planning, CDPH)
# Contact information for any questions: ccb@cdph.ca.gov

# About this script:
# This script produces the necessary state, county, and MSSA-level population data which are then used in "ltmaker.R" to calculate life tables.
# To execute this file successfully, open "Life Tables.Rproj" so the working directory is set to the appropriate path.

# Script dependencies:
# R packages:
# - pacman
# - dplyr
# - tidyr
# - readxl
# - readr
# - stringr

# Inputs:
# ageLink.xlsx - standard linkage file for mapping age group names to age group lower and upper limits
# raceLink.xlsx - standard linkage file, used here for linking DOF's race codes to CCB race codes/names
# countyLink.xlsx - standard linkage file which links county names to county fips
# Intercensal_2000-2010_DBInput.csv - DOF's 2000-2009 intercensal complete county x race x age x sex population estimates downloaded from their website
# P3_Complete.csv - DOF's 2010- Complete County x Race x Age x Sex population projections downloaded from their website
# nxMSSA.RDS - Last year's MSSA-level population data file

# Outputs:
# 1. nxCounty.RDS - County-level population data - Contains every year(2000-)-county-sex(including total)-race(including total)-ageGroup combination
# 2. nxState.RDS - State-level population data - Contains every year(2000-)-state-sex(including total)-race(including total)-ageGroup combination
# 3. nxMSSA.RDS - MSSA-level population data - Contains every year(2007-)-MSSA-sex-race(including total)-ageGroup combination

# Script structure:
# 1. Setup - Load required R packages, set global constants, source standard and geography linkage files
# 2. Read in and prepare state, county-level population data
# 3. Read in and prepare MSSA-level population data
# 4. Save data

# Notes:
# - State and county population data are from the California Department Department of Finance (DOF). 
#   - 2000-2009 data are California and Counties Population by Age, Race/Hispanics, and Gender: 2000-2010 - https://dof.ca.gov/forecasting/Demographics/estimates/
#   - 2010- data are Complete State and County Projections (Table P-3) - https://dof.ca.gov/forecasting/demographics/Projections/
# - MSSA population data are aggregations of Census Tract pop data pulled from ACS 5-year surveys, using Table B01001 - https://data.census.gov/cedsci/table?q=b01001&tid=ACSDT5Y2019.B01001
#   - Medical Service Study Areas (MSSAs) is a unique California geographic designation based on aggregation of census tracts, constructed by the California Health Care Access and Information (HCAI)
#   - There are 542 MSSAs in California
#   - 2010 Census Tract boundaries are used for all years; Therefore, 2019 ACS 5-Year estimates are used for 2020-



# 1 Setup -----------------------------------------------------------------------------------------------------------------------------------

## 1.1 Load packages -------------------------------------

# install.packages("pacman") # Uncomment line if pacman is not installed on your system
pacman::p_load("readxl", "dplyr", "tidyr", "readr", "stringr")


## 1.2 Set global constant - most recent year -------------

myYear <- 2021

## 1.3 Source standards, read in linkage files -------------

# County Fips to county names
countyLink <- readxl::read_xlsx("dependencies/countyLink.xlsx") %>%
  select(countyName, CountyCode = cdphcaCountyTxt,  FIPSCounty) %>%
  mutate(fips = as.numeric(paste0("6", FIPSCounty)))

# Life Expectancy age groups
ageLink    <- read_excel("dependencies/ageLink.xlsx", sheet = "ageLE") %>% select(-ageCode) %>% rename(agell = lAge, ageul = uAge) # Life expextancy age groups
ageBreaks  <- c(-1, ageLink$ageul) # Breakpoints for grouping age
ageLabels  <- ageLink$ageName # Labels for age groups

# DOF race codes (race7) to CCB race codes/names (raceCode)
raceLink <- read_excel("dependencies/raceLink.xlsx") %>% 
  filter(!is.na(race7)) %>%
  select(raceCode, race7)

# 2 Read in and process state, county population data ------------------------------------------------------

## 2.1 Read in and process 2000- DOF's county-level population data -----------------

dof_pop_2000_2009 <- read_csv("dependencies/Intercensal_2000-2010_DBInput.csv")%>%
  mutate(year  = as.numeric(str_sub(Year,5,9)),
         month = as.numeric(str_sub(Year,1,1))) %>%    # 2000 has both April and July estimates; 2010 only April; all others only July
  filter(month == 7)  %>%
  select(CountyCode, year, sex=Gender, race7=RaceCode, age=Age, population=Population) %>% # CountyCode - 2 digit character
  full_join(countyLink, by="CountyCode") %>%    # "01", "02" ... "58", "59"
  filter(CountyCode != "59") %>% select(-CountyCode, -FIPSCounty, -countyName) %>%  #  59 - California
  mutate(race7 = ifelse(race7== 6,99,race7))  %>%                                           
  mutate(race7 = ifelse(race7== 7, 6,race7))  %>% # Recode race code 7 (multi-race) to race code 6
  mutate(race7 = ifelse(race7==99, 7,race7)) # Recode race code 6 (hispanic) to race code 7
# Note about race codes:
# DOF's race codes in their Intercensal 2000-2009 data are slightly different than their current race code system.
# The last 3 "mutates" above re-codes the race codes so that it is consistent with DOF's current race codes


# from: https://www.dof.ca.gov/forecasting/demographics/Projections/  - P3.complete
dof_pop_2010_myYear <- read_csv("dependencies/P3_Complete_2010-2023.csv") %>%
  filter(year <= myYear) %>%
  select(fips, year, sex, race7, age= agerc, population=perwt)   %>%     # fips - 4 digit character 
  mutate(sex = str_to_title(sex))

# Bind DOF 2000-2009 and 2010-recentYear population data
dof_pop_2000_myYear <- bind_rows(dof_pop_2000_2009,dof_pop_2010_myYear)



## 2.2 Process further state, county-level population  ----------

popCounty <- dof_pop_2000_myYear %>%
  rename(GEOID = fips) %>% 
  mutate(ageName = cut(age, breaks = ageBreaks, labels = ageLabels, right = TRUE), # Cut age into age Groups for life table calculation
         GEOID =  paste0("0", GEOID, "000000")) %>%
  full_join(raceLink, by = "race7") %>%
  full_join(ageLink, by = "ageName") %>% 
  bind_rows( # Add Californi population
    mutate(., GEOID = "06000000000")
  ) %>% 
  bind_rows( # Add total sex
    mutate(., sex = "Total")
  ) %>% 
  bind_rows( # Add total race
    mutate(., raceCode = "Total")
  ) %>% 
  group_by(year, GEOID, sex, agell, ageul, raceCode) %>% 
  summarise(Nx = sum(population))

## 2.3 Finalize county-level population data ------------
nxCounty <- popCounty %>%
  filter(GEOID != "06000000000")

# Data Quality check: Ensure all combinations exist

dqCheck <- c(table(nxCounty$year, nxCounty$sex, nxCounty$agell, nxCounty$raceCode) == 58)

print(paste0("Data Quality Check: There are population data for all 58 counties per year-sex-ageGroup-race combination: ", all(dqCheck)))

## 2.4 Finalize state-level population data ---------------
nxState <- popCounty %>%
  filter(GEOID == "06000000000")


# 3 Read in and process MSSA population data ------------------------------------------------------

# Notes about nxMSSA.RDS:
# 1. 2007-2017 MSSA Pop pulled by Ethan
# 2. 2018-2019 pulled by CCB Data Team via the tidycensus package
# 3. 2020+ data are actually 2019 ACS 5Y estimates since we have not yet switched to the new 2020 census tract boundaries
# - Because of this, the code below reads in nxMSSA.RDS and uses 2019 ACS 5Y estimates for 2020, 2021

# Read in MSSA-level county data
nxMSSA <- readRDS("dataIn/nxMSSA.RDS")

# Check if 2020 and 2021 are in nxMSSA. If not, use 2019 ACS 5Y estimates for 2020, 2021
checkYears <- 2020:myYear
for (checkYear in checkYears) {
  
  if ( checkYear %in% unique( nxMSSA$year ) ) {
    next()
  } else {
    t_nxMSSA <- nxMSSA %>% 
      filter(year == 2019) %>% # use 2019 ACS 5Y estimates
      mutate(year = checkYear)
    
    nxMSSA <- bind_rows(nxMSSA, t_nxMSSA)
  }
}

# Data Quality check - Frequency table on year - All frequencies should be the same
dqCheck <- table(nxMSSA$year, useNA = "ifany")
dqCheck 

# 4 Save data ------------------------------------------------------------------------------------

saveRDS(nxCounty, "dataIn/nxCounty.RDS")
saveRDS(nxState, "dataIn/nxState.RDS")
saveRDS(nxMSSA, "dataIn/nxMSSA.RDS")





library(tidyverse)
library(readxl)
library(jsonlite)
setwd("C:/Users/loren/Documents/GitHub/equals_ict_africa")

### Preparation ####
# Import country groups
odw_master_codes <- read_csv("Input/2021 ODW Country and Region Codes.csv") %>%
  # Clean all variable names by converting to snake case
  janitor::clean_names() %>% 
  # Clear out extra lines at the bottom that just contain notes
  filter(!is.na(country_name)) %>%
  # Clear out duplicate Faroe Islands
  distinct(iso_3_code, .keep_all = TRUE) %>%
  # Keep only relevant indicators and rename for clarity 
  select(iso3c = iso_3_code, country = country_name, odw_region_name, un_code = un_m49_code,
         incgroup = world_bank_income_group_name, lending_cat = world_bank_lending_code_july_2020,
         wbregion = world_bank_all_income_region_names) %>%
  mutate(un_code = as.numeric(un_code), 
         incgroup = fct_relevel(incgroup, "Low income", "Lower middle income", "Upper middle income", "High income"))

### Individuals using the internet ####
# ITU correspondence with Martin Schaaper
# This file has just been published here https://www.itu.int/en/ITU-D/Statistics/Pages/stat/default.aspx
# As of AUgust 20, 2020
int_use <- read_excel("Input/10. Individuals using the Internet by gender and urban-rural location.xls", skip = 2) %>%
  select(country = `Economy name`, year, total = Individuals, male = Male...6,
         female = Female...8) %>%
  mutate_at(c("total", "male", "female"), as.numeric) %>%
  # Filter out remaining observations at bottom of table, do not contain
  # country information
  filter(!is.na(year)) %>%
  # Add country codes
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ iso3c
         )) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  select(country = country.y, iso3c, year, total, male, female, incgroup, wbregion) %>%
  # Convert to long
  pivot_longer(-c(country, iso3c, year, incgroup, wbregion), names_to = "sex", values_to = "value") %>%
  # Only keep non-missing obs and after 2010
  filter(!is.na(value), year >= 2010) %>%
  # Calculate number of obs, will only be 1, but in case we get time-series later.
  group_by(iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(series = "Individuals using the Internet", indicator = series,
         source = "ITU correspondence", category = "Access")

### SWITCHING TO OWNING A MOBILE PHONE
#### Individuals using a mobile phone ####
## ITU correspondence with Martin Schaaper
#mob_use <- read_excel("Data/Input Data/ICT data/31. Individuals using a mobile cellular telephone by gender and urban-rural location.xlsx", skip = 2) %>%
#  select(country = `Economy name`, year, total = `All individuals`, male = Male...6,
#         female = Female...8) %>%
#  mutate_at(c("total", "male", "female"), as.numeric) %>%
#  # Filter out remaining observations at bottom of table, do not contain
#  # country information
#  filter(!is.na(year)) %>%
#  # Add country codes
#  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
#         iso3c = case_when(
#           country == "Kosovo" ~ "XKX",
#           TRUE ~ iso3c
#         )) %>%
#  # Merge in countries on iso3c and keep all observations
#  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
#  # Filter to just African countries, sub-saharan plus six north africa.
#  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
#  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
#  select(country = country.y, iso3c, year, total, male, female, incgroup, wbregion) %>%
#  # Convert to long
#  pivot_longer(-c(country, iso3c, year, incgroup, wbregion), names_to = "sex", values_to = "value") %>%
#  # Only keep non-missing obs and after 2010
#  filter(!is.na(value), year >= 2010) %>%
#  # Calculate number of obs, will only be 1, but in case we get time-series later.
#  group_by(iso3c, sex) %>%
#  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
#  ungroup() %>%
#  mutate(series = "Individuals using a Mobile Phone", indicator = series,
#         source = "ITU correspondence", category = "Access")

#### Individuals owning a mobile phone ####

# Time series SDG Database API data. As of 19 November, 2020,
# Using Q4 version of 2020 SDG database https://unstats.un.org/sdgs/indicators/database/archive
# Using static download of "Global SDG Indicators Database on 05 April 2021"
# When replicating this, filter for series "IT_MOB_OWN" or SDG indicator 5.b.1

# For live API download, use the following code:
## Import number of observations, need to supply to API call to get all
#tot_elements <- fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=IT_MOB_OWN")$totalElements
#
## Use number of observations to import right dimensions of dataframe
#mob_own_time_raw <- fromJSON(str_c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=IT_MOB_OWN&pageSize=", as.character(tot_elements)), flatten = TRUE)$data %>%
# Insert mob_own_time_raw instead of read_csv() call to static file
mob_own_time <- read_csv("Input/sdg_database_ind_own_mobile.csv") %>%
  mutate(iso3c = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
         iso3c = case_when(
           GeoAreaName == "Kosovo" ~ "XKX",
           TRUE ~ iso3c
         ),
         value = as.numeric(Value),
         Sex = case_when(
           Sex == "BOTHSEX" ~ "total",
           TRUE ~ tolower(Sex)
         )) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  select(country, iso3c, year = TimePeriod, sex = Sex, value, incgroup, wbregion) %>%
  # Only keep non-missing obs and after 2010
  filter(!is.na(value), year >= 2010) %>%
  # Calculate number of obs.
  group_by(iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(series = "Individuals owning a Mobile Phone", indicator = series,
         source = "SDG Database", category = "Access")


#### ICT Literacy & Skills ####

# Time series SDG Database API data. As of 19 November, 2020,
# Using Q4 version of 2020 SDG database https://unstats.un.org/sdgs/indicators/database/archive
# Using static download of "Global SDG Indicators Database on 05 April 2021"
# When replicating this, filter for SDG indicator 4.4.1

# For live API download, use the following code:

# # Import number of observations, need to supply to API call to get all
# tot_elements <- fromJSON("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SE_ADT_ACTS")$totalElements
# 
# # Use number of observations to import right dimensions of dataframe and clean up
# literacy_time_raw <- fromJSON(str_c("https://unstats.un.org/SDGAPI/v1/sdg/Series/Data?seriesCode=SE_ADT_ACTS&pageSize=", as.character(tot_elements)), flatten = TRUE)$data %>%
# Insert mob_own_time_raw instead of read_csv() call to static file
literacy_time <- read_csv("Input/sdg_database_ict_literacy.csv") %>%
  mutate(iso3c = countrycode::countrycode(GeoAreaName, "country.name", "iso3c"),
         iso3c = case_when(
           GeoAreaName == "Kosovo" ~ "XKX",
           TRUE ~ iso3c
         ),
         value = as.numeric(Value),
         Sex = case_when(
           Sex == "BOTHSEX" ~ "total",
           TRUE ~ tolower(Sex)
         ),
         `Type of skill` = case_when(
           `Type of skill` == "ARSP"	~ "Using basic arithmetic formula in a spreadsheet",
           `Type of skill` == "CMFL"	~ "Copying or moving a file or folder",
           `Type of skill` == "COPA"	~ "Using copy and paste tools to duplicate or move information within a document",
           `Type of skill` == "EMAIL" ~ "Sending e-mails with attached files",
           `Type of skill` == "EPRS"	~ "Creating electronic presentations with presentation software",
           `Type of skill` == "INST"	~ "Connecting and installing new devices",
           `Type of skill` == "PCPR"	~ "Writing a computer program using a specialized programming language",
           `Type of skill` == "SOFT"	~ "Finding, downloading, installing and configuring software",
           `Type of skill` == "TRAF"	~ "Transferring files between a computer and other devices",
           TRUE ~ `Type of skill`
         )) %>%
  select(iso3c, year = TimePeriod, sex = Sex, value, series = `Type of skill`) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  mutate(indicator = "ITU ICT skills and literacy",
         source = "SDG Database", category = "Skills") %>%
  # Only keep non-missing obs and after 2010
  filter(!is.na(value), year >= 2010) %>%
  # Calculate number of obs, will only be 1, but in case we get time-series later.
  group_by(series, iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  ungroup()

### Made and Received Digital Payments ####
# From World Bank Findex. Acquired through Databank
dig_payment <- read_csv("Input/made_or_received_digitalpayments.csv") %>%
  janitor::clean_names() %>%
  filter(!is.na(country_code)) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("country_code" = "iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | country_code %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  select(country, iso3c = country_code, incgroup, wbregion, starts_with("made_or_received")) %>%
  pivot_longer(-c(country, iso3c, incgroup, wbregion), names_to = "series", values_to = "value") %>%
  mutate(sex = case_when(
    str_detect(series, "_female") ~ "female",
    str_detect(series, "_male") ~ "male",
    TRUE ~ "total"
  ),
  value = as.numeric(value),
  year = case_when(
    !is.na(value) ~ as.numeric(str_extract(series, "(?<=yr)[0-9]{4}")),
    TRUE ~ NA_real_
  )) %>%
  # Keep only real values later than 2010
  filter(!is.na(value), year >= 2010) %>%
  # Keep latest year of observation, grouping by country and sex obs
  group_by(iso3c, sex) %>%
  # Compute number of non_missing observations
  mutate(non_missing_obs = sum(!is.na(value))) %>%
  # Filter max year of non-missing for countries where there's some data
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(series = "Made or Received Digital Payments in the last year",
         indicator = series, source = "World Bank FINDEX", category = "Access")

### Female share of Engineering and Tech Researchers ####
# From UNESCO UIS
# Researchers by field of R&D and sex (FTE and HC)
# Using only Full-time equivalent (FTE)
# June 2020 update
# Science, Technology, and Innovation --> Research and Experimental development
# --> Human resources in research and development (R&D) --> Researchers
# Select series Researchers (FTE) - Engineering and Technology
# Select series Researchers (FTE) - Female - Engineering and Technology
# Select series Female researchers as a percentage of total researchers (FTE) - Engineering and Technology
# Use the first two series to create a percentage of male. Then treat
# Level value for number of researchers in total as datapoint for "total" sex
# Values for Female and Male will be percentages!
researchers <- read_csv("Input/female_eng_tech_researchers.csv") %>%
  janitor::clean_names() %>%
  # make wide so we can create male share of FTE researchers in engineering in engineering and tech
  pivot_wider(c(location, country, time), names_from = "indicator", values_from = value) %>%
  # Create male share
  mutate(FRESP_MS_TFTE_ENGTECH = ((`20402` - `20462`)/`20402`)*100) %>%
  select(iso3c = location, year = time, FRESP_TT_TFTE_ENGTECH = `20402`,
         FRESP_MS_TFTE_ENGTECH, FRESP_FS_TFTE_ENGTECH) %>%
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  pivot_longer(-c(iso3c, country, year, incgroup, wbregion), names_to = "placeholder", values_to = "value") %>%
  mutate(sex = case_when(
    str_detect(placeholder, "_MS_") ~ "male",
    str_detect(placeholder, "_FS_") ~ "female",
    TRUE ~ "total"
  )) %>%
  # Want to only look at trends past 2010
  filter(!is.na(value), year>=2010) %>%
  group_by(iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(series = "Proportiong of engineering & technology researchers",
  indicator = series, source = "UNESCO UIS", category = "Leadership") %>%
  select(-placeholder)
  
### Female graduates from STEM/ICT programmes in tertiary education ####
# From UNESCO UIS
# Distribution of tertiary graduates by field of study
# ONLY HAVE DATA FROM JUNE 2020 UPDATE AFTER 2014 BECAUSE
# SEPTEMBER 2020 DATA UPDATE DOES NOT INCLUDE SEX-DISAGGREGATED DATA
# KEEP CHECKING IF FEMALE AND MALE DISAGGREGATION SHOWS UP
# As of September 2020, this is how to access series
# National Monitoring --> National Monitoring --> Percentage of graduates
# by field of education (tertiary education)
# As of June 2020, this was how to access series
# Education --> Education --> Completion --> Distribution of tertiary graduates
educ <- read_csv("Input/tertiary_graduates_stem_ict.csv") %>%
  janitor::clean_names() %>%
  pivot_wider(c(location, country, time), names_from = edulit_ind, values_from = value) %>%
  select(iso3c = location, year = time, starts_with("FOS")) %>%
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  pivot_longer(-c(country, iso3c, year, incgroup, wbregion), names_to = "placeholder", values_to = "value") %>%
  mutate(sex = case_when(
    str_detect(placeholder, "_F$") ~ "female",
    str_detect(placeholder, "_M$") ~ "male",
    TRUE ~ "total"
  ),
  series = case_when(
    str_detect(placeholder, "F500") ~ "Percentage of graduates from Science, Technology, Engineering and Mathematics programmes in tertiary education, (%)",
    TRUE ~ "Percentage of graduates from ICT programmes in tertiary education, (%)"
  )) %>%
  # Want to only look at trends past 2010, only keep existing observations
  filter(!is.na(value), year >= 2010) %>%
  group_by(series, iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(indicator = series, source = "UNESCO UIS", category = "Skills") %>%
  select(-placeholder)

### ICT and Telecommunications professionals ####
# ILOSTAT
# Series name: Employment by sex and occupation - ISCO level 2
# Occupation (ISCO-08), 2 digit level: 25 - Information and communications technology professionals
ict_professionals <- read_csv("Input/ILO_ict_professionals.csv") %>%
  janitor::clean_names() %>%
  select(country = ref_area_label, sex = sex_label, year = time, value = obs_value) %>%
  pivot_wider(c(country, year), names_from = sex, values_from = value) %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ iso3c
         )) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  select(country = country.y, iso3c, year, `Sex: Total`, `Sex: Male`, `Sex: Female`, incgroup, wbregion) %>%
  pivot_longer(-c(country, iso3c, year, incgroup, wbregion), names_to = "sex", values_to = "value") %>%
  mutate(sex = case_when(
    str_detect(sex, "Male") ~ "male",
    str_detect(sex, "Female") ~ "female",
    TRUE ~ "total"
  ),
  series = "Proportion of ICT professionals", indicator = series,
  source = "ILOSTAT", category = "Leadership") %>%
    filter(!is.na(value), year >= 2010) %>%
    group_by(iso3c, sex) %>%
    mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
    filter(year == max(year, na.rm = TRUE)) %>%
    ungroup()

# ILOSTAT
# Employment by sex and economic activity - ISIC level 2
# 2 digit level: 61 - Telecommunications
telecomms_professionals <- read_csv("Input/ILO_telecomms_professionals.csv") %>%
  janitor::clean_names() %>%
  select(country = ref_area_label, sex = sex_label, year = time, value = obs_value) %>%
  pivot_wider(c(country, year), names_from = sex, values_from = value) %>%
  mutate(iso3c = countrycode::countrycode(country, "country.name", "iso3c"),
         iso3c = case_when(
           country == "Kosovo" ~ "XKX",
           TRUE ~ iso3c
         )) %>%
  # Merge in countries on iso3c and keep all observations
  full_join(odw_master_codes %>% select(iso3c, country, incgroup, wbregion), by = c("iso3c")) %>%
  # Filter to just African countries, sub-saharan plus six north africa.
  # Should be 54. AU has 55 member countries, but no info exists on Sahrawi Republic
  filter(wbregion == "Sub-Saharan Africa" | iso3c %in% c("DZA", "DJI", "EGY", "LBY", "MAR", "TUN")) %>%
  select(country = country.y, iso3c, year, `Sex: Total`, `Sex: Male`, `Sex: Female`, incgroup, wbregion) %>%
  pivot_longer(-c(country, iso3c, year, incgroup, wbregion), names_to = "sex", values_to = "value") %>%
  mutate(sex = case_when(
    str_detect(sex, "Male") ~ "male",
    str_detect(sex, "Female") ~ "female",
    TRUE ~ "total"
  ),
  series = "Proportion of Telecomms professionals", indicator = series,
  source = "ILOSTAT", category = "Leadership") %>%
  filter(!is.na(value), year >= 2010) %>%
  group_by(iso3c, sex) %>%
  mutate(non_missing_obs = sum(!is.na(value), na.rm = TRUE)) %>%
  filter(year == max(year, na.rm = TRUE)) %>%
  ungroup()

### COMBINE ####
top_ten_indicators <- educ %>%
  bind_rows(dig_payment) %>%
  bind_rows(int_use) %>%
  bind_rows(literacy_time) %>%
  bind_rows(mob_own_time) %>%
  bind_rows(researchers) %>%
  bind_rows(ict_professionals) %>%
  bind_rows(telecomms_professionals)

### Analyze ####
# Least number of countries per series for female
top_ten_indicators %>%
  group_by(category, indicator, series, sex) %>%
  summarize(num_countries = n_distinct(iso3c)) %>%
  ungroup() %>%
  filter(sex == "female") %>%
  arrange(num_countries)

# grouped bar chart (have replicated in Excel)
top_ten_indicators %>%
  group_by(category, indicator, sex) %>%
  summarize(num_countries = n_distinct(iso3c)) %>%
  ungroup() %>%
  mutate(indicator = case_when(
    str_detect(indicator, "Science, Technology") ~ "Percentage of graduates from Science, Technology, Engineering\nand Mathematics programmes in tertiary education",
    TRUE ~ indicator
  )) %>%
  ggplot(aes(fill=sex, y=num_countries, x=fct_rev(indicator))) + 
  geom_bar(position="dodge", stat="identity") +
  coord_flip() +
  scale_y_continuous(position = "right", limits = c(0,54), breaks = c(0, 10, 20, 30, 40, 50, 54)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(x = "", y = "Number of African countries")

# Least number of countries per series for male
top_ten_indicators %>%
  group_by(category, indicator, series, sex) %>%
  summarize(num_countries = n_distinct(iso3c)) %>%
  ungroup() %>%
  filter(sex == "male") %>%
  arrange(num_countries)

# Number of countries with data:
top_ten_indicators %>% 
  group_by(indicator, category, sex) %>% 
  summarize(countries = n_distinct(iso3c)/54) %>%
  print(n = 27)
# By category
top_ten_indicators %>% 
  group_by(indicator, category, sex) %>% 
  summarize(countries = n_distinct(iso3c)/54) %>%
  ungroup() %>%
  group_by(category, sex) %>%
  summarize(coverage = mean(countries, na.rm = TRUE)) %>%
  ungroup()

top_ten_indicators %>% 
  pivot_wider(c(country, series, indicator), names_from = sex, values_from = value) %>%
  filter(indicator == "Proportion of Telecomms professionals", ((!is.na(male) & is.na(female)) | (is.na(male) & !is.na(female))))
top_ten_indicators %>% 
  pivot_wider(c(country, series, indicator), names_from = sex, values_from = value) %>%
  filter(indicator == "Proportion of ICT professionals", ((!is.na(male) & is.na(female)) | (is.na(male) & !is.na(female))))



# All three categories in a table
top_ten_indicators %>%
  group_by(category, indicator, series, sex) %>%
  summarize(num_countries = n_distinct(iso3c)) %>%
  ungroup() %>%
  pivot_wider(c(category, indicator, series), names_from = sex, values_from = num_countries)

# Look by country
top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  pivot_wider(c(country, iso3c), names_from = sex, values_from = num_countries) %>%
  arrange(desc(total), desc(female))

top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  pivot_wider(c(country, iso3c), names_from = sex, values_from = num_countries) %>%
  arrange(desc(female), desc(male))

# Worst countries
top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  pivot_wider(c(country, iso3c), names_from = sex, values_from = num_countries) %>%
  arrange(total, female) %>%
  print(n = 15)
# Worst countries addition
top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  pivot_wider(c(country, iso3c), names_from = sex, values_from = num_countries) %>%
  arrange(female, male) %>%
  print(n = 15)

# Average indicators per country by sex
top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  group_by(sex) %>%
  summarize(num_countries = sum(num_countries, na.rm = TRUE)/54) %>%
  ungroup()

# Average indicators per country total
top_ten_indicators %>%
  group_by(country, iso3c) %>%
  summarize(num_countries = n_distinct(indicator)) %>%
  ungroup() %>%
  summarize(num_countries = sum(num_countries, na.rm = TRUE)/54)



### Timeliness ####
# Total by indicator
top_ten_indicators %>% 
  group_by(category, indicator) %>%
  summarize(mean_year = mean(year, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(category) %>%
  mutate(mean_category = mean(mean_year, na.rm = TRUE)) %>%
  ungroup()

# By sex
top_ten_indicators %>% 
  group_by(category, indicator, sex) %>%
  summarize(mean_year = mean(year, na.rm = TRUE)) %>%
  ungroup() %>%
  print(n = 27)

# By country
# Most
top_ten_indicators %>% 
  group_by(country, iso3c, sex) %>%
  summarize(mean_year = mean(year, na.rm = TRUE), num_indicator = n_distinct(indicator)) %>%
  ungroup() %>%
  arrange(sex, desc(mean_year))
# Least
top_ten_indicators %>% 
  group_by(country, iso3c, sex) %>%
  summarize(mean_year = mean(year, na.rm = TRUE), num_indicator = n_distinct(indicator)) %>%
  ungroup() %>%
  arrange(sex, mean_year)

# Timeline graph for average most recent and most out of date by indicator
top_ten_indicators %>%
  group_by(indicator, sex) %>%
  summarize(latest_year = max(year, na.rm = TRUE), earliest_year = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  count(sex, latest_year)

top_ten_indicators %>%
  group_by(country, iso3c, sex) %>%
  summarize(latest_year = max(year, na.rm = TRUE), earliest_year = min(year, na.rm = TRUE)) %>%
  ungroup() %>%
  pivot_longer(latest_year:earliest_year, names_to = "lat_or_early") %>%
  count(sex, lat_or_early, value) %>%
  filter(sex == "female") %>%
  ggplot(aes(x = value, y = n, fill = lat_or_early)) +
  geom_col(position = position_dodge2(preserve = "single"))

# Average latest year by all countries
top_ten_indicators %>% 
  group_by(country, iso3c, sex) %>%
  summarize(mean_year = mean(year, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(sex) %>%
  summarize(meanest_year = mean(mean_year, na.rm = TRUE), num_countries = n_distinct(country)) %>%
  ungroup() %>%
  summarize(avg_latest_year = weighted.mean(meanest_year, num_countries, na.rm = TRUE))

### Frequency ####
top_ten_indicators %>% 
  group_by(category, series, sex) %>%
  summarize(sum_obs = sum(non_missing_obs, na.rm = TRUE)) %>%
  ungroup() %>%
  print(n = 51)

# Total count by country:
# Most countries
top_ten_indicators %>% 
  group_by(country, iso3c, sex) %>%
  summarize(sum_obs = sum(non_missing_obs, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(sex, desc(sum_obs))
# Least countries
top_ten_indicators %>% 
  group_by(country, iso3c, sex) %>%
  summarize(sum_obs = sum(non_missing_obs, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(sex, sum_obs)


# Restricting to where more two or more datapoints exist
top_ten_indicators %>%
  filter(non_missing_obs >= 2) %>%
  group_by(category, series, sex) %>%
  summarize(num_countries = n_distinct(iso3c)) %>%
  ungroup() %>%
  arrange(desc(num_countries))

# Table for brief on number of observations per indicator
top_ten_indicators %>%
  group_by(iso3c, category, indicator, sex) %>%
  summarize(actual_missing_obs = round(mean(non_missing_obs, na.rm = TRUE), digits = 0)) %>%
  ungroup() %>%
  count(category, indicator, sex, actual_missing_obs) %>% 
  filter(sex == "female") %>% 
  pivot_wider(id_cols = c(category, indicator), names_from = actual_missing_obs, values_from = n)

# Countries with most/least observations
top_ten_indicators %>%
  filter(non_missing_obs >= 2) %>%
  group_by(country, iso3c, sex) %>%
  summarize(num_indicators = n_distinct(series, na.rm = TRUE)) %>%
  ungroup() %>%
  arrange(desc(num_indicators)) %>%
  print(n = 15)

# Illustrating where the most recent values fall, by category
top_ten_indicators %>% 
  filter(!is.na(value)) %>% 
  count(category, sex, year) %>% 
  ggplot(aes(x = year, y = n, fill = sex)) + 
  geom_col(position = "dodge") + 
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019)) +
  facet_wrap(. ~ category) +
 #theme(strip.text.x.left = element_text(angle = 45)) +
  theme_classic()
# Rotate x axis labels 45 degrees so all years fit

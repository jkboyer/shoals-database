# loads data exports from SHOALS program, formats data to match big boy database
# data, and saves as .csv (ready to import into access data entry template)
#
# This script is for Grand Canyon electrofishing data
#
# Author: Jan Boyer, AGFD jboyer@azgfd.gov
# inputs: .txt file (tab delimited) of fish data output from shoals program
#         created using Files -> Export Data in shoals
# outputs: 2 .csv files saved in ./output_data
#   one with site(sample) data: to import to FISH_T_SAMPLE table in access data
#                               entry template
#   one with fish(specimen) data: to import to FISH_T_SPECIMEN table in access
#                                 data entry template
#
# Notes:
# 1. Originally written for AGFD data, may need minor edits (i.e., add columns
# that your project uses but AGFD projects don't, some projects record PIT and
# finclip in RECAP_YES_NO, others only record PIT recap info) to work for other
# agencies or projects


library(tidyverse)

current.year <- 2021 #define current year
trip.id <- "GC20210403"

#where input data (.txt file exported from shoals) is stored
#shoals.data.filepath <-
# "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/2019/"
shoals.data.filepath <- "C:/Users/jboyer/Documents/"
#name of input file (.txt file exported from shoals)
shoals.file.name <- paste0(trip.id, "_AGFD_EF_v6_Export.txt")
#where do you want to save your output data?
#save.filepath <- "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/2019/"
save.filepath <- "./output_data/"

#enter values for how many samples and fish are already in access database for
#this trip (from angling and efish).
#needed to make sure assigned primary keys are unique and do not duplicate keys for
#electofish data
n.samples.access <- 175
n.fish.access <- 1536

#load site lists
all.sites <- read_csv("./site_lists/GC_sites.csv")
all.sites <- all.sites %>%
  mutate(S_SITE_ID = gsub("_", "", SiteID),
         start.rm = RiverMile_100ths) %>%
  select(S_SITE_ID, start.rm, RiverSide, reach, id)
#add end rivermile to sites
left <- all.sites %>%
  filter(RiverSide == "L") %>%
  arrange(id) %>%
  mutate(end.rm = lead(start.rm))
right <- all.sites %>%
  filter(RiverSide == "R") %>%
  arrange(id) %>%
  mutate(end.rm = lead(start.rm))

all.sites <- bind_rows(left, right)

#load turbidity data
turbidity <- read.csv("./input_data/GC20210403_turbidity_temperature.csv")

#load shoals data (electrofishing)
d <- read.delim(paste(shoals.data.filepath, shoals.file.name, sep = "/"),
                header = TRUE, stringsAsFactors = FALSE) #load shoals data
colnames(d)

#id column importing with strange symbols - rename to fix
d <- d %>%
  rename(ID = `ï..ID`)

#select and order only needed columns
d <- d %>%
  select(S_START_DATE_TIME,
         S_SAMPLE_TYPE, S_GEAR,  S_CREW,
         S_CLIPBOARD,
         S_SITE_ID,
         S_RM_START, S_RM_END, S_SIDE,
         S_EF_MINUTES, S_EF_SECONDS, S_EF_VOLTS, S_EF_AMPS,
         S_TRIP_ID,
         S_HABITAT, S_COVER, S_SUBSTRATE, S_HYDRAULIC,
         S_SAMPLE_NOTES,
         SPECIES, TOTAL_LENGTH, FORK_LENGTH, WEIGHT,
         SEX, SEX_COND, SEX_CHAR,
         RECAP_YES_NO, FINCLIP1, FISH_TAG, FISH_TAG_READER_TIME,
         DISPOSITION, SPECIMEN_NOTES, FISH_TAG_READER_NUM,
         ID, SAMPLE_ID)

# format and fix site data ############
#a few capitalization and format cleanup/standardization things
glimpse(d)
d <- d  %>%
  #capitalize fields that should be capitalized
  mutate_at(c("S_TRIP_ID", "S_SITE_ID", "S_CLIPBOARD", "S_CREW",
              "FISH_TAG"), toupper) %>%
  #format datetime fields as datetime (POSIXct)
  ##mutate_at(vars(ends_with("DATE_TIME")), #mutate all variables ending with...
  #          as.POSIXct, #with this function
  #         format = "%m/%d/%Y %H:%M") %>% #arguments needed for function
  mutate(S_START_DATE_TIME = as.POSIXct(S_START_DATE_TIME, format = "%m/%d/%Y %H:%M")) %>%
  mutate_at(vars(ends_with("READER_TIME")),
            as.POSIXct,
            format = "%m/%d/%Y %H:%M:%S") %>%
  mutate(S_TRIP_ID = trip.id) #fix any trip id errors

#check that each column is correct data class
glimpse(d)

length(unique(d$S_SITE_ID))
unique(d$S_SITE_ID)

#fix errors with site ID if needed
d <- d %>%
  mutate(S_SITE_ID = case_when(S_SITE_ID == "218.17R" ~ "218.17L",
                   S_SITE_ID == "218.80L" ~ "218.8L",
                   S_SITE_ID == "275.8L" ~ "275.80L",
                   S_SITE_ID == "277.8L" ~ "277.80L",
                   S_SITE_ID == "279.6R" ~ "279.60R",
                   TRUE ~ as.character(S_SITE_ID)))

sites.sampled = d %>%
  select(S_SITE_ID, S_START_DATE_TIME)%>%
  unique()

#check that these numbers match
length(unique(d$S_SITE_ID))
length(unique(d$SAMPLE_ID))

#join site info to shoals data
d <- d %>%
  left_join(all.sites)

#assign start/end RM and side
d <- d %>%
  mutate(S_RM_START = start.rm,
         S_RM_END = end.rm)

#check for date/datetime or river mile errors
#on this graph, samples should progress downstream and onward in time in a
#logical manner. If any sample sites are by themselves, check for errors


#check that sites proceed downstream over time in logical manner
#this will reveal datetime and river mile errors
d %>%
  ggplot(aes(x = S_START_DATE_TIME, y = S_RM_START)) +
  geom_point()

# COMBINE EF MINUTES AND SECONDS FOR TOTAL SECONDS
d <- d %>%
  mutate(EF_TOTAL_SECONDS = S_EF_MINUTES*60 + S_EF_SECONDS) %>%
  select(-c(S_EF_SECONDS, S_EF_MINUTES)) #no longer needed

#crew - standardize format: boatmen first, commas in between
unique(d$S_CREW)

d <- d %>%
  mutate(S_CREW = case_when(S_CREW == "AN,CH,JB,SW" ~ "AN, CH",
                            TRUE ~ S_CREW))

#format and fix fish data #################

#recap info is in different format in shoals (PIT and finclip combined) vs.
#database (PIT and finclip separate). Make column that matches database format
d$PITTAG_RECAP <- ifelse(d$RECAP_YES_NO == "PY" | d$RECAP_YES_NO == "BY", "Y",
                         ifelse(d$RECAP_YES_NO == "BN" | d$RECAP_YES_NO == "FY",
                                "N", NA))

d$FINCLIP1_RECAP <- ifelse(d$RECAP_YES_NO == "FY" | d$RECAP_YES_NO == "BY", "Y",
                           ifelse(d$RECAP_YES_NO == "PY" | d$RECAP_YES_NO == "BN",
                                  "N", NA))

#species counts
species.counts <- d %>%
  group_by(S_SAMPLE_TYPE, SPECIES) %>%
  summarize(n = n())
#look especially for unusual fish (GSF, FHM)
#these could be real, but likely are computer errors


#check weights and lengths - outliers are likely data entry errors
d %>%
  ggplot(aes(TOTAL_LENGTH, FORK_LENGTH)) +
  geom_point()
d %>%
  ggplot(aes(TOTAL_LENGTH, WEIGHT)) +
  geom_point()
d %>%
  ggplot(aes(FORK_LENGTH, WEIGHT)) +
  geom_point()

#Fix fish errors ############


#CHECK for duplicate tag entries (i.e., new fish measured before previous fish
#was saved
#Not all duplicates are errors, fish may have been captured 2x - check datetimes
dup.tags <- d %>%
  filter(is.na(FISH_TAG) == FALSE & FISH_TAG != "" &
           duplicated(FISH_TAG))


#fix errors in sex and condition
d <- d %>%
  #cannot have ripe fish of unknown sex - ripe must be error if sex = U
  mutate(SEX_COND = case_when(SEX == "U" & SEX_COND == "R" ~ "N",
                              #also G (gravid?) occasionally recorded for unknown fish?!
                              SEX == "U" & SEX_COND == "G" ~ "N",
                              TRUE ~ SEX_COND))

#format to match access files ###########
#assign numeric primary keys
#shoals uses strings of numbers and letters, access uses numeric primary keys
d <- d %>%
  arrange(SAMPLE_ID, S_START_DATE_TIME) %>% #order by sample id and end time
  #create new field to deal with resampling sites - site id is not unique
  mutate(S_SITE_ID_2 = paste(S_SITE_ID, S_START_DATE_TIME)) %>%
  #create numeric ids by converting existing id fields to numeric
  mutate(ACCESS_SAMPLE_ID = as.numeric(factor(S_SITE_ID_2)),
         ACCESS_FISH_ID = row_number()) %>%
  mutate(ACCESS_SAMPLE_ID = ACCESS_SAMPLE_ID + n.samples.access,
         ACCESS_FISH_ID = ACCESS_FISH_ID + n.fish.access)

#look at your sample size - makes sense?
max(d$ACCESS_FISH_ID)
max(d$ACCESS_SAMPLE_ID)

#Calculate TOTAL CATCH for each site or hoop net
catch <- d %>%
  mutate(SPECIES = as.factor(SPECIES)) %>%
  group_by(S_SITE_ID_2, SPECIES) %>%
  summarize(catch = n()) %>%
  mutate(catch = case_when(SPECIES == "NFC" ~ 0,
                           TRUE ~ as.numeric(catch)))  %>%
  group_by(S_SITE_ID_2) %>%
  summarize(TOTAL_CATCH = sum(catch))

#add total catch to data
d <- merge(d, catch, by = "S_SITE_ID_2", all.x = TRUE)

# rename columns to access column names and split into two dataframes #####
#sample (site) data
#this can be imported to the FISH_T_SAMPLE table in access
colnames(d)

#add end date
d <- d %>%
  mutate(END_DATETIME = S_START_DATE_TIME + EF_TOTAL_SECONDS)

site <- d %>%
  transmute(ACCESS_SAMPLE_ID = ACCESS_SAMPLE_ID,
            DATASHEET_SAMPLE_ID = S_SITE_ID,
            TRIP_ID = S_TRIP_ID,
            SAMPLE_TYPE = S_SAMPLE_TYPE,
            #dates and times are extracted from datetime
            START_DATE = substr(as.character(S_START_DATE_TIME), 1, 10),
            START_TIME = substr(as.character(S_START_DATE_TIME), 12, 16),
            END_DATE = substr(as.character(END_DATETIME), 1, 10),
            END_TIME = substr(as.character(END_DATETIME), 12, 16),
            START_DATETIME = S_START_DATE_TIME,
            END_DATETIME = END_DATETIME,
            RIVER_CODE = "COR",
            SIDE_CODE = RiverSide,
            START_RM = S_RM_START,
            END_RM = S_RM_END,
            STATION_ID = paste0("S", S_SITE_ID),
            GPS_START_WAYPOINT = S_SITE_ID,
            CREW = S_CREW,
            CLIPBOARD = S_CLIPBOARD,
            GEAR_CODE = "EL",
            TOTAL_CATCH = TOTAL_CATCH,
            EF_AMPS = S_EF_AMPS,
            EF_VOLTS = S_EF_VOLTS,
            EF_TOTAL_SECONDS = EF_TOTAL_SECONDS,
            HABITAT_CODE = S_HABITAT,
            HYDRAULIC_CODE = S_HYDRAULIC,
            SUBSTRATE_CODE = S_SUBSTRATE,
            COVER_CODE = S_COVER,
            TURBIDITY = "L",
            SAMPLE_NOTES = S_SAMPLE_NOTES) %>%
  distinct() %>% #keep only unique rows
  #in character fields, replace NAs with "" so access does not read as "NA"
  #ok to keep NAs in numeric fields, access interprets correctly for numeric
  replace_na(list(SAMPLE_NOTES = ""))

#join on turbidity data
#subset turbidity to needed data
turbidity <- turbidity %>%
  select(START_DATE, TURBIDITY_NTU, WATER_TEMP)

site <- site %>%
  left_join(turbidity)

# Make dates the frustrating, nonstandard microsoft format
# why don't you use ISO 8601 date standards, Microsoft?!
site <- site %>%
  mutate(START_DATETIME = as.character(format(as.POSIXct(START_DATETIME),
                                              "%m/%d/%Y %H:%M:%S")),
         END_DATETIME = as.character(format(as.POSIXct(END_DATETIME),
                                            "%m/%d/%Y %H:%M:%S")),
         START_DATE = as.character(format(as.Date(START_DATE), "%m/%d/%Y")),
         END_DATE = as.character(format(as.Date(END_DATE), "%m/%d/%Y")))

fish <- d %>%
  transmute(ACCESS_FISH_ID = ACCESS_FISH_ID,
            ACCESS_SAMPLE_ID = ACCESS_SAMPLE_ID,
            SPECIES_CODE = SPECIES,
            TOTAL_LENGTH = TOTAL_LENGTH,
            FORK_LENGTH = FORK_LENGTH,
            WEIGHT = WEIGHT,
            SEX_CODE = SEX,
            SEX_COND_CODE = SEX_COND,
            SEX_CHAR_CODE = SEX_CHAR,
            #shoals records PIT and finclip recap info in one field
            #access has separate fields for each
            FINCLIP1_RECAP = case_when(RECAP_YES_NO %in% c("BY", "FY") ~ "Y",
                                       RECAP_YES_NO %in% c("BN", "PY") ~ "N"),
            FINCLIP1 = FINCLIP1,
            PITTAG_RECAP = case_when(RECAP_YES_NO %in% c("BY", "PY") ~ "Y",
                                     RECAP_YES_NO %in% c("BN", "FY") ~ "N"),
            PITTAG = FISH_TAG,
            DISPOSITION_CODE = DISPOSITION,
            SPECIMEN_NOTES = SPECIMEN_NOTES) %>%
  #in character fields, replace NAs with "" so access does not read as "NA"
  #ok to keep NAs in numeric fields, access interprets correctly for numeric
  replace_na(list(SEX_CODE = "",
                  SEX_COND_CODE = "",
                  SEX_CHAR_CODE = "",
                  FINCLIP1_RECAP = "",
                  FINCLIP1  = "",
                  PITTAG_RECAP = "",
                  PITTAG = "",
                  DISPOSITION_CODE = "",
                  SPECIMEN_NOTES = ""))

#check that column names match those in big boy database
colnames(site)
colnames(fish)

# save files ######
write.csv(site,
          file = paste0(save.filepath, trip.id, "_sample_for_access.csv"),
          na = "", #save NA values as blanks
          row.names = FALSE)

write.csv(fish,
          file = paste0(save.filepath, trip.id, "_specimen_for_access.csv"),
          na = "",
          row.names = FALSE)

# Access import instructions ######
# 1. open most recent version of GCMRC_FISH_DATA_ENTRY_TEMPLATE.mdb, click
#    save as to give it a filename with your trip id
# 2. Import Sample data to FISH_T_SAMPLE table
#    2a. Click external data tab
#    2b. Click Import Text file button. Access will open a window to guide you
#        through the import
#    2c. On the first screen, use the browse button to locate the file to import
#        (GC20191024_sample_for_access.csv or similar), and choose "Append a
#        copy of the records to the table: FISH_T_SAMPLE"
#    2d. choose delimited on the second screen
#    2e. choose comma when asked what delimiter separates your data, and check
#        the "First Row Contains Field Names" box
# 3. Repeat step 2 with specimen data, appending your specimen file to the
#    FISH_T_SPECIMEN table

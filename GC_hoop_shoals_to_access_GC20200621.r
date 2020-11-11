# cleans and edits shoals data output, formats into same format used by access
# data entry templates so that shoals data can be imported into access
# Also useful for getting shoals data into a format that can be merged/joined
# onto big boy downloads, if shoals data from recent trip is not yet in big boy

# Author: Jan Boyer, AGFD jboyer@azgfd.gov
# inputs: .txt file (tab delimited) of fish data output from shoals program
#         using Files -> Export Data in shoals
# outputs: 2 .csv files:
#   one with site(sample) data: to import to FISH_T_SAMPLE table in access data
#                               entry template
#   one with fish(specimen) data: to import to FISH_T_SPECIMEN table in access
#                                 data entry template

# Note:Originally written for AGFD data, may need minor edits (i.e., add columns
# that your project uses but AGFD projects don't, some projects record PIT and
# finclip in RECAP_YES_NO, others only record PIT recap info) to work for other
# agencies or projects

library(tidyverse)

# update these field to match your data #######
#trip id
trip.id <- "GC20200621"
#where input data (.txt file exported from shoals) is stored
#shoals.data.filepath <-
 # "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/2019/"
shoals.data.filepath <- "C:/Users/jboyer/Documents/"
#name of input file (.txt file exported from shoals)
shoals.file.name <- "GC20200621_AGFD_HP_v6_Export.txt"
#where do you want to save your output data?
#save.filepath <- "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/2019/"
save.filepath <- "./output_data/"

#load site lists
all.sites <- read_csv("./site_lists/GC_sites.csv")
all.sites <- all.sites %>%
  mutate(S_SITE_ID = gsub("_", "", SiteID)) %>%
  select(S_SITE_ID, RiverSide)

# read shoals data #####
#Important to use base R read functions - tidyverse read functions will read data
#incorrectly if data is only recorded in a few rows
d <- read.delim(paste0(shoals.data.filepath, shoals.file.name),
                stringsAsFactors = FALSE) #import text as character, not factor

#id column importing with strange symbols - rename to fix
d <- d %>%
  rename(ID = `ï..ID`)

#select and order only needed columns
d <- d %>%
  select(S_START_DATE_TIME, S_END_DATE_TIME,
         S_SAMPLE_TYPE, S_GEAR,  S_CREW,
         S_CLIPBOARD,
         S_SITE_ID, S_RIVER_MILE, S_GPS_ID, S_SET_NUM,
         S_TRIP_ID,
         S_HABITAT, S_COVER, S_SUBSTRATE, S_HYDRAULIC,
         S_SAMPLE_NOTES,
         SPECIES, TOTAL_LENGTH, FORK_LENGTH, WEIGHT,
         SEX, SEX_COND, SEX_CHAR,
         RECAP_YES_NO, FINCLIP1, FISH_TAG, FISH_TAG_READER_TIME,
         DISPOSITION, SPECIMEN_NOTES, FISH_TAG_READER_NUM,
         ID, SAMPLE_ID)

#a few capitalization and format cleanup/standardization things
glimpse(d)
d <- d  %>%
  #capitalize fields that should be capitalized
  mutate_at(c("S_TRIP_ID", "S_SITE_ID", "S_CLIPBOARD", "S_CREW", "S_GPS_ID",
              "FISH_TAG"), toupper) %>%
  #format datetime fields as datetime (POSIXct)
  mutate_at(vars(ends_with("DATE_TIME")), #mutate all variables ending with...
            as.POSIXct, #with this function
            format = "%m/%d/%Y %H:%M") %>% #arguments needed for function
  mutate_at(vars(ends_with("READER_TIME")),
            as.POSIXct,
            format = "%m/%d/%Y %H:%M:%S") %>%
  mutate(S_RIVER_MILE = as.numeric(S_RIVER_MILE)) %>%
  mutate(S_TRIP_ID = trip.id) #fix any trip id errors

#check that each column is correct data class
glimpse(d)

#error checks ######
#depending on your preference, any errors found can be fixed
#  a) in the shoals program using the edit tab
#  b) in a copy of the data export file from shoals
#  c) in R, with either ifelse() or mutate(case_when()) code

#Fix site ids that are missing side or have typos
d <- d %>%
  mutate(S_SITE_ID = case_when(
    S_SITE_ID == "225.13LC" ~ "225.13L",
    S_SITE_ID == "34.59" ~ "34.59R",
    S_SITE_ID == "35.73" ~ "35.73R",
    S_SITE_ID == "173.06" ~ "173.06R",
    S_SITE_ID == "173.0696" ~ "173.06R",
    S_SITE_ID == "201.79LC" ~ "201.79L",
    S_SITE_ID == "252.25" ~ "252.25R",
    S_SITE_ID == "253.32" ~ "253.32R",
    TRUE ~ S_SITE_ID))

#Check to see that n sites and n site ids match
length(unique(d$S_SITE_ID)) #n sample sites
length(unique(d$SAMPLE_ID)) #n sample sites according to shoals

#which shoals ids have multiple sites (caused by not using F10 for new sample)
d %>%
  group_by(SAMPLE_ID) %>%
  summarize(n_sites = length(unique(S_SITE_ID))) %>%
  arrange(-n_sites)

#fix - give second site a unique sample id by pasting on another letter
d <- d %>%
  mutate(SAMPLE_ID = case_when(S_SITE_ID == "253.73R" ~ paste0(SAMPLE_ID, "a"),
                               TRUE ~ SAMPLE_ID))

#which SITE ids have multiple keys (caused by entering wrong site id and
# inadvertantly duplicating another site, or adding site data mid-site)
d %>%
  group_by(S_SITE_ID) %>%
  summarize(n_sites = length(unique(SAMPLE_ID))) %>%
  arrange(-n_sites)

#fix - make sure site data matches for all records in site
d <- d %>%
  #81.68R - wrong side entered for opposite site 81.68L
  #91.13R - site id and gps id entered for 91.13R and next site(91.16L)
  #140.12L - this site id enterered for 140.26L and next site (140.55L)
  #201.79L, 225.13L - data entered in wrong site, see comments
  #253.32R: R/L typo
  mutate(S_SITE_ID = case_when(S_GPS_ID == "BL053" ~ "81.68L",
                               S_RIVER_MILE == 96.20 ~ "91.16L",
                               SAMPLE_ID == "0629_0738_39" ~ "140.55L",
                               SAMPLE_ID == "0703_0932_16" ~ "202.24L",
                               SAMPLE_ID == "0704_0820_17" ~ "225L",
                               SAMPLE_ID ==  "0705_1029_56" ~ "253.32L",
                               TRUE ~ S_SITE_ID),
         S_GPS_ID = case_when(S_RIVER_MILE == 96.20 ~ "AL0602",
                              SAMPLE_ID == "0703_0932_16" ~ "AL125",
                              SAMPLE_ID == "0704_0820_17" ~ "BL131",
                              TRUE ~ S_GPS_ID),
         S_RIVER_MILE = case_when(SAMPLE_ID == "0703_0932_16" ~ 202.27,
                                  SAMPLE_ID == "0704_0820_17" ~ 225.08,
                                  TRUE ~ S_RIVER_MILE),
         S_START_DATE_TIME = case_when(SAMPLE_ID == "0703_0932_16" ~
                                         as.POSIXct("2020-07-02 16:23:00"),
                                       SAMPLE_ID == "	0704_0820_17" ~
                                         as.POSIXct("2020-07-03 15:40:00"),
                                       TRUE ~ S_START_DATE_TIME))

#Check to see that n sites and n site ids match
length(unique(d$S_SITE_ID)) #n sample sites
length(unique(d$SAMPLE_ID)) #n sample sites according to shoals

#check for RM errors - subtract actual RM from start rm
#look for large (>0.20 miles) discrepancies
d <- d %>%
  mutate(diff = S_RIVER_MILE - as.numeric(gsub("[[:alpha:]]", "", S_SITE_ID)))

#Fix any errors found
d <- d %>%
  mutate(S_SITE_ID = case_when(SAMPLE_ID == "0624_0905_41" ~ "53.98R",
                              SAMPLE_ID == "0627_0744_32" ~ "96.16L",
                              SAMPLE_ID == "0630_0803_00" ~ "154.77L",
                              SAMPLE_ID == "0626_0551_46" ~ "79.8L",
                              SAMPLE_ID == "0625_0741_50" ~ "66.24R",
                               TRUE ~ S_SITE_ID),
         S_RIVER_MILE = case_when(SAMPLE_ID == "0703_0810_15" ~ 201.98,
                                  TRUE ~ S_RIVER_MILE)) %>%
  select(-diff)

#join on site list and check that matches to find site entry errors
d <- d %>%
  left_join(all.sites)

#fix errors
d <- d %>%
  mutate(S_SITE_ID = case_when(SAMPLE_ID == "0706_0649_28" ~ "275.23L",
                               SAMPLE_ID ==  "0627_0734_42" ~ "96.13R",
                               TRUE ~ S_SITE_ID)) %>%
  select(-RiverSide)

d <- d %>%
  left_join(all.sites)

#hoop net start and ends look accurate?
#no negative time or multi-day sets?
d <- d %>%
  mutate(EFFORT_HOURS = as.numeric(difftime(S_END_DATE_TIME, S_START_DATE_TIME,
                                            units = "hours")))

summary(d$EFFORT_HOURS[d$S_GEAR %in% c("MHB", "HB")])

#a few sites with 2019 instead of 2020 as start - add 1 year of seconds
d <- d %>%
  mutate(S_START_DATE_TIME = case_when(
    S_START_DATE_TIME < as.POSIXct("2020-01-01 01:01:01") ~ S_START_DATE_TIME +
                                     366*24*60*60, #2020 was leap year
    TRUE ~ S_START_DATE_TIME))

d <- d %>%
  mutate(EFFORT_HOURS = as.numeric(difftime(S_END_DATE_TIME, S_START_DATE_TIME,
                                            units = "hours")))

summary(d$EFFORT_HOURS[d$S_GEAR %in% c("MHB", "HB")])

#check for date/datetime or river mile errors
#on this graph, samples should progress downstream and onward in time in a
#logical manner. If any sample sites are by themselves, check for errors
d %>%
  ggplot(aes(x = S_START_DATE_TIME, y = S_RIVER_MILE)) +
  geom_point(aes(color = S_CLIPBOARD))

# add columns ######

#extract side from site id
d <- d %>%
  mutate(S_SIDE = str_extract(S_SITE_ID, "[A-Z]+" ))
unique(d$S_SIDE) #check that this is only L and R

#Calculate TOTAL CATCH for each site or hoop net
catch <- d %>%
  mutate(SPECIES = as.factor(SPECIES)) %>%
  group_by(S_SITE_ID, SPECIES) %>%
  summarize(catch = n()) %>%
  mutate(catch = case_when(SPECIES == "NFC" ~ 0,
                           TRUE ~ as.numeric(catch)))  %>%
  group_by(S_SITE_ID) %>%
  summarize(TOTAL_CATCH = sum(catch))

#add total catch to data
d <- merge(d, catch, by = "S_SITE_ID", all.x = TRUE)

#a few quick fish checks:
#that shoals bug that converts normal species to wierd species
#if stuff like GSF, STB shows up, check that it is real
species.counts <- d %>%
  group_by(SPECIES) %>%
  summarise(n = n()) %>%
  arrange(-n)

d <- d %>%
  mutate(FORK_LENGTH = as.numeric(FORK_LENGTH),
         TOTAL_LENGTH = as.numeric(TOTAL_LENGTH),
         WEIGHT = as.numeric(WEIGHT)) %>%
  mutate(SPECIES = case_when(SPECIES == "" ~ "FMS",
                             TRUE ~ SPECIES),
         TOTAL_LENGTH = case_when(ID == "ID_20200701_0906_25_233_03f0923b" ~ 370,
                                  ID == "ID_20200704_0831_34_460_42705a79" ~ 98,
                                  ID == "ID_20200630_0821_57_118_ec0371bc" ~ 437,
                                  ID == "ID_20200701_0948_00_900_950ff17c" ~ 363,
                                  ID == "ID_20200630_0926_09_331_0eb453ca" ~ 227,
                                  ID == "ID_20200701_0912_15_674_439a78f7" ~ 226,
                                  ID == "ID_20200630_0836_42_797_5d05ab61" ~ 226,
                                  ID == "ID_20200705_0905_15_325_400d6e7c" ~ 177,
                                  TRUE ~ TOTAL_LENGTH),
         FORK_LENGTH = case_when(ID == "ID_20200705_0916_07_019_ef04a42c" ~ as.numeric(NA),
                                 ID == "ID_20200630_0942_45_355_ef149d9b" ~ 371,
                                 ID == "ID_20200702_0932_26_049_d1c6ef6e" ~ 386,
                                 ID == "ID_20200630_0844_03_986_2315cb95" ~ 273,
                                 ID == "ID_20200703_0903_40_590_11f61c3a" ~ as.numeric(NA),
                                 ID == "ID_20200705_1040_16_197_717ed25d" ~ as.numeric(NA),
                                 ID == "ID_20200701_0959_37_820_d25ffc0c" ~ as.numeric(NA),
                                 ID == "ID_20200704_0833_52_497_128e8b7f" ~ 231,
                                 ID == "ID_20200701_0920_36_625_88741647" ~ 406,
                                 ID == "ID_20200627_0805_57_165_8c641cd0" ~ as.numeric(NA),
                                 TRUE ~ FORK_LENGTH),
         WEIGHT = case_when(ID == "ID_20200630_0844_03_986_2315cb95" ~ 179,
                            ID == "ID_20200629_0834_58_852_93d93a24" ~ as.numeric(NA),
                            TRUE ~ WEIGHT))

d <- d %>%
  mutate(total_minus_fork = TOTAL_LENGTH - FORK_LENGTH)

d %>%
  mutate(error = case_when(total_minus_fork >= 40 ~ "error",
                                       TRUE ~ "ok")) %>%
  ggplot(aes(x = TOTAL_LENGTH, y = FORK_LENGTH, color = error)) +
  geom_point()+
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))

d %>%
  mutate(error = case_when(total_minus_fork >= 40 ~ "error",
                           TRUE ~ "ok")) %>%
  ggplot(aes(x = TOTAL_LENGTH, y = WEIGHT, color = error)) +
  geom_point() +
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))


d %>%
  mutate(error = case_when(total_minus_fork >= 40 ~ "error",
                           TRUE ~ "ok")) %>%
  ggplot(aes(x = FORK_LENGTH, y = WEIGHT, color = error)) +
  geom_point() +
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))

#assign numeric primary keys
#shoals uses strings of numbers and letters, access uses numeric primary keys
d <- d %>% #order data
  mutate(START_DATE = substr(as.character(S_START_DATE_TIME), 1, 10)) %>%
  arrange(START_DATE, S_CLIPBOARD, S_END_DATE_TIME) %>%
  #create numeric ids by converting existing id fields to numeric
  mutate(ACCESS_SAMPLE_ID = as.numeric(
    #making access_sample_id from these fields keeps order logical
    factor(paste(START_DATE, S_CLIPBOARD, S_END_DATE_TIME))),
    #simply converting shoals IDs makes order super confusing
    #ACCESS_SAMPLE_ID = as.numeric(factor(S_SITE_ID)),
         ACCESS_FISH_ID = row_number()) %>%
  #add n samples already in access db (efish + angling) so IDs are unique
  mutate(ACCESS_SAMPLE_ID = ACCESS_SAMPLE_ID + 267,
         ACCESS_FISH_ID = ACCESS_FISH_ID + 2686)

#look at your sample size
max(d$ACCESS_FISH_ID)
max(d$ACCESS_SAMPLE_ID)

# rename columns to access column names and split into two dataframes #####
#sample (site) data
#this can be imported to the FISH_T_SAMPLE table in access

site <- d %>%
  transmute(ACCESS_SAMPLE_ID = ACCESS_SAMPLE_ID,
            DATASHEET_SAMPLE_ID = S_SITE_ID,
            TRIP_ID = S_TRIP_ID,
            SAMPLE_TYPE = S_SAMPLE_TYPE,
            #dates and times are extracted from datetime
            START_DATE = substr(as.character(S_START_DATE_TIME), 1, 10),
            START_TIME = substr(as.character(S_START_DATE_TIME), 12, 16),
            END_DATE = substr(as.character(S_END_DATE_TIME), 1, 10),
            END_TIME = case_when(S_GEAR == "MHB" ~
                                   substr(as.character(S_END_DATE_TIME), 12, 16),
                                 S_GEAR == "EL" ~ ""),
            START_DATETIME = S_START_DATE_TIME,
            END_DATETIME = S_END_DATE_TIME,
            TOTAL_HOURS = round(EFFORT_HOURS, 2),
            RIVER_CODE = "COR",
            SIDE_CODE = S_SIDE,
            START_RM = S_RIVER_MILE,
            STATION_ID = S_GPS_ID,
            CREW = S_CREW,
            CLIPBOARD = S_CLIPBOARD,
            GEAR_CODE = S_GEAR,
            TOTAL_CATCH = TOTAL_CATCH,
            HABITAT_CODE = S_HABITAT,
            HYDRAULIC_CODE = S_HYDRAULIC,
            SUBSTRATE_CODE = S_SUBSTRATE,
            COVER_CODE = S_COVER,
            SAMPLE_NOTES = S_SAMPLE_NOTES) %>%
  distinct() #keep only unique rows

#add turbidity and temperature data
turbidity <- read.csv("./input_data/GC20200621_turbidity_temperature.csv",
                      stringsAsFactors = FALSE)
turbidity <- turbidity %>%
  transmute(START_DATE = date,
            WATER_TEMP = temperature,
            TURBIDITY_NTU = TurbidityNTU)

site <- site %>%
  left_join(turbidity)

# Make dates the frustrating, nonstandard microsoft format
# why don't you use ISO 8601 date standards, Microsoft?!
site <- site %>%
  mutate(START_DATE = as.character(format(as.Date(START_DATE), "%m/%d/%Y")),
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
            SPECIMEN_NOTES = SPECIMEN_NOTES)

#check that column names match those in big boy database
colnames(site)
colnames(fish)

# save files ######
write.csv(site,
          file = paste0(save.filepath, trip.id, "_hoops_sample_for_access.csv"),
          na = "", #save NA values as blanks
          row.names = FALSE)

write.csv(fish,
          file = paste0(save.filepath, trip.id, "_hoops_specimen_for_access.csv"),
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


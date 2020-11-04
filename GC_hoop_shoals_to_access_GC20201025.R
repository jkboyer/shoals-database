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
trip.id <- "GC20201025"
#where input data (.txt file exported from shoals) is stored
#shoals.data.filepath <-
 # "\\\\FLAG-SERVER/Office/Grand Canyon Downstream/Databases/2019/"
shoals.data.filepath <- "./input_data/"
#name of input file (.txt file exported from shoals)
shoals.file.name <- "GC20201025_AGFD_HP_v6_Export.txt"
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
unique(d$S_SITE_ID)

#Check to see that n sites and n site ids match
length(unique(d$S_SITE_ID)) #n sample sites
length(unique(d$SAMPLE_ID)) #n sample sites according to shoals

#which shoals ids have multiple sites (caused by not using F10 for new sample)
d %>%
  group_by(SAMPLE_ID) %>%
  summarize(n_sites = length(unique(S_SITE_ID))) %>%
  arrange(-n_sites)

#sample 1026_0930_42 (boat B day 1) has all 8 hoop nets under 1 sample code
# (Caused by not using f10(new sample) in shoals)
# to fix, add site id to each sample id to make unique for each net
d <- d %>%
  mutate(SAMPLE_ID = case_when(SAMPLE_ID == "1026_0930_42" ~
                                 paste0(SAMPLE_ID, S_SITE_ID),
                               TRUE ~ SAMPLE_ID))

#which SITE_IDs have multiple SAMPLE_IDs (caused by entering wrong site id and
# inadvertantly duplicating another site, or adding site data mid-site)
d %>%
  group_by(S_SITE_ID) %>%
  summarize(n_sites = length(unique(SAMPLE_ID))) %>%
  arrange(-n_sites)

#fix - make sure site data matches for all records in site
  #254.66R - fish found on floor - had to use new entry - change SAMPLE_ID below
  #280.29L - accidentally entered wrong site info - changed in shoals, reload data
d$SAMPLE_ID <- ifelse(d$ID == "ID_20201027_1019_05_171_e0873e75", "1027_0954_15",
                      d$SAMPLE_ID)

#Check to see that n sites and n site ids match
length(unique(d$S_SITE_ID)) #n sample sites
length(unique(d$SAMPLE_ID)) #n sample sites according to shoals

#check for RM errors - subtract actual RM from start rm
#look for large (>0.20 miles) discrepancies
d <- d %>%
  mutate(diff = S_RIVER_MILE - as.numeric(gsub("[[:alpha:]]", "", S_SITE_ID)))

#Fix any errors found
d <- d %>%
  mutate(S_SITE_ID = case_when(
    #typos entering site id
    SAMPLE_ID == "1027_1006_50" ~ "255.37R",
    SAMPLE_ID == "1028_0842_30" ~ "260.86R",
    #had to set net 1 site further downstream due to sandbar
    SAMPLE_ID == "1029_0841_05" ~ "279.49L",
    #moved 1 site upstream cause pearce ferry rapid is scary
    SAMPLE_ID == "1029_0957_19" ~ "281.26L",
    TRUE ~ S_SITE_ID),
    #typo writing river mile
         S_RIVER_MILE = case_when(SAMPLE_ID == "1027_0954_15" ~ 254.80,
                                  SAMPLE_ID == "1026_0946_48" ~ 237.29,
                                  TRUE ~ S_RIVER_MILE))

d <- d %>%  #calculate distances again
  mutate(diff = S_RIVER_MILE - as.numeric(gsub("[[:alpha:]]", "", S_SITE_ID)))

d <- d %>%
  select(-diff) #no longer needed, remove column

#join on site list and check that matches to find site entry errors
d <- d %>%
  left_join(all.sites)

#fix errors
d <- d %>%
  #side missing from site ID
  mutate(S_SITE_ID = case_when(SAMPLE_ID == "1026_1144_45" ~ "240.11L",
                               TRUE ~ S_SITE_ID)) %>%
  select(-RiverSide)

d <- d %>%
  left_join(all.sites)

#check that regular and supplemental codes are correct
sample.type <- d %>%
 select(S_CLIPBOARD, S_SAMPLE_TYPE, S_SITE_ID, S_START_DATE_TIME, SAMPLE_ID, S_SAMPLE_NOTES) %>%
  unique() %>%
  arrange(S_CLIPBOARD, S_START_DATE_TIME)

#day 1 boat B net 4 had comment + sample type carried over from previosu net
#coded as 115 but should be 99, throat was still in water
d <- d %>%
  mutate(S_SAMPLE_TYPE = case_when(SAMPLE_ID == "1026_0930_42238.19R" ~ as.integer(99),
                                   TRUE ~ S_SAMPLE_TYPE),
         S_SAMPLE_NOTES = case_when(SAMPLE_ID == "1026_0930_42238.19R" ~ "",
                                    TRUE ~ S_SAMPLE_NOTES))

#fix confusing sample notes
d <- d %>%
  mutate(S_SAMPLE_NOTES = case_when(
    S_SAMPLE_NOTES == "Mouth of hoopnet out of water but still submerged2" ~
      "Mouth of hoopnet out of water but still submerged",
    S_SAMPLE_NOTES == "Float is out of the water, net partially submerged" ~
      "Throat is out of the water, net partially submerged",
    S_SAMPLE_NOTES == "Float is out of the water"~
      "Throat is out of the water",
    TRUE ~ S_SAMPLE_NOTES))

#hoop net start and ends look accurate?
#no negative time or multi-day sets?
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

#assign numeric primary keys
#shoals uses strings of numbers and letters, access uses numeric primary keys
d <- d %>%
  arrange(SAMPLE_ID, S_END_DATE_TIME) %>% #order by sample id and end time
  #create numeric ids by converting existing id fields to numeric
  mutate(ACCESS_SAMPLE_ID = as.numeric(factor(S_SITE_ID)),
         ACCESS_FISH_ID = row_number())

#look at your sample size
max(d$ACCESS_FISH_ID)
max(d$ACCESS_SAMPLE_ID)

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
#GSF is correct (dead release, lengths make sense)


d <- d %>%
  mutate(FORK_LENGTH = as.numeric(FORK_LENGTH),
         TOTAL_LENGTH = as.numeric(TOTAL_LENGTH),
         WEIGHT = as.numeric(WEIGHT)) %>%
  mutate(SPECIES = case_when(
    #recorded as BNT, actually FMS based on PIT tag and TL/FL ratio
    ID == "ID_20201026_1301_00_676_b574e4d2" ~ "FMS",
    #recorded as CRP, actually FMS based on no dorsal clip and TL/FL/WT ratio
    ID == "ID_20201027_1026_47_051_6d46085e" ~ "FMS",
    #recorded as SMB, actually FMS based of RA and TL/FL ratio
    ID == "ID_20201026_1223_24_571_9fc95052" ~ "FMS",
                             TRUE ~ SPECIES))

#check lengths and weights
d <- d %>%
  mutate(total_minus_fork = TOTAL_LENGTH - FORK_LENGTH)

d %>%
  mutate(error = case_when(abs(total_minus_fork) >= 40 ~ "error",
                                       TRUE ~ "ok")) %>%
  ggplot(aes(x = TOTAL_LENGTH, y = FORK_LENGTH, color = error)) +
  geom_point()+
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))

d %>%
  mutate(error = case_when(abs(total_minus_fork) >= 40 ~ "error",
                           TRUE ~ "ok")) %>%
  ggplot(aes(x = TOTAL_LENGTH, y = WEIGHT, color = error)) +
  geom_point() +
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))

d %>%
  mutate(error = case_when(abs(total_minus_fork) >= 40 ~ "error",
                           TRUE ~ "ok")) %>%
  ggplot(aes(x = FORK_LENGTH, y = WEIGHT, color = error)) +
  geom_point() +
  geom_text(aes(label = total_minus_fork, alpha = error)) +
  scale_alpha_manual(values = c(1, 0))

#fix weight and length errors
d <- d %>%
  #have to use NA_real_ instead of NA so dplyr interprets as numeric
  #This is a frustrating tidyverse thing that is worse than base R
  mutate(FORK_LENGTH = case_when(ID == "ID_20201026_0944_18_121_c5d33d12" ~ NA_real_,
                                 ID == "ID_20201026_1007_22_978_a42e7e00" ~ NA_real_,
                                 TRUE ~ FORK_LENGTH),
         TOTAL_LENGTH = case_when(ID == "ID_20201028_1004_58_543_4aaf6c5c" ~ 334,
                                  TRUE ~ TOTAL_LENGTH))

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
turbidity <- read.csv("./input_data/GC20201025_turbidity_temperature.csv",
                      stringsAsFactors = FALSE)
turbidity <- turbidity %>%
  transmute(START_DATE = date,
            WATER_TEMP = temperature,
            TURBIDITY_NTU = TurbidityNTU)
glimpse(turbidity)
glimpse(site)

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


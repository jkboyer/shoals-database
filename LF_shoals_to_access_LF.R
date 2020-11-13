# revised code - as SHOALS was changed
# and some basic plots for BNT/RBT for trip reports or presentations
# NOTE: END_DATETIME is incorrect from SHOALS, it is default 2018-01-01 00:01:00
# but don't really need that for EF data
#loads data entered directly into field computers (from shoals program),
#formats it to match database data, and saves as .csv
#inputs: .txt file with data export from shoals
#        (no format changes from program output!)
#outputs: site_shoals.csv
#         fish_shoals.csv
#        shoals data formatted to match format of database data
#        saved in ./data/raw

library(tidyverse)
site.list <- read.csv("./site_lists/LF_sites.csv", stringsAsFactors = FALSE)
site.list <- site.list %>%
  filter(type == "monitoring") %>%
  select(S_SITE_ID, side, rm_start, rm_end) %>%
  distinct()

current.year <- 2020 #define current year
trip.id <- "LF202000310"

#current.year.directory <- paste(
#  "\\\\flag-server/Office/Lees Ferry/Database files",
#  current.year, sep = "/")

#working at home because COVID, no server access
current.year.directory <- "./input_data/"

file.name <- "LF20200310_shoals_export.txt"

d <- read.delim(paste(current.year.directory, file.name, sep = "/"),
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

#fix errors with site ID
d <- d %>%
 mutate(S_SITE_ID = case_when(S_SITE_ID == "-5.05 R" ~ "-5.05R",
                              S_SITE_ID == "-4.61" ~ "-4.61R",
                              TRUE ~ S_SITE_ID))


sites.sampled = d %>%
  select(S_SITE_ID, S_START_DATE_TIME)%>%
  unique()

length(unique(d$S_SITE_ID))
length(unique(d$SAMPLE_ID))

d <- d %>%
  mutate(S_SIDE = case_when(S_SIDE == "L(LEFT)" ~ "L", TRUE ~ S_SIDE))

#join site info to shoals data
#use base, because tidyverse was erraneously adding rows
d <- d %>%
  left_join(site.list)


#assign start/end RM and side
#d <- d %>%
 # mutate(S_RM_START = case_when(
 #   #If start RM entered (nonnative sites), keep existing value
    !is.na(S_RM_START) ~ S_RM_START,
    #For monitoring sites, start RM from joined site table
  #  !is.na(rm_start) ~ rm_start,
    #RM often not recorded for slough. If missing for slough sites, add RM
    #useful to have RM for mapping locations of nonnatives, do not want
    # to leave blank(this RM is a center point of slough)
   # is.na(S_RM_START) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ -12.15),
  #  S_RM_END = case_when( #do the same for end RM
    #  !is.na(S_RM_END) ~ S_RM_END,
   #   !is.na(rm_end) ~ rm_end,
    #  is.na(S_RM_END) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ -12.15),
   # S_SIDE = case_when( #do the same for end RM
    #  !is.na(S_SIDE) & S_SIDE != "" ~ S_SIDE,
    #  !is.na(side) & side != "" ~ side,
    #  is.na(S_SIDE) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ "L")) %>%
  #select(-c(rm_start, rm_end, side)) #no longer needed, remove


#check for date/datetime or river mile errors
#on this graph, samples should progress downstream and onward in time in a
#logical manner. If any sample sites are by themselves, check for errors


#check that sites proceed downstream over time in logical manner
#this will reveal datetime and river mile errors
d %>%
  ggplot(aes(x = S_START_DATE_TIME, y = S_RM_START)) +
  geom_point()

d <- d %>% #fix one time error
  mutate(S_START_DATE_TIME = case_when(
    S_START_DATE_TIME == as.POSIXct("2020-03-10 11:32:00") ~
                     as.POSIXct("2020-03-10 22:00:00"),
                   TRUE ~ S_START_DATE_TIME))


# COMBINE EF MINUTES AND SECONDS FOR TOTAL SECONDS
d <- d %>%
  mutate(EF_TOTAL_SECONDS = S_EF_MINUTES*60 + S_EF_SECONDS) %>%
  select(-c(S_EF_SECONDS, S_EF_MINUTES)) #no longer needed

#crew - standardize format: boatmen first, commas in between
unique(d$S_CREW)


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
#look especially for unusual fish (GSF, FHM) caught at montioring sites (96)
#these could be real, but likely are computer errors

#fix errors if needed


#check weights and lengths
#rainbows
d %>%
  filter(SPECIES == "RBT") %>%
  ggplot(aes(TOTAL_LENGTH, FORK_LENGTH)) +
  geom_point()
d %>%
  filter(SPECIES == "RBT") %>%
  ggplot(aes(TOTAL_LENGTH, WEIGHT)) +
  geom_point()
d %>%
  filter(SPECIES == "RBT") %>%
  ggplot(aes(FORK_LENGTH, WEIGHT)) +
  geom_point()

#browns
d %>%
  filter(SPECIES == "BNT") %>%
  ggplot(aes(TOTAL_LENGTH, FORK_LENGTH)) +
  geom_point()
d %>%
  filter(SPECIES == "BNT") %>%
  ggplot(aes(TOTAL_LENGTH, WEIGHT)) +
  geom_point()
d %>%
  filter(SPECIES == "BNT") %>%
  ggplot(aes(FORK_LENGTH, WEIGHT)) +
  geom_point()

#others
#rainbows
d %>%
  filter(SPECIES %in% c("RBT", "BNT", "NFC") == FALSE) %>%
  ggplot(aes(TOTAL_LENGTH, FORK_LENGTH)) +
  geom_point() +
  facet_wrap(vars(SPECIES))
d %>%
  filter(SPECIES %in% c("RBT", "BNT", "NFC") == FALSE) %>%
  ggplot(aes(TOTAL_LENGTH, WEIGHT)) +
  geom_point()+
  facet_wrap(vars(SPECIES))
d %>%
  filter(SPECIES %in% c("RBT", "BNT", "NFC") == FALSE) %>%
  ggplot(aes(FORK_LENGTH, WEIGHT)) +
  geom_point()+
  facet_wrap(vars(SPECIES))

d <- d %>%
  mutate(total_minus_fork = TOTAL_LENGTH - FORK_LENGTH)


#Fix fish errors ############
d <- d %>%
  mutate(TOTAL_LENGTH = case_when(ID == "ID_20200312_2132_58_596_bb992dd2" ~ as.integer(423),
                                  ID == "ID_20200310_2206_22_081_87b62916" ~ as.integer(114),
                                  TRUE ~ TOTAL_LENGTH),

         WEIGHT = case_when(ID == "ID_20200310_2216_45_048_9b94578c" ~ NA_integer_,
                            ID == "ID_20200310_2221_08_713_8a637530" ~ as.integer(38),
                            TRUE ~ WEIGHT))



#CHECK for duplicate tag entries (i.e., new fish measured before previous fish
#was saved
#Not all duplicates are errors, fish may have been captured 2x - check datetimes
dup.tags <- d %>%
  filter(is.na(FISH_TAG) == FALSE & FISH_TAG != "" &
           duplicated(FISH_TAG))


#fix errors in sex and condition
d <- d %>%
  #cannot have ripe fish of unknown sex - ripe must be error
  mutate(SEX_COND = case_when(SEX == "U" & SEX_COND == "R" ~ "N",
  #also G (gravid?) occasionally recorded for unknown fish?!
                              SEX == "U" & SEX_COND == "G" ~ "N",
                              TRUE ~ SEX_COND))

#format to match access files ###########
#assign numeric primary keys
#shoals uses strings of numbers and letters, access uses numeric primary keys
d <- d %>%
  arrange(SAMPLE_ID, S_START_DATE_TIME) %>% #order by sample id and end time
  #create numeric ids by converting existing id fields to numeric
  mutate(ACCESS_SAMPLE_ID = as.numeric(factor(S_SITE_ID)),
         ACCESS_FISH_ID = row_number())

#look at your sample size - makes sense?
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
            SIDE_CODE = S_SIDE,
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



#save to ~/data/raw
write.csv(fish, "./output_data/LF20200310_fish.csv", row.names = FALSE)
write.csv(site, "./output_data/LF202000310_site.csv", row.names = FALSE)
write.csv(d, "./output_data/LF202000310_shoals_export_errorchecked.csv", row.names = FALSE)


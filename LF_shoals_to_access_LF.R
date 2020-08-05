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

current.year <- 2020 #define current year
trip.id <- "LF20200715"

#current.year.directory <- paste(
#  "\\\\flag-server/Office/Lees Ferry/Database files",
#  current.year, sep = "/")

#working at home because COVID, no server access
current.year.directory <- "C:/Users/jboyer/Documents/data"

file.name <- "LF20200715_shoals_export_edited.tsv"

d <- read.delim(paste(current.year.directory, file.name, sep = "/"),
                header = TRUE, stringsAsFactors = FALSE) #load shoals data
colnames(d)

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
  mutate(S_START_DATE_TIME = as.POSIXct(S_START_DATE_TIME)) %>%
  mutate_at(vars(ends_with("READER_TIME")),
            as.POSIXct,
            format = "%m/%d/%Y %H:%M:%S") %>%
  mutate(S_TRIP_ID = trip.id) #fix any trip id errors

#check that each column is correct data class
glimpse(d)

#fix errors with site ID
d <- d %>%
  mutate(S_SITE_ID = case_when(S_SITE_ID == "-13.95" ~"-13.95R",
                               S_SITE_ID == "-9.17L" ~"-9.17R",
                               S_SITE_ID == "-7.63L" ~"-7.63R",
                               S_SITE_ID == "--7.19L" ~ "-7.19R",
                               TRUE ~ S_SITE_ID))

d <- d %>%
  mutate(S_SIDE = case_when(S_SIDE == "L(LEFT)" ~ "L", TRUE ~ S_SIDE))

#add start, end RM and side info (by joining with site id)
site.list <- site.list %>%
  select(S_SITE_ID, rm_start, rm_end, side)

#join site info to shoals data
d <- d %>%
  left_join(site.list)

#assign start/end RM and side
d <- d %>%
  mutate(S_RM_START = case_when(
    #If start RM entered (nonnative sites), keep existing value
    !is.na(S_RM_START) ~ S_RM_START,
    #For monitoring sites, start RM from joined site table
    !is.na(rm_start) ~ rm_start,
    #RM often not recorded for slough. If missing for slough sites, add RM
    #useful to have RM for mapping locations of nonnatives, do not want
    # to leave blank(this RM is a center point of slough)
    is.na(S_RM_START) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ -12.15),
    S_RM_END = case_when( #do the same for end RM
      !is.na(S_RM_END) ~ S_RM_END,
      !is.na(rm_end) ~ rm_end,
      is.na(S_RM_END) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ -12.15),
    S_SIDE = case_when( #do the same for end RM
      !is.na(S_SIDE) ~ S_SIDE,
      !is.na(side) ~ side,
      is.na(S_SIDE) & grepl("SLOUGH", S_SITE_ID, fixed = TRUE) ~ "L")) %>%
  select(-c(rm_start, rm_end, side)) #no longer needed, remove


#check for date/datetime or river mile errors
#on this graph, samples should progress downstream and onward in time in a
#logical manner. If any sample sites are by themselves, check for errors

#check for volts/amps errors and fix
d <- d %>%
  mutate(S_EF_VOLTS = case_when(S_EF_VOLTS == 17.4 ~ 420, TRUE ~ S_EF_VOLTS),
         S_EF_AMPS = case_when(S_EF_AMPS == 420 ~17.4,
                               S_EF_AMPS == 167.0 ~ 16.7,
                               TRUE ~ S_EF_AMPS))

#check that sites proceed downstream over time in logical manner
#this will reveal datetime and river mile errors
d %>%
  ggplot(aes(x = S_START_DATE_TIME, y = S_RM_START)) +
  geom_point()


# COMBINE EF MINTUES AND SECONDS FOR TOTAL SECONDS
d <- d %>%
  mutate(EF_TOTAL_SECONDS = S_EF_MINUTES*60 + S_EF_SECONDS) %>%
  select(-c(S_EF_SECONDS, S_EF_MINUTES)) #no longer needed

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

#fix errors
#this was recorded as GSF but is definitely a trout (PIT tagged and ADP clip)
#and fork length seems more likely to be RBT than BNT
d <- d %>%
  mutate(SPECIES == case_when(ID == "ID_20200717_2048_38_415_3e6c66c5" ~ "RBT",
                              TRUE ~ SPECIES))


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
  mutate(FORK_LENGTH = as.numeric(FORK_LENGTH),
         TOTAL_LENGTH = as.numeric(TOTAL_LENGTH),
         WEIGHT = as.numeric(WEIGHT)) %>%
  mutate(TOTAL_LENGTH = case_when(
    ID == "ID_20200717_0113_30_194_a337bd7a" ~ 334,
    TRUE ~ TOTAL_LENGTH),
    FORK_LENGTH = case_when(
    ID == "	ID_20200715_2142_19_565_1b885a65" ~ 587,
    ID == "ID_20200715_2142_19_565_1b885a65" ~ 277,
    TRUE ~ FORK_LENGTH),
         WEIGHT = case_when(
           ID == "ID_20200718_2052_13_810_33314fea" ~ as.numeric(NA),
           ID == "ID_20200715_2108_55_311_13ce0ee8" ~ as.numeric(NA),
           TRUE ~ WEIGHT)) %>%
  select(-total_minus_fork) #no longer needed


#format to match access files


#save to ~/data/raw
write.csv(f, "./data/raw/fish_shoals.csv", row.names = FALSE)
write.csv(s, "./data/raw/site_shoals.csv", row.names = FALSE)

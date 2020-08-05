#Quick summaries of Lees Ferry electrofishing data
#intended for use in field to quickly review progress and errors

require(tidyverse)

filepath <- "C:/dasa/data/AGFD_EF/AGFD_EF_v6/"
filename <- "LF20200715_AGFD_EF_v6_Export.txt"
trip.id = "LF20200715"
night1 = as.Date(substr(trip.id, 3, 10), format = "%Y%m%d")


fish <- read_tsv(paste0(filepath, filename), col_names = TRUE)

#remove unneeded columns
fish <- fish %>%
  select_if(~!all(is.na(.))) %>% #remove rows with only NA
  select(-ends_with("_DESC")) %>% #descriptions duplicate info in codes
 # select(-starts_with("FISH_TAG_")) %>%
  select(-c(RECMOD, S_END_DATE_TIME))
glimpse(fish)


#extract date, and assign day
fish <- fish %>%
  mutate(S_START_DATE_TIME = as.POSIXct(S_START_DATE_TIME,
                                   format = "%m/%d/%Y %H:%M"),
         S_date = as.Date(substr(as.character(S_START_DATE_TIME), 1, 10)),
         hour = as.numeric(substr(as.character(S_START_DATE_TIME), 12, 13))) %>%
  #assign sampling night (i.e., deal with sites fished after midnight)
  mutate(S_night = as.numeric(
    case_when(hour > 6 ~ 1 + difftime(S_date, night1, units = "days"),
                           hour < 6 ~ difftime(S_date, night1, units = "days")))) %>%
  select(-hour) #no longer needed, remove

glimpse(fish)

# make corrections ###########
fish <- fish %>%
  mutate(S_SITE_ID = toupper(S_SITE_ID)) %>%
  #reclassify all slough sites as 120 (nonnative) 3 incorrectly coded as 96
  mutate(S_SAMPLE_TYPE = case_when(S_SITE_ID %in% c("SLOUGH 1A", "SLOUGH 1B",
                                                    "SLOUGH 1C") ~ 120,
                                   TRUE ~ S_SAMPLE_TYPE))

#sites fished after midnight on day 2 had wrong date recorded
# need to add a day
fish <- fish %>%
  mutate(S_START_DATE_TIME = case_when(
    S_SITE_ID %in%  c("-11.72L", "-11.02R", "-10.92L", "-10.8L") ~
     S_START_DATE_TIME + 24*60*60, #POSIXct is seconds, add n seconds in 1 day
    TRUE ~ S_START_DATE_TIME)) %>%
    #rerun code to extract date and night from datetime, now datetime is fixed
 mutate(S_date = as.Date(substr(as.character(S_START_DATE_TIME), 1, 10)),
        hour = as.numeric(substr(as.character(S_START_DATE_TIME), 12, 13))) %>%
#assign sampling night (i.e., deal with sites fished after midnight)
mutate(S_night = as.numeric(
       case_when(hour > 6 ~ 1 + difftime(S_date, night1, units = "days"),
                 hour < 6 ~ difftime(S_date, night1, units = "days")))) %>%
      select(-hour) #no longer needed, remove

#	-4.76L being read as two sites because sample comment added to only 1 specimen
# record. Add that comment to all speciments in 	-4.76L
fish <- fish %>%
  mutate(S_SAMPLE_NOTES = case_when(S_SITE_ID == "-4.76L" ~ "1 BEAVER",
                                    TRUE ~ S_SAMPLE_NOTES))

#	--7.19L recorded instead of 	-7.19L
fish <- fish %>%
  mutate(S_SAMPLE_NOTES = case_when(S_SITE_ID == "-4.76L" ~ "1 BEAVER",
                                    TRUE ~ S_SAMPLE_NOTES))


#extract sample data
site <- fish %>%
  select(starts_with("S_")) %>%
  unique()

#how many sites sampled each night?
site %>%
  group_by(S_night, S_SAMPLE_TYPE) %>%
  summarise(n = n())


#species count
species.by.day <- fish %>%
  group_by(SPECIES, S_night) %>%
  summarize(n = n())
species <- fish %>%
  group_by(SPECIES) %>%
  summarize(n = n())

write.table(fish, sep='\t',
            "C:/Users/jboyer/Documents/data/LF20200715_shoals_export_edited.tsv",
            row.names = FALSE)

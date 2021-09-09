#quick CPUE calculation and summaries for trip reports

# setup - load data and packages ###############################################
library(tidyverse)

s <- read.csv("./output_data/LF20210308_site.csv", stringsAsFactors = FALSE)
f <- read.csv("./output_data/LF20210308_fish.csv", stringsAsFactors = FALSE)

# add length, condition, size class ############################################
#calculate TL from FL for years with only FL
#add TL column - TL if available, calculated TL's if FL only was measured
#equations are from downstream annual report
f$TL <- ifelse(is.na(f$TOTAL_LENGTH) == FALSE, f$TOTAL_LENGTH,
               ifelse(f$SPECIES_CODE == "BNT",
                      round(8.211 + 1.024*f$FORK_LENGTH, 0),
                      ifelse(f$SPECIES_CODE == "RBT",
                             round(1.9557129 + 1.0643074*f$FORK_LENGTH, 0),
                             ifelse(f$SPECIES_CODE == "CRP",
                                    round(6.266 + 1.100*f$FORK_LENGTH, 0),
                                    NA))))

#calculate relative condition factor (Kn)
f$kn <- ifelse(f$SPECIES_CODE == "RBT",
               f$WEIGHT/(10^(-4.6023 + 2.8193*log10(f$TL))),
               NA)

#assign size class to RBT and BNT
f$size_class <- factor(ifelse(f$SPECIES_CODE == "BNT",
                              as.character(cut(f$TL, breaks = c(0, 200, 350, Inf))),
                              ifelse(f$SPECIES_CODE == "RBT",
                                     as.character(cut(f$TL, breaks = c(0, 151, 305, 405, Inf))),
                                     NA)))
levels(f$size_class) <- c("<152", "<200", "152-305", "200-350", #rename levels
                          "306-405", ">350", ">405")
f$size_class <- as.character(f$size_class)

# CALCULATE CPUE at each site ##################################################
#first by species
#than by size class

# define levels of SPECIES_CODE to include all species caught
all.sp <- unique(f$SPECIES_CODE)
all.sp <- all.sp[all.sp != "" & all.sp != "NFC"]

# calculate counts of each species
sp.counts <- as.data.frame.matrix(table(f$ACCESS_SAMPLE_ID, f$SPECIES_CODE))
# unique_site_id column became rownames, add it as a column again
sp.counts$ACCESS_SAMPLE_ID <- rownames(sp.counts)
sp.counts$NFC <- NULL  # remove NFC column, "No Fish Caught" is not a species

########## calculate CPUE for each species
# merge effort into sp.counts dataframe

#calculate minutes for electrofishing effort
s$effort_minutes <- s$EF_TOTAL_SECONDS/60

effort <- s[, c("ACCESS_SAMPLE_ID", "effort_minutes")] #extract effort from s

sp.counts <- merge(sp.counts, effort, by = "ACCESS_SAMPLE_ID", all.x = TRUE)

cpue <- data.frame(sp.counts[ ,1], # unique_sample_id column
                   # calculate CPUE columns for each species
                   sweep(sp.counts[ ,2:(ncol(sp.counts) - 1)],
                         sp.counts[,"effort_minutes"], MARGIN = 1,"/"))

colnames(cpue) <- c("ACCESS_SAMPLE_ID",
                    # use paste to add "CPUE_" to each species code
                    paste("CPUE_", colnames(cpue[2:ncol(cpue)]), sep = ""))

sp.counts$effort_minutes <- NULL

#subset to just bnt and rbt - don't need others for this analysis
sp.counts <- sp.counts[, c("BNT", "RBT", "ACCESS_SAMPLE_ID")]
cpue <- cpue[, c("CPUE_BNT", "CPUE_RBT", "ACCESS_SAMPLE_ID")]

# merge catch and cpue dataframes onto site dataframe
s <- merge(s, sp.counts, by = "ACCESS_SAMPLE_ID")
s <- merge(s, cpue, by = "ACCESS_SAMPLE_ID")

###########calculate cpue by size class

f$group <- as.factor(paste(f$SPECIES_CODE, f$size_class, sep = "")) #variable with sp and size class
# define levels of SPECIES_CODE to include all species caught
all.groups <- levels(f$group)
all.groups <- all.groups[all.groups != "all" & all.groups != "NFCall"]

# calculate counts of each species
group.counts <- as.data.frame.matrix(table(f$ACCESS_SAMPLE_ID, f$group))
# unique_site_id column became rownames, add it as a column again
group.counts$ACCESS_SAMPLE_ID <- rownames(group.counts)
group.counts$NFCall <- NULL  # remove NFC column, "No Fish Caught" is not a species

# calculate CPUE for each species
# merge effort into group.counts dataframe

#calculate hours for electrofishing and angling, since their effort data is
#recorded in seconds and minutes in the database

group.counts <- merge(group.counts, effort, by = "ACCESS_SAMPLE_ID", all.x = TRUE)

cpue.group <- data.frame(group.counts[ ,1], # unique_sample_id column
                         # calculate CPUE columns for each species
                         sweep(group.counts[ ,2:(ncol(group.counts) - 1)],
                               group.counts[,"effort_minutes"], MARGIN = 1,"/"))

colnames(cpue.group) <- c("ACCESS_SAMPLE_ID",
                          # use paste to add "CPUE_" to each species code
                          paste("CPUE_", colnames(cpue.group[2:ncol(cpue.group)]),
                                sep = ""))

group.counts$effort_minutes <- NULL
colnames(cpue.group)
colnames(group.counts)

group.counts <- group.counts[, c("BNT<200", "BNT>350", "BNT200-350",
                                 "RBT<152", "RBT152-305", "RBT306-405",
                                 "RBT>405", "ACCESS_SAMPLE_ID")]
cpue.group <- cpue.group[, c("CPUE_BNT.200", "CPUE_BNT.350", "CPUE_BNT200.350",
                             "CPUE_RBT.152", "CPUE_RBT152.305", "CPUE_RBT306.405",
                             "CPUE_RBT.405", "ACCESS_SAMPLE_ID")]

# merge catch and cpue dataframes onto site dataframe
s <- merge(s, group.counts, by = "ACCESS_SAMPLE_ID")
s <- merge(s, cpue.group, by = "ACCESS_SAMPLE_ID")

rm(cpue, cpue.group, effort, group.counts, sp.counts)

#add useful time columns (year, season, month, etc) ############################
s$START_DATETIME <- as.POSIXct(s$START_DATETIME) #code datetimes as posixct
s$START_DATE <- as.Date(format(s$START_DATETIME, "%Y-%m-%d")) # add just date

#add a reach column #############################################################
s$reach <- as.character(cut(s$START_RM, breaks = c(-15.31, -10.8, - 4.0, -0.3)))

#find and remove duplicate records ##############################################

f <- f[f$SPECIES_CODE != "NFC",]

#join site data to fish
site.small <- s %>%
  select(ACCESS_SAMPLE_ID, DATASHEET_SAMPLE_ID, START_RM, START_DATETIME, START_DATE, SAMPLE_TYPE)

f <- f %>%
  left_join(site.small)


#cpue summaries by year for each species/size class ###########################
#subset monitoring data only (remove supplemental and non-native hunt)
#2009,2010 SAMPLE_TYPE codes missing/incorrect, keep those years
s <- s[s$SAMPLE_TYPE == 96 ,] # remove supplemental data

#subset to only neccessary columns
#s.cpue for calculating cpues, s.n for sample size of fish

s.cpue <- s %>%
  select(ACCESS_SAMPLE_ID,
         CPUE_RBT, CPUE_RBT.152, CPUE_RBT152.305, CPUE_RBT306.405, CPUE_RBT.405,
         CPUE_BNT)

s.n <- s %>%
  select(ACCESS_SAMPLE_ID,
         RBT, `RBT<152`, `RBT152-305`, `RBT306-405`, `RBT>405`, BNT)

colnames(s.n) = c("ACCESS_SAMPLE_ID",
                  "RBT", "RBT.152", "RBT152.305", "RBT306.405", "RBT.405", "BNT")

#pivot dataframe to long format
s.cpue <- s.cpue %>%
  pivot_longer(starts_with("CPUE"), #columns to transform to long
               names_to = "SPECIES_CODE",
               values_to = "CPUE")

s.n <- s.n %>%
  pivot_longer(!ACCESS_SAMPLE_ID, #all cols except this one
               names_to = "SPECIES_CODE",
               values_to = "n")

# remove "CPUE_" from species codes (i.e, CPUE_BHS becomes BHS)
s.cpue$SPECIES_CODE <- gsub("CPUE_", "", s.cpue$SPECIES_CODE)


#calculate cpue by year for each species/size class #########

cpue <- s.cpue %>%
  group_by(SPECIES_CODE) %>%
  summarize(mean_cpue = signif(mean(CPUE, na.rm = TRUE), 3),
            n.sites = length(SPECIES_CODE),
            stdev = sd(CPUE, na.rm = TRUE),
            ci_95 = qnorm(0.975)*stdev/sqrt(n.sites),
            ci.low = signif(mean_cpue - ci_95, 3),
            ci.high = signif(mean_cpue + ci_95, 3)) %>%
  mutate(ci.low = case_when(ci.low < 0 ~ 0, TRUE ~ ci.low))


#total counts
fish.counts <- f %>%
  group_by(SAMPLE_TYPE, SPECIES_CODE) %>%
  summarise(n = n())

#total fish captured
sum(fish.counts$n)

#number of recaps
recaps <- f %>%
  group_by(SPECIES_CODE, PITTAG_RECAP) %>%
  summarize(n = n())

#length freq histogram
p.length <- f %>%
  filter(SPECIES_CODE %in% c("RBT", "BNT")) %>%
  mutate(species = case_when(SPECIES_CODE == "RBT" ~ "Rainbow Trout",
                             TRUE ~ "Brown Trout")) %>%
  ggplot(aes(x = TL)) +
  theme_minimal() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(breaks = seq(0, 20, by = 5)) +
  scale_x_continuous("Total Length (mm)") +
  geom_histogram(binwidth = 10) +
  facet_grid(vars(species), scales = "free", space = "free")
p.length

ggsav
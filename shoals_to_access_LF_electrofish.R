#loads data entered directly into field computers (from shoals program),
#formats it to match database data, and saves as .csv
#inputs: .txt file with data export from shoals
#        (no format changes from program output!)
#outputs: site_shoals.csv
#         fish_shoals.csv
#        shoals data formatted to match format of database data
#        saved in ./data/raw

current.year <- 2019 #define current year

file.name <- "GC20190311_shoals_export.txt"

# specify location of file
current.year.directory <- paste(
  "\\\\flag-server/Office/Lees Ferry/Database files",
  current.year, sep = "/")

f <- read.delim(paste(current.year.directory, file.name, sep = "/"),
              header = TRUE) #load shoals data

#remove test data entries: not
f <- f[f$S_SAMPLE_NOTES != "test",]


colnames(f)[1] <- "SPECIMEN_ID"

#subset to needed columns
keeps <- c("SPECIMEN_ID", "FISH_TAG", "S_SAMPLE_TYPE", "S_SAMPLE_NOTES",
           "SPECIMEN_NOTES", "S_CLIPBOARD", "S_TEMPERATURE", "S_TURBIDITY_NTU",
           "S_SITE_ID", "S_RIVER_MILE", "S_SIDE", "S_TURBIDITY",
           "S_GEAR", "S_SET_NUM", "S_TRIP_ID",
           "S_SIDE", "S_RIVER_MILE", "S_END_DATE_TIME",
           "S_START_DATE_TIME", "RECAP_YES_NO", "WEIGHT", "FORK_LENGTH",
           "SEX_CHAR", "SEX_COND", "DISPOSITION",
           "SEX",  "TOTAL_LENGTH", "SPECIES")
f <- f[,keeps]


#rename columns to match database column names
colnames(f) <- c("ACCESS_FISH_ID", "PITTAG", "SAMPLE_TYPE", "SAMPLE_NOTES",
                 "SPECIMEN_NOTES", "CLIPBOARD", "WATER_TEMP", "TURBIDITY_NTU",
                 "DATASHEET_SAMPLE_ID", "START_RM", "SIDE_CODE","TURBIDITY",
                 "GEAR_CODE", "EL_SECONDS", "TRIP_ID",
                 "SIDE_CODE", "RIVER_MILE", "END_DATETIME",
                 "START_DATETIME", "RECAP_YES_NO", "WEIGHT", "FORK_LENGTH",
                 "SEX_CHAR_CODE", "SEX_COND_CODE", "DISPOSITION_CODE",
                 "SEX_CODE",  "TOTAL_LENGTH", "SPECIES_CODE")


#recap info is in different format in shoals (PIT and finclip combined) vs.
#database (PIT and finclip separate). Make column that matches database format
f$PITTAG_RECAP <- ifelse(f$RECAP_YES_NO == "PY" | f$RECAP_YES_NO == "BY", "Y",
                         ifelse(f$RECAP_YES_NO == "BN" | f$RECAP_YES_NO == "FY",
                                "N", NA))

f$FINCLIP1_RECAP <- ifelse(f$RECAP_YES_NO == "FY" | f$RECAP_YES_NO == "BY", "Y",
                         ifelse(f$RECAP_YES_NO == "PY" | f$RECAP_YES_NO == "BN",
                                "N", NA))


#format dates as dates
f$START_DATETIME <- as.POSIXct(f$START_DATETIME, format = "%m/%d/%Y %H:%M:%S")
f$END_DATETIME <- as.POSIXct(f$END_DATETIME, format = "%m/%d/%Y %H:%M:%S")


#convert effort from mm:ss to seconds
#mm:ss conversion subtract numeric 0 seconds from numeric mm:ss
f$EF_TOTAL_SECONDS <- as.numeric(as.POSIXct(strptime(f$"EL_SECONDS",
                                                     format = "%M:%S"))) -
    as.numeric(as.POSIXct(strptime("0", format = "%S")))

#remove columns not in database
f$RECAP_YES_NO <- NULL
f$RIVER_MILE <- NULL
f$EL_SECONDS <- NULL

f$START_RM <- round(f$START_RM, 2)
f$DATASHEET_SAMPLE_ID <- toupper(f$DATASHEET_SAMPLE_ID)

#extract site info only to make site (s) dataframe
s <- f #copy dataframe with new name

#subset to only site columns, no specimen columns
s.keeps <- c("SAMPLE_TYPE", "SAMPLE_NOTES", "CLIPBOARD", "WATER_TEMP",
             "TURBIDITY_NTU", "DATASHEET_SAMPLE_ID", "START_RM", "GEAR_CODE",
             "TRIP_ID", "SIDE_CODE", "END_DATETIME", "START_DATETIME", "EF_TOTAL_SECONDS")
s <- s[, s.keeps]

s <- unique(s) #keep only rows that are unique (remove duplicates)
s[duplicated(s$DATASHEET_SAMPLE_ID),]


# remove certain site columns from f to match database columns
drops <- c("SAMPLE_NOTES", "WATER_TEMP", "TURBIDITY_NTU",
           "TURBIDITY", "TOTAL_HOURS")
f <- f[, !names(f) %in% drops]

#save to ~/data/raw
write.csv(f, "./data/raw/fish_shoals.csv", row.names = FALSE)
write.csv(s, "./data/raw/site_shoals.csv", row.names = FALSE)

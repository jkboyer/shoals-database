# shoals-database
R code to convert data exported from the shoals data entry program into a format that is compatible with the big boy database

These R scripts load data exports from SHOALS program (tab separated .txt files), formats data to match big boy database data, and saves data as two .csv files (one for SAMPLE, one for SPECIMEN) that are ready to import into access data entry template

They were written for AGFD data, and will require minor editing (i.e., add columns that your project uses but AGFD projects don't, some projects record PIT and finclip in RECAP_YES_NO, others only record PIT recap info) to work for other agencies or projects.

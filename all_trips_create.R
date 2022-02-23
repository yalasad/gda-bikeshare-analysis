# This R script is the code I executed to merge, aggregate, filter, clean, and amend the data so that it is ready for analysis and visualisation.
# Google Data Analytics Professional Certificate -- Capstone course

# load libraries
library(tidyverse)

# load datasets
feb_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202102-divvy-tripdata.csv")
mar_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202103-divvy-tripdata.csv")
apr_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202104-divvy-tripdata.csv")
may_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202105-divvy-tripdata.csv")
jun_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202106-divvy-tripdata.csv")
jul_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202107-divvy-tripdata.csv")
aug_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202108-divvy-tripdata.csv")
sep_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202109-divvy-tripdata.csv")
oct_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202110-divvy-tripdata.csv")
nov_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202111-divvy-tripdata.csv")
dec_2021_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202112-divvy-tripdata.csv")
jan_2022_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/divvy-tripdata/202201-divvy-tripdata.csv")


# creating one large dataset: the union dataset
all_trips <- rbind(feb_2021_trips, mar_2021_trips, apr_2021_trips, may_2021_trips, jun_2021_trips, jul_2021_trips, aug_2021_trips, sep_2021_trips, oct_2021_trips, nov_2021_trips, dec_2021_trips, jan_2022_trips)


# exploring the union dataset
# realize time columns are chr type
View(all_trips)
colnames(all_trips)
str(all_trips)
dim(all_trips)


# converting columns to the necessary type to perform calculations
all_trips$ended_at <- as.POSIXct(all_trips$ended_at)   
str(all_trips$ended_at)

all_trips$started_at <- as.POSIXct(all_trips$started_at)   
str(all_trips$started_at)


# adding trip_length column and removing any negative trips
all_trips$trip_length_s <- (all_trips$ended_at - all_trips$started_at)
all_trips$trip_length_m <- round((all_trips$ended_at - all_trips$started_at)/60,2)
all_trips <- filter(all_trips, trip_length_m >= 0)

# adding a month column
all_trips$month <- format(all_trips$started_at, "%m")
# Convert month column from chr to num
str(all_trips$month)
all_trips$month <- as.numeric(all_trips$month)


# adding a day column
all_trips$day <- weekdays(all_trips$started_at)

# adding column to change weekday into number
# Monday == 1
all_trips$dayno <- 01
all_trips$dayno[ all_trips$day == "Tuesday" ] <- 02
all_trips$dayno[ all_trips$day == "Wednesday" ] <- 03
all_trips$dayno[ all_trips$day == "Thursday" ] <- 04
all_trips$dayno[ all_trips$day == "Friday" ] <- 05
all_trips$dayno[ all_trips$day == "Saturday" ] <- 06
all_trips$dayno[ all_trips$day == "Sunday" ] <- 07


# adding a season column
# since this is US data:
# spring: [ March - May ]
# summer: [ June - August ]
# fall: [ September - Nov ]
# winter: [ Dec - Feb ]
all_trips$season <- "Winter"
all_trips$season[ all_trips$month >= 03 & all_trips$month <= 05 ] <- "Spring"
all_trips$season[ all_trips$month >= 06 & all_trips$month <= 08 ] <- "Summer"
all_trips$season[ all_trips$month >= 09 & all_trips$month <= 11] <- "Fall"


# Export all_trips data frame
write.csv(all_trips, "~/Desktop/goog_dta_analysis/case_study1/all_trips.csv")


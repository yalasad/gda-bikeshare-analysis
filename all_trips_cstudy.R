# This R script is the code I execute to analyze and visualize the data to complete the case study
# Google Data Analytics Professional Certificate -- Capstone course

# load libraries
library(tidyverse)
#library(lubridate)


all_trips <- read.csv("~/Desktop/goog_dta_analysis/case_study1/all_trips.csv")
View(all_trips)

# By member_casual
all_trips_rider <- all_trips %>%
  group_by(member_casual) %>%
  summarise(rides = n())

View(all_trips_rider)

# By bike type
bike_type_member <- all_trips %>%
  filter(member_casual == "member") %>%
  group_by(rideable_type) %>%
  summarise(rides = n(), member_casual = "Member")

bike_type_member$ride_share <- round(bike_type_member$rides/sum(bike_type_member$rides),4)

View(bike_type_member)

bike_type_casual <- all_trips %>%
  filter(member_casual == "casual") %>%
  group_by(rideable_type) %>%
  summarise(rides = n(), member_casual = "Casual")

bike_type_casual$ride_share <- round(bike_type_casual$rides/sum(bike_type_casual$rides),4)

View(bike_type_casual)


# Merge bike_type into one table and plot it
bike_type <- rbind(bike_type_casual, bike_type_member)

View(bike_type)

ggplot(bike_type, aes(x="", y= ride_share, fill = rideable_type))+
  geom_bar(width = 1, stat = "identity") + labs(title = "Rides by BIKE TYPE for each rider type" , x= "Rider Type", y= "Percent of Rides") + facet_wrap(~member_casual)

ggsave("~/Desktop/goog_dta_analysis/case_study1/bike_type.png")


# By day
ggplot(data = all_trips) + geom_bar(mapping = aes(x=dayno, fill = day)) + facet_wrap(~member_casual) + labs(title = "Trip count by DAY for each rider type" , subtitle = "Monday is day one", x="Day of the Week", y= "Trip Count")

ggsave("~/Desktop/goog_dta_analysis/case_study1/day_number.png")


# By season
ggplot(data = all_trips) + geom_bar(mapping = aes(x= season, fill = season)) + facet_wrap(~member_casual) + labs(title = "Trip count by SEASON for each rider type", x="Season", y= "Trip Count")

ggsave("~/Desktop/goog_dta_analysis/case_study1/season.png")


# By month
ggplot(data = all_trips) + geom_bar(mapping = aes(x= month, fill = season)) + facet_wrap(~member_casual) + labs(title = "Trip count by MONTH for each rider type", substitle = "Bars colour is grouped by season", y= "Trip Count") + scale_x_discrete(name ="Month", limits=c("1","2","3","4","5","6","7","8","9","10","11","12"))

ggsave("~/Desktop/goog_dta_analysis/case_study1/month.png")


# By trip_length
trip_length <- all_trips %>%
  group_by(member_casual) %>%
  summarise(mean = mean(trip_length_m), sd = sd(trip_length_m), max = max(trip_length_m), min = min(trip_length_m), quantile(trip_length_m), median = median(trip_length_m))

View(trip_length)

# Removing outlier data (>75th percentile)
all_trips_75q <- all_trips %>%
  group_by(member_casual) %>%
  filter(trip_length_m <= quantile(trip_length_m, 0.75))

View(all_trips_75q)

trip_length_75q <- all_trips_75q %>%
  group_by(member_casual) %>%
  summarise(mean = mean(trip_length_m), sd = sd(trip_length_m), max = max(trip_length_m), min = min(trip_length_m), quantile(trip_length_m), median = median(trip_length_m))

View(trip_length_75q)

ggplot(data = all_trips_75q) + geom_boxplot(mapping = aes(x= member_casual, y= trip_length_m, fill = member_casual)) + labs(title = "Trip LENGTH for each rider type", x= "Rider Type", y= "Trip Length in Minutes")

ggsave("~/Desktop/goog_dta_analysis/case_study1/trip_length.png")


# By start station
all_trips_start <- all_trips %>%
  filter(start_station_id != "") %>%
  group_by(member_casual, start_station_id, start_station_name) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides)) %>%
  group_by(member_casual) %>%
  slice(1:10)

View(all_trips_start)

atsm <- filter(all_trips_start, member_casual == "member")
atsc <- filter(all_trips_start, member_casual == "casual")
ggplot(atsm, aes(x= reorder(start_station_name, -rides), y= rides, fill = start_station_name)) + geom_bar( stat = "identity", width = 0.5) + theme(axis.text.x = element_text (angle = 65)) + labs(title = "Top START STATION for member riders", x= "Start Station Name", y= "Number of Rides") + theme(legend.position="none")
ggsave("~/Desktop/goog_dta_analysis/case_study1/start_station_member.png")
ggplot(atsc, aes(x= reorder(start_station_name, -rides), y= rides, fill = start_station_name)) + geom_bar( stat = "identity", width = 0.5) + theme(axis.text.x = element_text (angle = 65)) + labs(title = "Top START STATION for casual riders", x= "Start Station Name", y= "Number of Rides") + theme(legend.position="none")
ggsave("~/Desktop/goog_dta_analysis/case_study1/start_station_casual.png")

all_trips_start_count <- all_trips %>%
  filter(start_station_id != "") %>%
  group_by(member_casual) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))

View(all_trips_start_count)

# By end station
all_trips_end <- all_trips %>%
  filter(end_station_id != "") %>%
  group_by(member_casual, end_station_id, end_station_name) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides)) %>%
  group_by(member_casual) %>%
  slice(1:10)

View(all_trips_end)

atem <- filter(all_trips_end, member_casual == "member")
atec <- filter(all_trips_end, member_casual == "casual")
ggplot(atem, aes(x= reorder(end_station_name, -rides), y= rides, fill = end_station_name)) + geom_bar( stat = "identity", width = 0.5) + theme(axis.text.x = element_text (angle = 65)) + labs(title = "Top END STATION for member riders", x= "End Station Name", y= "Number of Rides") + theme(legend.position="none")
ggsave("~/Desktop/goog_dta_analysis/case_study1/end_station_member.png")
ggplot(atec, aes(x= reorder(end_station_name, -rides), y= rides, fill = end_station_name)) + geom_bar( stat = "identity", width = 0.5) + theme(axis.text.x = element_text (angle = 65)) + labs(title = "Top END STATION for casual riders", x= "End Station Name", y= "Number of Rides") + theme(legend.position="none")
ggsave("~/Desktop/goog_dta_analysis/case_study1/end_station_casual.png")

all_trips_end_count <- all_trips %>%
  filter(end_station_id != "") %>%
  group_by(member_casual) %>%
  summarise(rides = n()) %>%
  arrange(desc(rides))

View(all_trips_end_count)

# For further analysis
all_trips_station <- rbind(all_trips_start, all_trips_end)
write.csv(all_trips_station, "~/Desktop/goog_dta_analysis/case_study1/all_trips_station.csv")

"
  Ways to look at member_casual differences:
    By bike type      DONE
    By month          DONE
    By day            DONE
    By season         DONE
    By trip length    DONE
    By start station  DONE
    By end station    DONE
"

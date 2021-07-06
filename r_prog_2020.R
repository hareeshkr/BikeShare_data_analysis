library(tidyverse)

#load the csv to data frame
trips_q1 <- read_csv("Divvy_Trips_2020_Q1.csv")

#just look into the data frame
head(trips_q1)
colnames(trips_q1)
View(trips_q1)

#check the ride_able type and if there is only one type we can remove that column
unique(trips_q1$rideable_type)

#calculate riding time and create a new column for that and store the new data to a new data frame
trips_q1 <- trips_q1 %>% 
  select(ride_id, start_station_name, end_station_name, started_at, ended_at, member_casual) %>% 
  mutate(time_of_ride = difftime(ended_at, started_at, units = "mins"))

#clean all data having trip time less than a minute
trips_q1 <- subset(trips_q1, time_of_ride>1)

#combine the station start and end station to a new column
trips_q1$trip_stations <- paste(trips_q1$start_station_name, 'to', trips_q1$end_station_name)

#final data frame with necessary data of the quarter
trips_q1 <- trips_q1 %>% 
  select(ride_id,trip_stations,started_at,ended_at,time_of_ride,member_casual)

# Load 4, 5, and 6 months data
trips_m04 <- read_csv("202004-divvy-tripdata.csv")
trips_m05 <- read_csv("202005-divvy-tripdata.csv")
trips_m06 <- read_csv("202006-divvy-tripdata.csv")

#bind these to quarter 2 data frame and looks to the data
trips_q2 <- rbind(trips_m04, trips_m05, trips_m06)
View(trips_q2)
colnames(trips_q2)

#check ride_able type uniqueness
unique(trips_q2$rideable_type)

#found out there is only one ride able type (docked_bike), now we can remove unnecessary columns
#like rideable_type, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng
#also find out the time of rides
trips_q2 <- trips_q2 %>% 
  select(ride_id, start_station_name, end_station_name, started_at, ended_at, member_casual) %>% 
  mutate(time_of_ride = difftime(ended_at, started_at, units = "mins"))

#clean all data having trip time less than a minute
trips_q2 <- subset(trips_q2, time_of_ride>1)

#combine the station start and end station to a new column
trips_q2$trip_stations <- paste(trips_q2$start_station_name, 'to', trips_q2$end_station_name)

#final data frame with necessary data of the quarter
trips_q2 <- trips_q2 %>% 
  select(ride_id,trip_stations,started_at,ended_at,time_of_ride,member_casual)

rm("trips_m04", "trips_m05", "trips_m06")

# Load 7, 8, and 9 months data
trips_m07 <- read_csv("202007-divvy-tripdata.csv")
trips_m08 <- read_csv("202008-divvy-tripdata.csv")
trips_m09 <- read_csv("202009-divvy-tripdata.csv")

#bind these to quarter 3 data frame and looks to the data
trips_q3 <- rbind(trips_m07, trips_m08, trips_m09)
View(trips_q3)
colnames(trips_q3)

#check ride_able type uniqueness
unique(trips_q3$rideable_type)

#found out there are two ride able type (docked_bike & electric_bike), now we can remove unnecessary columns
#like start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng
#also find out the time of rides
trips_q3 <- trips_q3 %>% 
  select(ride_id, rideable_type, start_station_name, end_station_name, started_at, ended_at, member_casual) %>% 
  mutate(time_of_ride = difftime(ended_at, started_at, units = "mins"))

#clean all data having trip time less than a minute
trips_q3 <- subset(trips_q3, time_of_ride>1)

#combine the station start and end station to a new column
trips_q3$trip_stations <- paste(trips_q3$start_station_name, 'to', trips_q3$end_station_name)

#final data frame with necessary data of the quarter
trips_q3 <- trips_q3 %>% 
  select(ride_id, rideable_type, trip_stations, started_at, ended_at, time_of_ride, member_casual)

#remove all data frames which are already used and unwanted anymore
rm("trips_m07", "trips_m08", "trips_m09")


# Load 10, 11, and 12 months data
trips_m10 <- read_csv("202010-divvy-tripdata.csv")
trips_m11 <- read_csv("202011-divvy-tripdata.csv")
trips_m12 <- read_csv("202012-divvy-tripdata.csv")

#bind these to quarter 4 data frame and looks to the data
trips_q4 <- rbind(trips_m10, trips_m11, trips_m12)
View(trips_q4)
colnames(trips_q4)

#check ride_able type uniqueness
unique(trips_q4$rideable_type)

#found out there are three ride able type (docked_bike, electric_bike and classic_bike), now we can remove unnecessary columns
#like start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng
#also find out the time of rides
trips_q4 <- trips_q4 %>% 
  select(ride_id, rideable_type, start_station_name, end_station_name, started_at, ended_at, member_casual) %>% 
  mutate(time_of_ride = difftime(ended_at, started_at, units = "mins"))

#clean all data having trip time less than a minute
trips_q4 <- subset(trips_q4, time_of_ride>1)

#combine the station start and end station to a new column
trips_q4$trip_stations <- paste(trips_q4$start_station_name, 'to', trips_q4$end_station_name)

#final data frame with necessary data of the quarter
trips_q4 <- trips_q4 %>% 
  select(ride_id, rideable_type, trip_stations, started_at, ended_at, time_of_ride, member_casual)

#remove all data frames which are already used and unwanted anymore
rm("trips_m10", "trips_m11", "trips_m12")



trips_q1_sorted <- trips_q1 %>% 
  group_by(trip_stations,member_casual) %>% 
  summarise(length(member_casual))

# group by trip stations and member_casual to get number of members or casual riders on each trip stations
trips_q2_sorted <- trips_q2 %>% 
  group_by(trip_stations,member_casual) %>% 
  summarise(count_mem_cas = length(member_casual))
trips_q2_sorted <- subset(trips_q2_sorted, count_mem_cas>500)

trips_q3_sorted <- trips_q3 %>% 
  group_by(trip_stations,member_casual) %>% 
  summarise(count_mem_cas = length(member_casual))
trips_q3_sorted <- subset(trips_q3_sorted, count_mem_cas>1900)

trips_q4_sorted <- trips_q4 %>% 
  group_by(trip_stations,member_casual) %>% 
  summarise(count_mem_cas = length(member_casual))
trips_q4_sorted <- subset(trips_q4_sorted, count_mem_cas>400)


#finally export the quarter data to csv to further analyze in google spreadsheet
write.csv(trips_q4_sorted,"C:\\Users\\haree\\Desktop\\Bikeshare - Case_study\\trips_q4_2020.csv")



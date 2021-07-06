library(tidyverse)

#load the csv to data frame
trips_2019_q1 <- read_csv("Divvy_Trips_2019_Q1.csv")
trips_2019_q2 <- read_csv("Divvy_Trips_2019_Q2.csv")
trips_2019_q3 <- read_csv("Divvy_Trips_2019_Q3.csv")
trips_2019_q4 <- read_csv("Divvy_Trips_2019_Q4.csv")
#just look into the data frame
head(trips_2019_q1)
colnames(trips_2019_q1)

#rename necessary columns on q2 data frame
trips_2019_q2 <- trips_2019_q2 %>% 
  rename(tripduration = "01 - Rental Details Duration In Seconds Uncapped",
         from_station_name = "03 - Rental Start Station Name",
         to_station_name = "02 - Rental End Station Name",
         usertype = "User Type"
         )

#clean all data having trip time less than a minute
trips_2019_q1 <- subset(trips_2019_q1, tripduration>61)
trips_2019_q2 <- subset(trips_2019_q2, tripduration>61)
trips_2019_q3 <- subset(trips_2019_q3, tripduration>61)
trips_2019_q4 <- subset(trips_2019_q4, tripduration>61)

#combine the station start and end station to a new column
trips_2019_q1$trip_stations <- paste(trips_2019_q1$from_station_name, 'to', trips_2019_q1$to_station_name)
trips_2019_q2$trip_stations <- paste(trips_2019_q2$from_station_name, 'to', trips_2019_q2$to_station_name)
trips_2019_q3$trip_stations <- paste(trips_2019_q3$from_station_name, 'to', trips_2019_q3$to_station_name)
trips_2019_q4$trip_stations <- paste(trips_2019_q4$from_station_name, 'to', trips_2019_q4$to_station_name)


# group by trip stations and usertype to get number of subscriber or customer riders on each trip stations
trips_2019_q2_final <- trips_2019_q2 %>% 
  select(trip_stations,tripduration,usertype) %>% 
  group_by(trip_stations,usertype) %>% 
  summarise(count_usertype = length(usertype))
trips_2019_q2_final <- subset(trips_2019_q2_final, count_usertype > 1030)

trips_2019_q1_final <- trips_2019_q1 %>% 
  select(trip_stations,tripduration,usertype) %>% 
  group_by(trip_stations,usertype) %>% 
  summarise(count_usertype = length(usertype))
trips_2019_q1_final <- subset(trips_2019_q1_final, count_usertype > 400)

trips_2019_q3_final <- trips_2019_q3 %>% 
  select(trip_stations,tripduration,usertype) %>% 
  group_by(trip_stations,usertype) %>% 
  summarise(count_usertype = length(usertype))
trips_2019_q3_final <- subset(trips_2019_q3_final, count_usertype > 1800)

trips_2019_q4_final <- trips_2019_q4 %>% 
  select(trip_stations,tripduration,usertype) %>% 
  group_by(trip_stations,usertype) %>% 
  summarise(count_usertype = length(usertype))
trips_2019_q4_final <- subset(trips_2019_q4_final, count_usertype > 653)


#finally export the quarter data to csv to further analyze in google spreadsheet
write.csv(trips_2019_q2_final,"C:\\Users\\haree\\Desktop\\Bikeshare - Case_study\\trips_q2_2019.csv")
write.csv(trips_2019_q1_final,"C:\\Users\\haree\\Desktop\\Bikeshare - Case_study\\trips_q1_2019.csv")
write.csv(trips_2019_q3_final,"C:\\Users\\haree\\Desktop\\Bikeshare - Case_study\\trips_q3_2019.csv")
write.csv(trips_2019_q4_final,"C:\\Users\\haree\\Desktop\\Bikeshare - Case_study\\trips_q4_2019.csv")

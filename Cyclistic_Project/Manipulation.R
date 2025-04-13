library(skimr)
library(tibble)
library(dplyr)
library(tidyr)
library(Tmisc)
library(ggplot2)


# Set working directory to the folder containing your CSVs
setwd("~/Desktop/Others/Analytics Portfolio/Project 1 Cyclistic Bike-Share/Datasets/Modified")

# List all CSV files in the folder
csv_files <- list.files(pattern = "\\.csv$")

# Read all CSV files into a list
data_list <- lapply(csv_files, read.csv, sep = ",")

# Optionally, combine all into one data frame (if they have the same structure)
combined_data <- do.call(rbind, data_list)

#saving it to another name
df <- combined_data
head(df)

#Converted the values into YY-MM-DD HH-MM-SS format
df$started_at <- as.POSIXlt.character(df$started_at, format="%Y-%m-%d %H:%M:%S")
df$ended_at <- as.POSIXlt.character(df$ended_at, format="%Y-%m-%d %H:%M:%S")

#Calculated for the average ride length of consumers
df$ride_length <- df$ended_at-df$started_at

library(hms)
library(data.table)

#Converted ride length to HMS format
df$ride_length_hms <- as.hms(df$ride_length)

#Determine day of the week using started at variable
df$day_of_week <-  weekdays(df$started_at)
#Determine month using started at variable
df$month <- format(df$started_at, "%b")

#We only retained those with values for ride length
df2 <- df[complete.cases(df$ride_length), ]

#Checking variables
glimpse(df2)

#Modifying structure of variables
df2$ride_length <- as.numeric(df2$ride_length)
df2$day_of_week <- as.factor(df2$day_of_week)

#Just in case numeric form is better
df2$day_of_week_num <- ifelse(df2$day_of_week=="Sunday",1,
                              ifelse(df2$day_of_week=="Monday",2,
                                     ifelse(df2$day_of_week=="Tuesday",3,
                                            ifelse(df2$day_of_week=="Wednesday",4,
                                                   ifelse(df2$day_of_week=="Thursday",5,
                                                          ifelse(df2$day_of_week=="Friday",6,7))))))

#Removing NAs
df2 <- df2[complete.cases(df2$month), ]
df2 <- df2[complete.cases(df2$day_of_week), ]

#Remove columns not important for simple analyses, to save on data
df3 <- df2 %>%
  select(-started_at, -ended_at, -start_station_name,-start_station_id,-end_station_name,-end_station_id)


#Checking variables
glimpse(df3)
#Exporting the csv file for either R again or tableau
# save.image("~/Desktop/Others/Portfolio/Project 1 Cyclistic Bike-Share/Datasets/Manipulation/Manipulation.RData") For easing loading
write.csv(df3,"visualisationdf.csv")

library(skimr)
library(tibble)
library(dplyr)
library(tidyr)
library(Tmisc)
library(ggplot2)

load("~/Desktop/Others/Portfolio/Project 1 Cyclistic Bike-Share/Datasets/Manipulation/Manipulation.RData")

#Analysis

#Fixing variables
df3$month <- factor(df3$month,levels=c("Jan",
                                       "Feb",
                                       "Mar",
                                       "Apr",
                                       "May",
                                       "Jun",
                                       "Jul",
                                       "Aug",
                                       "Sep",
                                       "Oct",
                                       "Nov",
                                       "Dec"))

#Function to get mode
get_mode <- function(x) {
  uniq_x <- unique(x)
  uniq_x[which.max(tabulate(match(x, uniq_x)))]
}

#Summary statistics across months
summary <- df3 %>%
  group_by(member_casual, rideable_type, month) %>%
  summarize(
    count = n(),
    mean_ride_length = mean(ride_length, na.rm = TRUE),
    sd_ride_length = sd(ride_length, na.rm = TRUE),
    mode_day = get_mode(day_of_week)
  ) 

#Fixing mode_day variable
summary$mode_day <- factor(summary$mode_day,levels=c("Sunday",
                                             "Monday",
                                             "Tuesday",
                                             "Wednesday",
                                             "Thursday",
                                             "Friday",
                                             "Saturday"))
#Summary statistics across days
by_day <- df3 %>%
  group_by(day_of_week, member_casual, rideable_type) %>%
  summarize(
    count = n(),
    mean_ride_length = mean(ride_length, na.rm = TRUE),
    sd_ride_length = sd(ride_length, na.rm = TRUE),
  ) 

by_day$day_of_week <- factor(by_day$day_of_week,levels=c("Sunday",
                                                     "Monday",
                                                     "Tuesday",
                                                     "Wednesday",
                                                     "Thursday",
                                                     "Friday",
                                                     "Saturday"))
#Exporting summary statistics
write.csv(summary,"Summary.csv")



#Plot per month

pdf(paste("Plot1_Year.pdf"),height=6,width=6)
ggplot(summary, aes(x=member_casual,y=mean_ride_length/60, fill=rideable_type))+
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~month)+
  xlab("Membership type")+
  ylab("Ride length (mins)")+
  guides(fill=guide_legend(title="Rideable type"))+
  theme(strip.background = element_blank())+
  theme_bw()
dev.off()

pdf(paste("Plot2_Day.pdf"),height=6,width=12)
ggplot(by_day, aes(x=day_of_week,y=mean_ride_length/60, fill=rideable_type))+
  geom_bar(position="dodge", stat="identity") +
  facet_wrap(~member_casual)+
  xlab("Day")+
  ylab("Ride length (mins)")+
  guides(fill=guide_legend(title="Rideable type"))+
  theme(strip.background = element_blank())+
  theme_bw()
dev.off()


# for visualisation
write.csv(df3,"Visualisation_Cyclist_data2.csv")




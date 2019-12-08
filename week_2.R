## Note: This script requires setwd("./Projects/Reproducible_Data/Week 2")
library(dplyr)
## Create variables for fileanames
df <- read.csv("./activity.csv")

##  What is mean total number of steps taken per day?
# process data frame
df_steps <- df %>% group_by(date) %>% summarize(daily_steps = sum(na.omit(steps))) 
# Generate histogram displaying total steps per day
hist(df_steps$daily_steps, col="blue2", main="Histogram of Total Daily Steps", 
     xlab="Steps", breaks = 20)
# Generate Box plot to display mean and median of the total daily steps
M = mean(df_steps$daily_steps)
boxplot(df_steps$daily_steps, ylab="Daily Steps", xlab="")
points(M, col="red", pch= 8, cex=2)

##=============================================================

## What is the average daily activity pattern?
library(ggplot2)
theme_set(theme_minimal())
# process data frame
df_interval <- df %>% group_by(interval) %>% summarize(avg_steps = mean(na.omit(steps)))
# identify maximum data point
highlight_pt <- df_interval %>% filter(avg_steps == max(avg_steps))
# Generate time series plot
# Basic line plot
ggplot(data = df_interval, aes(x = interval, y = avg_steps))+
  geom_line(color = "#00AFBB", size = 1) +
  geom_point(data=highlight_pt, aes(x= interval,y= avg_steps), 
             color='orange',size=2) +
  annotate("text", x = 1250, y = 206, label = "Max Average Steps")

##=============================================================

## Imputing missing values
# Calculate and report the total number of missing values in the dataset
tot_NAs <- count(df,is.na(steps))$n[2]
# Devise a strategy for replacing missing values. In this code
# I chose to replace NAs with the daily step average for a given 5 min interval
# Replace the NAs in the steps variable
df_correct <- df %>% mutate(avg_step = rep(df_interval$avg_steps, length.out = length(df$interval)))
corr_step <- if_else(is.na(df_correct$steps), df_correct$avg_step, as.numeric(df_correct$steps))
df_correct <- mutate(df_correct, c_steps = corr_step)
df_correct <- df_correct %>% select(c_steps,date,interval)
# process data frame
df_c_steps <- df_correct %>% group_by(date) %>% summarize(daily_steps = sum(c_steps)) 
# Generate histogram displaying total steps per day
hist(df_c_steps$daily_steps, col="indianred1", main="Histogram of Total Daily Steps (corrected)", 
     xlab="Steps", breaks = 20)
# Generate Box plot to display mean and median of the total daily steps
M = mean(df_c_steps$daily_steps)
boxplot(df_steps$daily_steps, ylab="Daily Steps", xlab="")
points(M, col="indianred1", pch= 8, cex=2)

##=============================================================
## Are there differences in activity patterns between weekdays and weekends?
# process data frame
df_correct <- df_correct %>% mutate(day_ = weekdays(as.Date(date))) %>% 
  mutate(day_type = if_else(((day_ == ("Saturday"))|(day_ == ("Sunday"))), "Weekend", "Weekday"))
df_c_interval <- df_correct %>% group_by(interval,day_type) %>% summarize(avg_steps = mean(c_steps))
# Multiple line plot
ggplot(df_c_interval, aes(x = interval, y = avg_steps)) +
  geom_line(aes(color = day_type), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800"))
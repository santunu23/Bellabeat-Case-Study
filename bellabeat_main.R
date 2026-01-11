install.packages("tidyverse")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(lubridate)
library(janitor)
library(ggplot2)

daily_activity <- read_csv("D:/Course/Data Analysis/Case Study/Bellabeat/db/FitabaseData4.12.16-5.12.16/dailyActivity_merged.csv")
sleep_day <- read_csv("D:/Course/Data Analysis/Case Study/Bellabeat/db/FitabaseData4.12.16-5.12.16/sleepDay_merged.csv")
hourly_steps <- read_csv("D:/Course/Data Analysis/Case Study/Bellabeat/db/FitabaseData4.12.16-5.12.16/hourlySteps_merged.csv")
View(daily_activity)
str(daily_activity)
str(sleep_day)
str(hourly_steps)
# Convert daily_activity date and time using mdy() function
daily_activity <- daily_activity %>%  
    mutate(actvity_date=mdy(ActivityDate))
str(daily_activity)
# Converted SleepDay,hourly_steps to a date-time format using as.POSIXct because the 
# original column included both date and time data.
sleep_day <- sleep_day %>%
  mutate(sleep_day = as.POSIXct(SleepDay, format="%m/%d/%Y %H:%M", tz="UTC"))

hourly_steps <- hourly_steps %>% 
                mutate(activity_hour= as.POSIXct(ActivityHour, format="%m/%d/%Y %H:%M", tz="UTC"))
str(hourly_steps)

#  Remove Duplicate
daily_activity <- daily_activity %>% distinct()
View(daily_activity)
hourly_steps <- hourly_steps %>% distinct()

View(hourly_steps)

#Remove duplicate from the tables
sleep_day <- sleep_day %>% distinct()
daily_activity <- daily_activity %>% distinct()
hourly_steps <- hourly_steps %>% distinct()

#Check and sure the row size using nrow() function
nrow(daily_activity)
nrow(sleep_day)
nrow(hourly_steps)
#Used the View() function to inspect all dataframes and verify the number of entries. 
#Confirmed that daily_activity has 940 records. Validated unique user counts across 
#datasets to identify participation levels (e.g., only 24 users provided sleep data).

n_distinct(daily_activity$Id)
n_distinct(sleep_day$Id)
n_distinct(hourly_steps$Id)

View(daily_activity)
View(sleep_day)
View(hourly_steps)

# To marge both tables(daily_activity,sleep_day) we need a common column
daily_activity <- daily_activity %>% 
  rename(date=actvity_date)

sleep_day <- sleep_day %>% 
    rename(date=sleep_day) 

#Marge both tables 
combined_date <- merge(daily_activity,sleep_day,by=c("Id","date"))

#Display only important column
daily_activity %>% 
  select(-ActivityDate) %>% 
  View()


# 1. Relation between Lightly Active Minutes and Calories
cor(daily_activity$LightlyActiveMinutes, daily_activity$Calories)

# 2. Relation between Very Active Minutes and Calories
cor(daily_activity$VeryActiveMinutes, daily_activity$Calories)

combined_data_subset <- combined_date %>% 
  select(Id, date, LightlyActiveMinutes,VeryActiveMinutes, TotalSteps, Calories, TotalMinutesAsleep, TotalTimeInBed)

View(combined_data_subset)

colnames(combined_date)


# Average per user
user_patterns <- combined_date %>%
  group_by(Id) %>%
  summarize(
    avg_steps = mean(TotalSteps),
    avg_calories = mean(Calories),
    avg_sleep = mean(TotalMinutesAsleep)
  )

# ফলাফলটি দেখা
View(user_patterns)

# স্টেপস এবং ঘুমের মধ্যে সম্পর্কের গাণিতিক মান দেখা 

#Calculated the correlation coefficient between TotalSteps and TotalMinutesAsleep, which resulted in -0.19. This confirms a weak negative correlation, proving that higher physical activity does not lead to increased sleep duration for this user group.
cor(combined_date$TotalSteps, combined_date$TotalMinutesAsleep)

low_sleepers <- user_patterns %>%
  filter(avg_sleep < 200) # ২০০ মিনিটের কম ঘুম
View(low_sleepers)

# ২০০ মিনিটের কম গড় ঘুম আছে এমন ইউজারদের বাদ দেওয়া
#Identified three users with average sleep durations under 200 minutes. These are likely outliers due to inconsistent device usage at night. Filtered out these outliers to ensure the integrity and accuracy of the final trend analysis.
user_patterns_clean <- user_patterns %>%
  filter(avg_sleep >= 200)

# এখন আবার কো-রিলেশন চেক করে দেখুন
cor(user_patterns_clean$avg_steps, user_patterns_clean$avg_sleep)

ggplot(data = user_patterns_clean, aes(x = avg_steps, y = avg_sleep)) +
  geom_point(color = "darkblue", size = 3) +
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  annotate("text", x = 15000, y = 500, label = "Correlation: -0.42", color = "red", size = 5) +
  labs(title = "Average Steps vs Average Sleep (Cleaned Data)",
       subtitle = "Showing a moderate negative correlation among users",
       x = "Average Daily Steps",
       y = "Average Daily Sleep (Minutes)") +
  theme_minimal()

weekday_summary <- combined_date %>%
  group_by(day_of_week) %>%
  summarize(
    avg_steps = mean(TotalSteps),
    avg_sleep = mean(TotalMinutesAsleep)
  )

ggplot(weekday_summary, aes(x = day_of_week, y = avg_steps, fill = avg_steps)) +
  geom_bar(stat = "identity") +
  coord_polar() + # এটি চার্টটিকে বৃত্তাকার (Design format) করে দেবে
  scale_fill_gradient(low = "linen", high = "tomato") + # বেল্লাবিটের ব্র্যান্ড কালারের মতো
  labs(title = "Weekly Activity Design: The Rhythm of Movement",
       subtitle = "A circular view of how users move through the week") +
  theme_void() # চারদিকের গ্রিড সরিয়ে ক্লিন ডিজাইন দেবে


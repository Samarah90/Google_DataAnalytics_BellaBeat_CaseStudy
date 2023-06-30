# CASE STUDY: Bellabeat Fitness Data Analysis 
##### Author: Samarah Rizvi

##### Date: June 5, 2023

This case study follows the follwing six data analysis steps:
### 1- Ask
### 2- Prepare
### 3- Process
### 4- Analyze
### 5- Share
### 6- Act

# Background of bellabeat
Bellabeat is a high-tech company that manufactures health-focused smart products. Since it was founded in 2013, Bellabeat has grown rapidly and quickly positioned itself as a tech-driven wellness company for women. The company has 5 main products: bellabeat app, leaf(basic wellness tracker), time(wellness watch), spring(smart water bottle) and bellabeat membership. Bellabeat is a successful small company, but they have the potential to become a larger player in the global smart device market. 
## 1- Ask phase
### Business Task : We are asked to analyze Fitbit smart device data to gain insight into how consumers are using their smart devices. The insights can help guide the marketing strategy for the company. 
Main stakeholders: Urška Sršen(cofounder and Chief Creative Officer), Sando Mur (Mathematician and Bellabeat’s cofounder) and Bellabeat marketing analytics team.
## 2- Prepare phase 
### Dataset source
The data source used for our case study is the FitBit Fitness Tracker Data. This is a 30 participants FitBit Fitness Tracker Data avaiable from Mobius: https://www.kaggle.com/arashnic/fitbit. It is an open source data. the dataset has 18 csv files.
### Does the data follow the ROCCC approach? 
- Reliability and originality:
The data is from 30 FitBit users who consented to the submission of personal tracker data and generated by from a distributed survey via Amazon Mechanical Turk.
- Comprehensive:  Thirty eligible Fitbit users consented to the submission of personal tracker data, including minute-level output for physical activity, heart rate, and sleep monitoring.
- Current: These datasets were generated by respondents to a distributed survey via Amazon Mechanical Turk between 03.12.2016-05.12.2016.
- Cited : unknown
### Data Credibility and Integrity
The sample size of the data is very small and the absence of the demographic information of the participents can make the data prone to the sampling bias which means we are unsure if the sample size is representative of the whole population as a whole. Another problem is that the dataset is not current (recorded in 2016). 
## 3- Process phase
I have focused on the following datasets for my analysis:
- dailyActivity_merged
- hourlySteps_merged
- sleepDay_merged
- weightLogInfo_merged
### installing and loading all the required libraries
```
install.packages("readr")
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggpubr")
install.packages("ggrepel")
```
```
library(readr)
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggplot2)
library(ggrepel)
```
### 3.1- The dailyActivity_merged dataset
#### summary
```
skim_without_charts(dailyActivity_merged)
glimpse(dailyActivity_merged)
```
#### Change date format
```
dailyActivity_merged$ActivityDate <- mdy(dailyActivity_merged$ActivityDate)
```
#### Count the missing, duplicated and distinct Ids
```
dim(dailyActivity_merged)
sum(is.na(dailyActivity_merged)) 
sum(duplicated(dailyActivity_merged)) 
n_distinct(dailyActivity_merged$Id)
```
No missing or duplicated values. 33 distinct Ids instead of the reported 30 distinct Ids.
#### Making a new dataset of daily_activity with new columns of Total active hours, Total inactive hours and Weekdays
```
daily_activity <- dailyActivity_merged %>% mutate(Total_active_hours = c(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)/60) %>%
    mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y"))) %>% mutate(Total_sedentary_hours = SedentaryMinutes/60)
daily_activity <- daily_activity %>% mutate(Total_active_hours = round(Total_active_hours, digits = 0)) #round the hours#
daily_activity <- daily_activity %>% mutate(Total_sedentary_hours = round(Total_sedentary_hours, digits = 0))
daily_activity <- daily_activity[,c (1,2,17,3,4,16,18,15,5,6,7,8,9,10,11,12,13,14)] 
colnames(daily_activity)
```
### 3.2- The hourleySteps_merged dataset
#### summary
```
glimpse(hourlySteps_merged)
sum(duplicated(hourlySteps_merged)) #0 duplicated#
n_distinct(hourlySteps_merged$Id) #33 ids#
```
No missing or duplicated values. 33 distinct Ids instead of the reported 30 distinct Ids.
#### Making a new dataset of HourleySteps with seperate date time column using the lubridate package's parse_date_time()
```
Hourlysteps <- hourlySteps_merged %>% #sep date time column#
  mutate(ActivityHour = parse_date_time(ActivityHour, "m/d/y I:M:S p")) %>% 
  mutate(ActivityDate = as.Date(ActivityHour), ActivityHour = format(ActivityHour, format = "%H:%M:%S"))
```
### 3.3- The SleepDay_merged datset
#### summary
```
skim_without_charts(sleepDay_merged)
glimpse(sleepDay_merged)
```
#### Count the missing, duplicated and distinct Ids
```
dim(sleepDay_merged)
sum(is.na(sleepDay_merged)) #no missing value
sum(duplicated(sleepDay_merged)) #3 duplicates#
sleep_day <- sleepDay_merged[!duplicated(sleepDay_merged), ]
sum(duplicated(sleep_day)) #no duplicated values#
n_distinct(sleepDay_merged$Id) #24 distant ids#
```
No missing values but three duplicated values. So made a new dataset of Sleep_day with the removed duplicated values. 24 distinct Ids instead of 33 as in the previous datasets.
#### seperate date time column using the lubridate package's parse_date_time(), new column of weekdays from the date and new column for sleep in hours
```
sleep_day <- sleepDay_merged %>% mutate(SleepDay = parse_date_time(SleepDay, "m/d/y I:M:S p")) %>%  
  mutate(SleepDate = as.Date(SleepDay), SleepTime = format(SleepDay, format = "%H:%M:%S")) %>% 
  mutate( SleepWeekday = weekdays(as.Date(SleepDate, "%m/%d/%Y"))) %>% 
  mutate(TotalHoursAsleep = TotalMinutesAsleep/60) %>% 
  mutate(TotalHoursInBed = TotalTimeInBed/60) 
sleep_day <- sleep_day %>% select(-SleepDay)
```
### 3.4- The weightLogInfo_merged datset
#### summary
```
skim_without_charts(weightLogInfo_merged)
glimpse(weightLogInfo_merged)
```
#### Count the missing, duplicated and distinct Ids
```
sum(is.na(weightLogInfo_merged)) 
sum(duplicated(weightLogInfo_merged)) 
n_distinct(weightLogInfo_merged$Id) 
```
There are 65 missing values from the column Fat and only 8 distinct Ids with no duplicates. I have dropped this dataset from my further analysis as the sample size is very small.

## 4- Analyze and Share phase
At first we wanted to analyze the trends in daily activity of fitbit users that can possibly help Bellabeat to device good marketing strategies.
### 4.1- Distribution of activity in minutes
The distribution of active minutes on the daily_activity dataset is presented in form of a pie chart.
```
active_sed <- data.frame(Type = c("Very Active Minutes", "Fairly Active Minutes","Lightly Active Minutes", "Sedentary Minutes"), 
                         sums = c(sum(daily_activity$VeryActiveMinutes), sum(daily_activity$FairlyActiveMinutes),
                         sum(daily_activity$LightlyActiveMinutes), sum(daily_activity$SedentaryMinutes))) 
sum_all <- sum(active_sed$sums, na.rm=TRUE)
active_sed <- active_sed %>%  mutate(Percent = (sums /sum_all) * 100)
active_sed <- active_sed %>% mutate(Percent = round(Percent, digits = 2))
```
First we calculate the sum of different categories of activity minutes and then calculate the percentage by dividing each type of activity min with the sum of all minutes. 

```
df2 <- active_sed %>% 
  mutate(csum = rev(cumsum(rev(Percent))), 
         pos = Percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percent/2, pos))
```
Making a dataframe to get the positions of the labels that will be used by the "geom_label_repel" from the ggrepel package to adds text directly to the plot and repel the text if they overlap.

```
ggplot(active_sed, aes(x = "" , y = Percent, fill = fct_inorder(Type)))+ 
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2, aes(y = pos, label = paste0(Percent, "%")), 
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Type")) +
  ggtitle("Percentage of activity in minutes") +
  theme_void()
```
![2_Percentage of activity in minutesRplot](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/ed3b6dd8-015f-4d1b-afa2-1c5d57f26bcc)
As it is clearly seen that most of the time is spend being inactive only around 2% of the total time according to our fitbit data is spend being very active. We dived more deeper into the daily activity on the weekly basis.
### 4.2- Weekly activity in hours
Making a dataframe containing the mean of total steps, mean of calories burned, mean of active and sedentary hours spent each day in a week.
```
Weekly_activity_data <- daily_activity %>% 
  group_by(Weekday) %>% 
  summarise(MeanSteps = mean(TotalSteps), MeanCalories = mean(Calories), 
            MeanActiveHours = mean(Total_active_hours), 
            MeanSedentaryHours = mean(Total_sedentary_hours), TotalDistance = mean(TotalDistance), steps = sum(TotalSteps))
```
```
ggarrange(
  ggplot(data = Weekly_activity_data, aes(x = Weekday, y = MeanActiveHours)) +
    geom_bar(stat = "identity", fill = 'darkgreen') +
    geom_hline(yintercept = 4) +
    labs(title = "Weekly Active data", x = "Days of week", y = "Mean Active Hours"),
  ggplot(data = Weekly_activity_data, aes(x = Weekday, y = MeanSedentaryHours)) +
    geom_bar(stat = "identity", fill = 'lightgreen') +
    geom_hline(yintercept = 16) +
    labs(title = "Weekly inactive data", x = "Days of week", y = "Mean Sedentary Hours"),
  ggplot(data = Weekly_activity_data, aes(x = Weekday, y = MeanSteps)) +
    geom_histogram(stat = "identity", fill = 'darkblue') +
    geom_hline(yintercept = 8000) +
    labs(title = "Weekly steps data", x = "Days of week", y = "Mean Steps")
  )
```
![3_Weekly_summery](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/ce51d1ec-5071-4609-8b3c-d6707eadeb20)
Users seems to be most active on Saturday spending around 4 hours being active and taking more than 8,000 steps. On the other hand Sunday seems to be the resting day of the week with less activity. However users are spending around 16hours being sedentary througout the week. Tuesday also seems to be to be a hectic day in terms of total steps taken. Next we wanted to see our activity data on the bases to user Ids.
### 4.3- Identifying the user's class based on the total steps
The total target of 10,000 steps is considered to be a healthy target for the healthy adults. According to the guidelines provided here (https://www.10000steps.org.au/articles/healthy-lifestyles/counting-steps/#:~:text=The%20following%20pedometer%20indices%20have,to%209%2C999%20steps%20per%20day) the users are divied into various categories. This serves the bases of our following analysis.
```
daily_activity_ids<- daily_activity %>% 
  group_by(Id) %>% 
  summarise(MeanSteps = mean(TotalSteps), MeanCalories = mean(Calories), 
            MeanActiveHours = mean(Total_active_hours), 
            MeanSedentaryHours = mean(Total_sedentary_hours), MeanTotalDistance = mean(TotalDistance), TotalSteps = sum(TotalSteps), 
            TotalCalories = sum(Calories),
            TotalDistance = sum(TotalDistance))
daily_activity_ids <- daily_activity_ids %>% mutate(MeanActiveHours = round(MeanActiveHours, digits = 0)) 
daily_activity_ids <- daily_activity_ids %>% mutate(MeanSedentaryHours = round(MeanSedentaryHours, digits = 0))
```
Making a seperate dataframe which contain the mean of total steps taken, calories burned, active and inactive hours and respective sums of total steps, calories and distance. 
```
daily_activity_ids <- daily_activity_ids %>% 
  mutate(UsersActivityClass = case_when(
    MeanSteps < 5000 ~ 'Sedentary', 
    MeanSteps >= 5000 & MeanSteps < 7499 ~ 'Low Active', 
    MeanSteps >= 7500 & MeanSteps < 9999 ~ 'Fairly Active', 
    MeanSteps >= 10000 ~ 'Active', 
    MeanSteps >= 12500 ~ 'HighlyActive'))
```
Now introducing a new column in the dataframe where users will be allorted a certain activity class based on the mean steps they have taken.
```
user_class <- daily_activity_ids %>% 
  group_by(UsersActivityClass) %>% 
  summarise(ActiveHours = mean(MeanActiveHours), MeanCal = mean(MeanCalories)) 
user_class <- user_class %>% mutate(ActiveHours = round(ActiveHours, digits = 0)) %>% mutate(MeanCal = round(MeanCal, digits = 0))
```
Now grouping the user's activity classes to see how many hours a certain user class is active and how much calories they burn respectively.

```
ggarrange(
  ggplot(data = daily_activity_ids, aes(x = UsersActivityClass)) +
    geom_bar(fill='blue', color='black') +
    labs(title = "Users Activity", x = "classes of users"),
  ggplot(data = user_class, aes(x = UsersActivityClass , y = ActiveHours)) +
    geom_bar(stat = "identity", fill ='steelblue', color= 'black') +
    labs(title = "users vs active hours", x = "Users Class", y = "Mean Active Hours"),
ggplot(data = mean_cal, aes(x = UsersActivityClass, y = meancal)) +
  geom_bar(stat = "identity", fill = 'lightblue', color = 'black') +
  labs(title = "Calories based on User Class", x = "User Activity Class", y = "Mean Calories"))
```
![5_ users_class](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/d5451fca-7189-4928-97a7-0c3978d126d0)
In the group of 33 fitbit users, there are more people who are fairly or low active which mean they take around 5,000 - 9,999 per day and are active for 4-5 hours. These users clearly also burns more calories as compared to the low active and sedentary ones.
### 4.4- Hourly steps
Now that we have got the idea baout the active days of the week and the distribution of active and inactive users in our fitbit datasets, we want to know exactly what part of the whole day can be expected to be most active or inactive. 
```
Hourlysteps_viz <- Hourlysteps %>% 
  group_by(ActivityHour) %>% 
  summarise(AvgSteps = mean(StepTotal))
ggplot() +
  geom_col (data = Hourlysteps_viz, aes(x = ActivityHour, y = AvgSteps, fill = AvgSteps)) +
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "yellow", high = "purple")+
  theme(axis.text.x = element_text(angle = 90))
```
![4_hourly_summary](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/db75796c-fae0-4868-8f56-f67de9a79871)
As it can be seen people are more active during the afternoon(from around 12pm to 2:00pm) and in the evening (from 4:30pm to 7:00pm). 

### 4.5- Merging the sleep_day and daily_activity datasets for Corelation analysis
Merging the two datasets by same ids and dates for corelation studies.
```
sleep_day <- sleep_day %>% rename(Date = SleepDate) 
daily_activity <- daily_activity %>% rename(Date = ActivityDate)
daily_activity_sleep <- merge(daily_activity,sleep_day, by= c("Id", "Date"))
daily_activity_sleep <- daily_activity_sleep %>% mutate(TotalHoursAsleep = round(TotalHoursAsleep, digits = 0)) %>% 
  mutate(TotalHoursInBed = round(TotalHoursInBed, digits = 0))
daily_activity_sleep$SleepWeekday <- factor(daily_activity_sleep$SleepWeekday, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))
```
First checking the sleep datasets on the basis of weekdays.
```
weekly_sleep <- daily_activity_sleep %>% 
  group_by(SleepWeekday) %>% 
  summarise(Mean_sleephr = mean(TotalHoursAsleep), Mean_inBed = mean(TotalHoursInBed)) # cal mean sleep hours
ggplot(data= weekly_sleep, aes(x= SleepWeekday, y = Mean_sleephr)) +
  geom_bar(stat = "identity", fill = 'grey', color= 'black') +
  geom_hline(yintercept = 8) +
  labs(title = "Hours asleep per day in a week", x = "Days of Week", y = "Hours")
```
![6_sleep hrs per day](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/0bd81e71-902b-4866-b0c8-941c6105f4a4)
The data shows that users are not sleeping the recommended more than 7 hours of sleep everyday. As we dont have the demographic data of the users, we cant say anything about their age which is very important in determining the recommended hours of sleep (https://www.cdc.gov/sleep/about_sleep/how_much_sleep.html). 

Now lets perform some corelation analysis to see if one variable is affecting the other.
#### 4.5.1- Calories vs sleep and daily steps
```
ggarrange( 
  ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_jitter()+
  geom_smooth(color = "red") +
  labs(title = "Daily Steps vs Calories", x = "Total Steps", y = "Calories"),
  ggplot(data = daily_activity_sleep, aes(x = TotalHoursAsleep, y = Calories)) +
    geom_jitter() +
    geom_smooth(color = "red") +
    labs(title = "Sleep vs Calories", x = "Hours Asleep", y = "Calories"))
```
![7_calories vs dailysteps and sleep](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/86687c81-2ef3-40a8-81ad-d62b1a46eda8)
As it is expected there is a positive corelation between total steps taken and total calories burned however there is no corelation between hours spent asleep and calories burned.
#### 4.5.2- Sleep vs activity
Now lets see if there is any corelation between the sleep and the active/inactive state of the users.
```
p2 <- ggplot(data = daily_activity_sleep, aes(x = TotalSteps, y = TotalHoursAsleep)) +
  geom_jitter() +
  geom_smooth(color = "red") +
  labs(title = "Daily Steps vs Sleep", x = "Daily Steps", y = "Hours") +
  theme_classic()

p1 <- ggscatter(daily_activity_sleep, x = "TotalMinutesAsleep", y = "SedentaryMinutes", 
          add = "reg.line",  
          add.params = list(color = "red", fill = "lightgray"), 
          conf.int = TRUE,  
          title = "Sleep vs inactivity",
          xlab = "Total Minutes Asleep", ylab = "Sendentary Minutes") 
p1 + stat_cor(method = "pearson") 
p1 + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)                                                    
ggarrange(p2,p1+ stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)) 
```
![8_daily steps, inactivity vs sleep](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/35421dfc-3bca-4e56-afca-54deab6f2a1a)
It is clear that there is no relationship between daily steps taken and calories. However there is a significantl negative corelation between total minutes asleep and sedentary minutes with a negative r value of -0.6 and P < 0.001 . Which means that according to this analysis of around 30 fitbit users, people who tends to sleep less are more inactive or vice versa. There are studies that confirm the lack of excercise or sedentary lifestyle causing sleep issues(https://www.sleepfoundation.org/insomnia/exercise-and-insomnia#references-80044). 
### 4.5- Device usage
It is very important for the stakeholders to understand the trend of the smart device usuage among the users so that they can device their marketing strategies accordingly. 
```
daily_use <- daily_activity_sleep %>% 
  group_by(Id) %>% 
  summarise(days_use = sum(n())) %>% 
  mutate(usage = case_when(
    days_use >= 1 & days_use <= 10 ~ "Low use",
    days_use >= 11 & days_use <= 20 ~ "Moderate use",
    days_use >= 21 & days_use <= 35 ~ "High use"
  ))
head(daily_use)
```
Here we have made a dataset where the users are grouped according to their device usage. 
```
percent_use <- daily_use %>% 
group_by(usage) %>% 
summarise(total = n()) %>% 
mutate(totals = sum(total)) %>% 
group_by(usage) %>% 
summarise(total_percent = total/totals) %>% 
mutate(labels = scales::percent(total_percent))
```
Then we have made a new dataframe calculating the percentage of the device usage.
```
week_usage_1 <- ggplot(data = percent_use, aes(x = "", y = total_percent, fill=usage)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+ #A minimalistic theme with no background annotations#
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5))+
  scale_fill_manual(values = c("#990053","#ff33a1","#ff99d0"),
                    labels = c("High use - 21 to 35 days",
                               "Moderate use - 11 to 20 days",
                               "Low use - 1 to 10 days"))+
  labs(title="Daily use of smart device")
```
Other than the daily use of the smart device, it would be nice to see the daily input of the data during the week by the users.
```
week_usage <- ggplot(data = daily_activity, aes(x=Weekday)) +
  geom_bar(fill='#ff99d0', color='black') +
  labs(title = "Data Input during the Week",x = "Days of the week", y = "Frequency")
ggarrange(week_usage, week_usage_1)
```
![9_data input in week and days](https://github.com/Samarah90/Google_DataAnalytics_BellaBeat_CaseStudy/assets/120459742/d3dc8d54-7790-49e7-b9f7-12a276ec1ff9)
According to theses results, the users have used the device most frequently during the week from Tue-Thurs. Only half of the users are frequently using the device.

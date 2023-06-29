#loading the readr package to import the csv files#
library(readr)
#load the cleaning libraries#
install.packages("here")
install.packages("skimr")
install.packages("janitor")
install.packages("dplyr")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("ggpubr")
library(here)
library(skimr)
library(janitor)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggpubr)
library(ggplot2)


#summary of the daily_activity columns#
skim_without_charts(dailyActivity_merged)
glimpse(dailyActivity_merged)
dailyActivity_merged$ActivityDate <- mdy(dailyActivity_merged$ActivityDate) #chamge date format#
#count distant ids#
dim(dailyActivity_merged)
sum(is.na(dailyActivity_merged)) #no missing values#
sum(duplicated(dailyActivity_merged)) #no duplicates#
n_distinct(dailyActivity_merged$Id) #33 distant values, instead of expexted 30#
#incorporating a new column of week days using the ActivityDate column#
#incorporating a new column for total active hours#
#incorporating a new column to nonactive hours#
daily_activity <- dailyActivity_merged %>% mutate(Total_active_hours = c(VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes)/60) %>%
    mutate( Weekday = weekdays(as.Date(ActivityDate, "%m/%d/%Y"))) %>% mutate(Total_sedentary_hours = SedentaryMinutes/60) %>% 
daily_activity <- daily_activity %>% mutate(Total_active_hours = round(Total_active_hours, digits = 0)) #round the hours#
daily_activity <- daily_activity %>% mutate(Total_sedentary_hours = round(Total_sedentary_hours, digits = 0))
daily_activity <- daily_activity[,c (1,2,17,3,4,16,18,15,5,6,7,8,9,10,11,12,13,14)] #arrange#
 colnames(daily_activity)
 
#summary of the hourly merged data#
glimpse(hourlySteps_merged)
sum(duplicated(hourlySteps_merged)) #0 duplicated#
n_distinct(hourlySteps_merged$Id) #33 ids#
Hourlysteps <- hourlySteps_merged %>% #sep date time column#
  mutate(ActivityHour = parse_date_time(ActivityHour, "m/d/y I:M:S p")) %>% 
  mutate(ActivityDate = as.Date(ActivityHour), ActivityHour = format(ActivityHour, format = "%H:%M:%S"))

#summary of the sleep data#
skim_without_charts(sleepDay_merged)
glimpse(sleepDay_merged)
#count distant ids for sleep data#
dim(sleepDay_merged)
sum(is.na(sleepDay_merged)) #no missing value
sum(duplicated(sleepDay_merged)) #3 duplicates#
sleep_day <- sleepDay_merged[!duplicated(sleepDay_merged), ]
sum(duplicated(sleep_day)) #no duplicated values#
n_distinct(sleepDay_merged$Id) #24 distant ids#
sleep_day <- sleepDay_merged %>% mutate(SleepDay = parse_date_time(SleepDay, "m/d/y I:M:S p")) %>%  #lubridate package's parse_date_time() to convert SleepTime from character strings to date/time values. Then we can break the date and time apart with as.Date() and format#
  mutate(SleepDate = as.Date(SleepDay), SleepTime = format(SleepDay, format = "%H:%M:%S")) %>% 
  mutate( SleepWeekday = weekdays(as.Date(SleepDate, "%m/%d/%Y"))) %>% #weekdays from date#
  mutate(TotalHoursAsleep = TotalMinutesAsleep/60) %>% 
  mutate(TotalHoursInBed = TotalTimeInBed/60) 
sleep_day <- sleep_day %>% select(-SleepDay)

#summary of the weight data#
skim_without_charts(weightLogInfo_merged)
glimpse(weightLogInfo_merged)
sum(is.na(weightLogInfo_merged)) #65 missing values from the column fat so remove the column#
weight <- weightLogInfo_merged %>% select(-Fat) #too many NAs#
colnames(weight)
sum(duplicated(weightLogInfo_merged)) #no duplication#
n_distinct(weightLogInfo_merged$Id) #only 8 distinct ids#
weight <- weightLogInfo_merged %>% mutate(Date = parse_date_time(Date, "m/d/y I:M:S p")) %>%  #lubridate package's parse_date_time() to convert Time from character strings to date/time values. Then we can break the date and time apart with as.Date() and format#
  mutate(WeightDate = as.Date(Date), WeightTime = format(Date, format = "%H:%M:%S")) %>% 
  mutate( WeightWeekDay = weekdays(as.Date(WeightDate, "%m/%d/%Y"))) %>% 
  select(-Date) %>% select(-LogId) %>% select(-Fat) #removing fat column cux of so many nas and logid cux i dont need it and the remaing date column as i splitted it#

#summary of the daily_activity#
colnames(daily_activity) #x axis in r range from 0 to 1
daily_activity %>% 
  dplyr::select(Weekday, TotalSteps, TotalDistance, Total_active_hours, Total_sedentary_hours, Calories) %>% 
  summary()

#daily_activity input#
daily_activity$Weekday <- factor(daily_activity$Weekday, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday")) #arrange weekdays
week_usage <- ggplot(data = daily_activity, aes(x=Weekday)) +
  geom_bar(fill='#ff99d0', color='black') +
  labs(title = "Data Input during the Week",x = "Days of the week", y = "Frequency") #midweek more data input#

#compute percentages of activity in min#
active_sed <- data.frame(Type = c("Very Active Minutes", "Fairly Active Minutes","Lightly Active Minutes", "Sedentary Minutes"), 
                         sums = c(sum(daily_activity$VeryActiveMinutes), sum(daily_activity$FairlyActiveMinutes),
                         sum(daily_activity$LightlyActiveMinutes), sum(daily_activity$SedentaryMinutes))) 
sum_all <- sum(active_sed$sums, na.rm=TRUE)
active_sed <- active_sed %>%  mutate(Percent = (sums /sum_all) * 100)
active_sed <- active_sed %>% mutate(Percent = round(Percent, digits = 2)) #round to 2 decimal places#
# Get the positions
install.packages(ggrepel)
library(ggrepel)
df2 <- active_sed %>% 
  mutate(csum = rev(cumsum(rev(Percent))), 
         pos = Percent/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percent/2, pos))

ggplot(active_sed, aes(x = "" , y = Percent, fill = fct_inorder(Type)))+ #fct_inorder(): by the order in which they first appear#
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2, aes(y = pos, label = paste0(Percent, "%")), #geom_text_repel adds text directly to the plot and repel the text if they are close by#
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Type")) +
  ggtitle("Percentage of activity in minutes") +
  theme_void()
 
#Weekly activity# 
#table for mean activity in a week#
Weekly_activity_data <- daily_activity %>% 
  group_by(Weekday) %>% 
  summarise(MeanSteps = mean(TotalSteps), MeanCalories = mean(Calories), 
            MeanActiveHours = mean(Total_active_hours), 
            MeanSedentaryHours = mean(Total_sedentary_hours), TotalDistance = mean(TotalDistance), steps = sum(TotalSteps)) 

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

#table for mean activity of ids# #identifying the class of users based on the total steps#
daily_activity_ids<- daily_activity %>% 
  group_by(Id) %>% 
  summarise(MeanSteps = mean(TotalSteps), MeanCalories = mean(Calories), 
            MeanActiveHours = mean(Total_active_hours), 
            MeanSedentaryHours = mean(Total_sedentary_hours), MeanTotalDistance = mean(TotalDistance), TotalSteps = sum(TotalSteps), 
            TotalCalories = sum(Calories),
            TotalDistance = sum(TotalDistance))
daily_activity_ids <- daily_activity_ids %>% mutate(MeanActiveHours = round(MeanActiveHours, digits = 0)) 
daily_activity_ids <- daily_activity_ids %>% mutate(MeanSedentaryHours = round(MeanSedentaryHours, digits = 0))
 
#seperating and visulizing the users on the bases of steps# 
daily_activity_ids <- daily_activity_ids %>% 
  mutate(UsersActivityClass = case_when(
    MeanSteps < 5000 ~ 'Sedentary', 
    MeanSteps >= 5000 & MeanSteps < 7499 ~ 'Low Active', 
    MeanSteps >= 7500 & MeanSteps < 9999 ~ 'Fairly Active', 
    MeanSteps >= 10000 ~ 'Active', 
    MeanSteps >= 12500 ~ 'HighlyActive'))

#activity vs hours#
user_class <- daily_activity_ids %>% 
  group_by(UsersActivityClass) %>% 
  summarise(ActiveHours = mean(MeanActiveHours), MeanCal = mean(MeanCalories)) 
user_class <- user_class %>% mutate(ActiveHours = round(ActiveHours, digits = 0)) %>% mutate(MeanCal = round(MeanCal, digits = 0))

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

#Hourly data viz#
Hourlysteps_viz <- Hourlysteps %>% 
  group_by(ActivityHour) %>% 
  summarise(AvgSteps = mean(StepTotal))
ggplot() +
  geom_col (data = Hourlysteps_viz, aes(x = ActivityHour, y = AvgSteps, fill = AvgSteps)) +
  labs(title = "Hourly steps throughout the day", x="", y="") + 
  scale_fill_gradient(low = "yellow", high = "purple")+
  theme(axis.text.x = element_text(angle = 90))

#Merging datasets#
sleep_day <- sleep_day %>% rename(Date = SleepDate) 
daily_activity <- daily_activity %>% rename(Date = ActivityDate)
weight <- weight %>% rename(Date = WeightDate)
daily_activity_sleep <- merge(daily_activity,sleep_day, by= c("Id", "Date"))
daily_activity_weight <- merge(daily_activity, weight, sleep_day, by= c("Id", "Date"))
daily_activity_sleep <- daily_activity_sleep %>% mutate(TotalHoursAsleep = round(TotalHoursAsleep, digits = 0)) %>% 
  mutate(TotalHoursInBed = round(TotalHoursInBed, digits = 0))
daily_activity_sleep$SleepWeekday <- factor(daily_activity_sleep$SleepWeekday, c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday", "Sunday"))

#sleep hours per day#
weekly_sleep <- daily_activity_sleep %>% 
  group_by(SleepWeekday) %>% 
  summarise(Mean_sleephr = mean(TotalHoursAsleep), Mean_inBed = mean(TotalHoursInBed)) # cal mean sleep hours
ggplot(data= weekly_sleep, aes(x= SleepWeekday, y = Mean_sleephr)) +
  geom_bar(stat = "identity", fill = 'grey', color= 'black') +
  geom_hline(yintercept = 8) +
  labs(title = "Hours asleep per day in a week", x = "Days of Week", y = "Hours")

##corelation study#
ggarrange( 
  ggplot(data = daily_activity, aes(x = TotalSteps, y = Calories)) +
  geom_jitter()+
  geom_smooth(color = "red") +
  labs(title = "Daily Steps vs Calories", x = "Total Steps", y = "Calories"),
  ggplot(data = daily_activity_sleep, aes(x = TotalHoursAsleep, y = Calories)) +
    geom_jitter() +
    geom_smooth(color = "red") +
    labs(title = "Sleep vs Calories", x = "Hours Asleep", y = "Calories"))

#steps vs sleep#
p2 <- ggplot(data = daily_activity_sleep, aes(x = TotalSteps, y = TotalHoursAsleep)) +
  geom_jitter() +
  geom_smooth(color = "red") +
  labs(title = "Daily Steps vs Sleep", x = "Daily Steps", y = "Hours") +
  theme_classic()
  

#Sleep vs inactivity# 
p1 <- ggscatter(daily_activity_sleep, x = "TotalMinutesAsleep", y = "SedentaryMinutes", 
          add = "reg.line",  #add reg line
          add.params = list(color = "red", fill = "lightgray"), #costumize reg line
          conf.int = TRUE,  # Add confidence interval
          title = "Sleep vs inactivity",
          xlab = "Total Minutes Asleep", ylab = "Sendentary Minutes") 
p1 + stat_cor(method = "pearson") #add corelation coefficient
p1 + stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)# Specify the number of decimal places of precision for p and r
                                                    # Using 3 decimal places for the p-value and
                                                    # 2 decimal places for the correlation coefficient (r)
ggarrange(p2,p1+ stat_cor(p.accuracy = 0.001, r.accuracy = 0.01)) #arrange the charts

#Device usage based on days#
daily_use <- daily_activity_sleep %>% 
  group_by(Id) %>% 
  summarise(days_use = sum(n())) %>% 
  mutate(usage = case_when(
    days_use >= 1 & days_use <= 10 ~ "Low use",
    days_use >= 11 & days_use <= 20 ~ "Moderate use",
    days_use >= 21 & days_use <= 35 ~ "High use"
  ))
head(daily_use)

#percent of device usgae for the piechart#
percent_use <- daily_use %>% 
group_by(usage) %>% 
summarise(total = n()) %>% 
mutate(totals = sum(total)) %>% 
group_by(usage) %>% 
summarise(total_percent = total/totals) %>% 
mutate(labels = scales::percent(total_percent))  
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
ggarrange(week_usage, week_usage_1)

#daily use in depth or in min#
daily_use_merged <- merge(daily_activity, daily_use, by = c("Id"))
head(daily_use_merged)
Total_min_worn <- daily_use_merged %>% 
mutate(min_worn = VeryActiveMinutes + FairlyActiveMinutes + LightlyActiveMinutes + SedentaryMinutes) %>% 
mutate(percent_worn = min_worn/1440 * 100) %>% 
mutate(worn = case_when(percent_worn == 100 ~ "All day",
                        percent_worn < 100 & percent_worn >= 50 ~ "More than half a day",
                        percent_worn < 50 & percent_worn >= 0 ~ "Less than half a day"))
head(Total_min_worn)
min_worn_percent <- Total_min_worn %>% 
  group_by(worn) %>% 
  summarise(total = n()) %>%  #count the nr of obv#
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

min_highuse_worn <- Total_min_worn %>% 
  filter(usage == "High use") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

min_moderateuse_worn <- Total_min_worn %>% 
  filter(usage == "Moderate use") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))

min_lowuse_worn <- Total_min_worn %>% 
  filter(usage == "Low use") %>% 
  group_by(worn) %>% 
  summarize(total = n()) %>% 
  mutate(totals = sum(total)) %>% 
  group_by(worn) %>% 
  summarise(total_percent = total/totals) %>% 
  mutate(labels = scales::percent(total_percent))
head(min_worn_percent)
head(min_highuse_worn)
head(min_moderateuse_worn)
head(min_lowuse_worn)

#viz#
T1 <- ggplot(data = min_worn_percent,aes(x = "", y = total_percent, fill= worn)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+ #A minimalistic theme with no background annotations#
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#009999","#00ffff","#ccffff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size = 3.5)+
  labs(title="Time worn per day", subtitle = "Total Users")

T2 <- ggplot(data = min_highuse_worn,aes(x = "", y = total_percent, fill= worn)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+ #A minimalistic theme with no background annotations#
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#009999","#00ffff","#ccffff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size = 3)+
  labs(title="", subtitle = "High use - Users")

T3 <- ggplot(data = min_moderateuse_worn,aes(x = "", y = total_percent, fill= worn)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+ #A minimalistic theme with no background annotations#
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#009999","#00ffff","#ccffff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size = 3)+
  labs(title="", subtitle = "Moderate use - Users")

T4 <- ggplot(data = min_lowuse_worn,aes(x = "", y = total_percent, fill= worn)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start=0)+
  theme_minimal()+ #A minimalistic theme with no background annotations#
  theme(axis.title.x= element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(), 
        panel.grid = element_blank(), 
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.title = element_text(hjust = 0.5, size=14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c("#009999","#00ffff","#ccffff")) +
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5), size = 3)+
  labs(title="", subtitle = "Low use - Users")

ggarrange(T1, ggarrange(T2,T3,T4, ncol = 3), nrow = 2)

  
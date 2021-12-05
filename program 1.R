#installing Librarys
install.packages("tidyverse")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("tibbles")
install.packages("lubridate")
install.packages("janitor")

#loading the packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(tibble)
library(lubridate)
library(janitor)

#importing the Data
dailyActivity <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)
dailycalories <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)
dailyintensities <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)
dailysleepdays <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)
dailyWeight_infos <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)
dailyhourlyintensities <- read.csv(file.choose("C:\\Users\\RITWIK PARIJA\\Desktop\\data analytics project\\data set-1\\bellabeat data set\\New Folder"),header = T)

#Changing format of the Dataset

dailyActivity$activitydate=as.POSIXct(dailyActivity$activitydate, format="%m/%d/%Y", tz=Sys.timezone())
dailysleepdays$sleepday=as.POSIXct(dailysleepdays$sleepday, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
dailyhourlyintensities$ActivityHour=as.POSIXct(dailyhourlyintensities$ActivityHour, format="%m/%d/%Y %I:%M:%S %p", tz=Sys.timezone())
dailyhourlyintensities$time <- format(dailyhourlyintensities$ActivityHour, format = "%H:%M:%S")
dailyhourlyintensities$date <- format(dailyhourlyintensities$ActivityHour, format = "%m/%d/%y")
dailysleepdays$date <- format(sleep$SleepDay, format = "%m/%d/%y")
#getting the Data Frames

tibble(dailyActivity)
tibble(dailycalories)
tibble(dailyintensities)
tibble(dailysleepdays)
tibble(dailyWeight_infos)
tibble(dailyhourlyintensities)

head(dailyActivity)
head(dailycalories)
head(dailyintensities)
head(dailysleepdays)
head(dailyWeight_infos)
head(dailyhourlyintensities)

#Exploring and summarizing data

n_distinct(dailyActivity$Id)
n_distinct(dailycalories$Id)
n_distinct(dailyintensities$Id)
n_distinct(dailysleepdays$Id)
n_distinct(dailyWeight_infos$Id)
n_distinct(dailyhourlyintensities$Id)
#Finding  duplicates

sum(duplicated(dailyActivity))
sum(duplicated(dailycalories))
sum(duplicated(dailyintensities))
sum(duplicated(dailysleepdays))
sum(duplicated(dailyWeight_infos))
sum(duplicated(dailyhourlyintensities))

#Removing Duplicates

dailyActivity <- dailyActivity %>% 
  distinct() %>% 
  drop_na()
dailysleepdays <- dailysleepdays %>% 
  distinct() %>% 
  drop_na()
dailycalories <- dailycalories %>% 
  distinct() %>% 
  drop_na()
dailyWeight_infos <- dailyWeight_infos %>% 
  distinct() %>% 
  drop_na()
dailyintensities <- dailyintensities %>% 
  distinct() %>% 
  drop_na()
dailyhourlyintensities <-dailyhourlyintensities %>% 
  distinct() %>% 
  drop_na()
#checking If the duplicates are removed.

sum(duplicated(dailysleepdays))
sum(duplicated(dailyhourlyintensities))

#cleaning  Data

clean_names(dailyActivity)
dailyActivity <- rename_with(dailyActivity,tolower)
clean_names(dailycalories)
dailycalories <- rename_with(dailycalories,tolower)
clean_names(dailyintensities)
dailyintensities <- rename_with(dailyintensities,tolower)
clean_names(dailysleepdays)
dailysleepdays <- rename_with(dailysleepdays,tolower)
clean_names(dailyWeight_infos)
dailyWeight_infos <- rename_with(dailyWeight_infos,tolower)
clean_names(dailyhourlyintensities)
dailyhourlyintensities <- rename_with(dailyhourlyintensities,tolower)

#summarizing the datasets

dailyActivity %>% 
  select(totalsteps,totaldistance,sedentaryminutes,calories) %>% 
  summary()
dailyActivity %>% 
  select(veryactiveminutes,fairlyactiveminutes,lightlyactiveminutes) %>% 
  summary()
dailycalories %>% 
  select(calories) %>% 
  summary()
dailyintensities %>% 
  select(sedentaryminutes,lightlyactiveminutes,fairlyactiveminutes,veryactiveminutes) %>% 
  summary()
dailyintensities %>% 
  select(sedentaryactivedistance,lightactivedistance,moderatelyactivedistance,veryactivedistance) %>% 
  summary()
dailysleepdays %>% 
  select(totalsleeprecords,totalminutesasleep,totaltimeinbed) %>% 
  summary()
dailyWeight_infos %>% 
  select(weightkg,bmi) %>% 
  summary()
dailyhourlyintensities %>% 
  select(totalintensity,averageintensity) %>% 
  summary()


#visualising data
ggplot(data = dailyActivity)+
  geom_point(mapping = aes(x=totalsteps,y=calories))

ggplot(data = dailyActivity)+
  geom_smooth(mapping = aes(x=totalsteps,y=calories))

ggplot(data=dailyActivity, aes(x=totalsteps, y=calories)) + 
  geom_point() + geom_smooth() + labs(title="Totalsteps vs. Calories")

ggplot(data=dailysleepdays, aes(x=totalminutesasleep, y=totaltimeinbed)) + 
  geom_point()+ labs(title="Total Minutes Asleep vs. Total Time in Bed")

int_new <- dailyhourlyintensities %>% group_by(time) %>% 
  drop_na() %>% 
  summarise(mean_total_int=mean(TotalIntensity))
ggplot(data=int_new, aes(x=time, y=mean_total_int)) + geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")

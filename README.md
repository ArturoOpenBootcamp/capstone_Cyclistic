MARKDOWN FINAL

---
title: "Cyclistic Bike-share Project"
author: "Arturo Bravo"
date: "2023-12-22"
output:
  html_document: default
  

---

```{r setup, echo=TRUE, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(scales)
library(tinytex)

```

```{r echo=TRUE, include=FALSE}
junetype     <- read.csv("Data semester/Averagetime/juneaverage2023.csv")
julytype     <- read.csv("Data semester/Averagetime/julyaverage2023.csv")
augusttype   <- read.csv("Data semester/Averagetime/augustaverage2023.csv") 
septembertype<- read.csv("Data semester/Averagetime/septemberaverage2023.csv")
octobertype  <- read.csv("Data semester/Averagetime/octoberaverage2023.csv")
novembertype <- read.csv("Data semester/Averagetime/novemberaverage2023.csv")
membertype <- rbind(junetype,julytype,augusttype,septembertype,octobertype,novembertype)
```
 
## Introduction
Cyclistic is a bike-share program operating in Chicago, providing diverse bicycles and docking stations. This project aims to explore distinct usage patterns between annual members and casual riders.

 <img src="https://cdn.pixabay.com/photo/2014/06/25/15/50/bike-377249_1280.jpg" alt= "Cyclistic" width="380" height ="330">
 

The program is operating in Chicago with over 5,800 bicycles and 600 docking stations, offering three types of bikes: **Docked, Electrical, and Classic.**.

The marketing goal is to convert casual riders into annual riders. In order to achieve this, the marketing team needs a better understanding of how annual members and casual riders differ. 

## User Demographics
Cyclistic ridership comprises members and casual users. In the second semester of 2023, 60% are members, while casual users show a slight increase in summer.


```{r Data in total,echo=FALSE,include=TRUE, results='asis', warning=FALSE}
suppressWarnings({
  
  result <-membertype %>% select(member_casual) %>% group_by(member_casual) %>% summarize (Sumatoria = n())
  porcentaje <- result$Sumatoria / sum(result$Sumatoria)* 100
  
 ggplot(result, aes(x = "", y= (Sumatoria), fill= member_casual)) +
  geom_bar(stat= "identity", width=1)+
    coord_polar("y") +
    theme_void()+
    
    
  labs(title= "             Total Users in the Second Semester of 2023") +
  guides(fill = guide_legend(title = "Type of Users")) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)),
  position = position_stack(vjust = 0.5)) +
  annotate("text", x = 0.1, y = 0.1, label = sprintf("Total: %s", comma(sum(result$Sumatoria)))) 
 

})
```

```{r Data in Summer,echo=FALSE,include=TRUE, results='asis', warning=FALSE}
suppressWarnings({
  summertype <- rbind(junetype,julytype,augusttype)
  resultSummer <-summertype %>% select(member_casual) %>% group_by(member_casual) %>% summarize (Sumatoria = n())
  porcentajeSummer <- resultSummer$Sumatoria / sum(resultSummer$Sumatoria)* 100
  
 ggplot(resultSummer, aes(x = "", y= (Sumatoria), fill= member_casual)) +
  geom_bar(stat= "identity", width=1)+
    coord_polar("y") +
    theme_void()+
    
    
  labs(title= "             Total Users in Summer-2023(Jun-Aug)") +
  guides(fill = guide_legend(title = "Type of Users")) +
  geom_text(aes(label = sprintf("%.1f%%", porcentajeSummer)),
  position = position_stack(vjust = 0.5)) +
  annotate("text", x = 0.1, y = 0.1, label = sprintf("Total: %s", comma(sum(resultSummer$Sumatoria)))) 
 

})
```

In the current overview, it's apparent that 6 out of 10 users are members. When we narrow our focus to the summer months (June-August), a slight variation of 5.05% in favor of casual users becomes noticeable. 


## Bike type Preference
Moving forward, our attention will be directed to the type of bicycle chosen by each user category. The majority of members prefer classic and electric bikes, while casual users show a slight preference for electric bikes. Notably, docked bikes are exclusively used by casual riders.

```{r Pie Chart Type of Bicycles for Members, echo=FALSE,include=TRUE, results='asis', warning=FALSE}

suppressWarnings({
  result <- membertype %>% select(member_casual, rideable_type) %>% 
  filter(member_casual == "member") %>% 
  group_by(rideable_type) %>% 
  summarize(TotalbyCategory = n())
  porcentaje <- result$TotalbyCategory/ sum(result$TotalbyCategory)*100
  
  ggplot(result, aes(x = "", y= TotalbyCategory, fill= rideable_type)) +
  geom_bar(stat= "identity", width=1)+
    coord_polar("y") +
    theme_void() +
    
  labs(title="Preferences of Bike Types for Members - Second Semester of 2023")+
    scale_fill_manual(values = c("classic_bike" = "cornflowerblue", "electric_bike" = "coral1", docked_bike = "goldenrod1"))+
  guides(fill = guide_legend(title = " Type of Bikes"))  +
  geom_text(aes(label = sprintf("%.1f%%",porcentaje)),
            position = position_stack(vjust = 0.5))+
  annotate("text", x=0.1, y = 0.1, size = 4, label = sprintf("Total %s",  comma(sum(result$TotalbyCategory))))
 
   
})
```
<small>  
The preferences for electric and classic bicycles are quite similar. However, it's noteworthy that docked bicycles **ARE NOT UTILIZED BY THE MEMBERS**.</small>


```{r Pie Chart Type of Bicycles for Casuals, echo=FALSE,include=TRUE,warning=FALSE}

suppressWarnings({
  resultcasual <- membertype %>% select(member_casual, rideable_type) %>% 
  filter(member_casual == "casual") %>% 
  group_by(rideable_type) %>% 
  summarize(TotalbyCategory = n())
  porcentajecasual <- resultcasual$TotalbyCategory/ sum(resultcasual$TotalbyCategory)*100
  
  ggplot(resultcasual, aes(x = "", y= TotalbyCategory, fill= rideable_type)) +
  geom_bar(stat= "identity", width=1)+
    coord_polar("y") +
    theme_void() +
    
  labs(title= "Preferences of Bike Types for Casuals - Second Semester of 2023")+
    scale_fill_manual(values = c("classic_bike" = "cornflowerblue", "electric_bike" = "coral1", docked_bike = "goldenrod1"))+
  guides(fill = guide_legend(title = "Types of Bikes"))  +
  geom_text(aes(label = sprintf("%.1f%%",porcentajecasual)),
            position = position_stack(vjust = 0.5))+
  annotate("text", x=0.1, y = 0.1, size= 4, label = sprintf("Total %s", 
            comma(sum(resultcasual$TotalbyCategory))))
    
})

```

It is now understood that members exhibit a 50% preference for classic and electric bicycles. In contrast, casual users show a slight inclination towards electric bicycles, closely followed by the classic bicycle. Notably, casual users are the exclusive group utilizing docked 


## Average Trip Duration
Members and casual users show different patterns in average trip duration throughout the week. Members tend to have longer trips on weekends, while casual users have more evenly distributed trip durations.

```{r average rides per days, echo=FALSE,include=TRUE,warning=FALSE}
suppressWarnings({
tripaverage <-select(membertype, member_casual, started_at, ended_at,day_of_week)
tripaverage$started_at <- as.POSIXct(tripaverage$started_at, format= "%m/%d/%Y %H:%M")
tripaverage$ended_at <- as.POSIXct(tripaverage$ended_at, format= "%m/%d/%Y %H:%M")
tripaverage$averagetrip <- as.numeric(difftime(tripaverage$ended_at, tripaverage$started_at, units = "mins"))
averagetotal <- tripaverage %>% 
  group_by(day_of_week, member_casual) %>%
  summarize(averagetrip = mean(averagetrip, na.rm = TRUE))

dayarranged <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
averagetotal$day_of_week <- factor(averagetotal$day_of_week, levels = dayarranged, ordered = TRUE)

ggplot(averagetotal, aes(x=day_of_week, y= averagetrip, fill = member_casual))+
  geom_bar(stat = "identity", position = "dodge", color="black")+
  geom_text(aes(label = comma(round(averagetrip,2))),
            position = position_dodge(width = 0.9), vjust = -0.5)+
   labs(title = "Average Trip Duration for Members by Day of Week",
       x = NULL,
       y = "Average Trip Duration (minutes)",
       fill= NULL)+
theme(axis.text = element_text(size = 5))+
facet_wrap(~member_casual)+
  theme_minimal()
})
```
  
  
## Total Trips by Day
"The total number of trips varies each day, with weekends being more popular for casual users and weekdays for members. Thursday stands out as the most popular day among members, and there's a decrease in trips until Monday. Meanwhile, Saturday is the most popular day for casual members, and similarly, there's a decrease in trips until Monday as well."
```{r total rides,echo=FALSE,include=TRUE,warning=FALSE}
suppressWarnings({
countrides <- membertype %>% group_by(day_of_week, member_casual) %>%
  summarize(totalrides = n())

countrides$day_of_week <- factor(countrides$day_of_week, levels = dayarranged, ordered= TRUE)
ggplot(countrides, aes(x = day_of_week, comma(totalrides), group = member_casual, colour = member_casual))+
  geom_line() +
  geom_text(aes(label = comma(totalrides)))+
  labs(title = "Total Trips by Day for the whole 2023 second semester",
       x = NULL,
       y = "Total Trips",
       colour = NULL)+
theme_minimal()
})
  
```
  
## Total Trips by Hour
Trips per hour are calculated throughout the entire week. For members, there is a noticeable increasing trend from 04:00 hours onwards, reaching the first maximum around 8:00 hours and the highest peak around 18:00 hours. Meanwhile, casual users exhibit a similar pattern to members, but the peaks are not as high
```{r, echo=FALSE,include=TRUE,warning=FALSE}
suppressWarnings({
  
memberty <- rbind(membertype)
memberty$started_at <- as.POSIXct(memberty$started_at, format= "%m/%d/%Y %H:%M")


totaltime <- memberty %>% 
  mutate(timehour = format(memberty$started_at, "%H"))

totaltime <-select(totaltime, member_casual, timehour) %>% 
  group_by(member_casual,timehour) %>% summarize(totalhours = n()) 




ggplot(totaltime, aes(x = timehour, y = comma(totalhours), group = member_casual, color = member_casual)) +
  geom_line(position = "dodge") +
geom_text(
  data = totaltime %>% filter(timehour %in% c("02","04","06","08","10","12","15","17","19","21","23")),
  aes(label = comma(totalhours)), 
  position = position_dodge(width = 0.9), vjust = -0.5
)+
  labs(title = "Total Trips per Hour",
       x = "Hours",
       y = "Trips per Hours",
       color = NULL) +
  theme_minimal()
})
```
  
  
<small> it's essential to note that the necessary data for this case study is sourced from Coursera/Google, and it pertains to a real company available on the [Divvy website](https://divvy-tripdata.s3.amazonaws.com/index.html). The data has been generously provided by Motivate International Inc. under the terms of a specific [license](https://divvybikes.com/data-license-agreement)

The information was initially in CSV format and underwent cleaning in Excel. This process involved:
*Formatting the data
*Removing bias
*Verifying credibility
*Identifying and addressing any instances of dirty data <small> 

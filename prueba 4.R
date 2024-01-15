library(tidyverse)
library(scales)
June_2023 <-select(averagejune2023, member_casual, started_at, ended_at,day_of_week)
June_2023$started_at <- as.POSIXct(June_2023$started_at, format= "%m/%d/%Y %H:%M")
June_2023$ended_at <- as.POSIXct(June_2023$ended_at, format = "%m/%d/%Y %H:%M")
June_2023$averagetrip <- as.numeric(difftime(June_2023$ended_at, June_2023$started_at, units = "mins"))
table_june <- table(June_2023$member_casual)
print(table_june)
totalaverage <- June_2023 %>%  
  group_by(day_of_week,member_casual) %>% 
  summarize(averagebytrip = mean(averagetrip, na.rm = TRUE))

averagedays <- filter(June_2023, member_casual == "member", day_of_week %in% 
                        c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))

dayarranged <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
totalaverage$day_of_week <- factor(totalaverage$day_of_week, levels = dayarranged, ordered = TRUE)
June_2023$hour <- format(June_2023$started_at, "%H") %>% factor(levels = hoursarranged, ordered = TRUE)

totalhours <-select(June_2023, member_casual, hour) %>% group_by(hour, member_casual) 





ggplot(June_2023, aes(x = hour, y = ..count.., group = member_casual, color = member_casual)) +
  geom_line(stat = "count", position = "dodge") +
  geom_text(
    aes(x = hour, y = max(..count..), label = ..count..),
    stat = "count",
    position = position_dodge(wxidth = 0.9),
    vjust = -0.5,
    color = "black",
    show.legend = FALSE
  )




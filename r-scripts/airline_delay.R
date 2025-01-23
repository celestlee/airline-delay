## ST2195 COURSEWORK PROJECT QUESTION 1 
## Date: 05 Mar 2022
## Author: celestlee

# Setting working directory 
setwd("~/Desktop/data_files/dataverse_files")

# Loading and importing relevant libraries
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("ragg")
install.packages("cowplot")
library(ragg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)

# Loading files -- Years 2005 to 2007 and binding it into data frame "years"
years <- rbind(read.csv("2005.csv.bz2"), # takes a really long time to load!
               read.csv("2006.csv.bz2"),
               read.csv("2007.csv.bz2"))

str(years)
summary(years)

#################################################################################################
# 1. When is the best time of day, day of the week, and time of year to fly to minimise delays?
#################################################################################################

# Setting variables as factors
years$Month <- as.factor(years$Month)
years$Year <- as.factor(years$Year)
years$DayOfWeek <- as.factor(years$DayOfWeek)
years$DayofMonth <- as.factor(years$DayofMonth) 


# Adding new column to data for flight status
years$status <- NA
years$status[years$Diverted == 1] <- "Diverted"
years$status[years$DepDelay > 0] <- "Delayed"
years$status[years$Cancelled == 1] <- "Cancelled"
years$status <- ifelse(years$Diverted != 1 & years$DepDelay <= 0 & 
                         years$Cancelled != 1, "On Time", years$status)
years$status <- as.factor(years$status)
summary(years$status)

# Bar plot of status of flights in percentage
status_perc <- years %>% 
  count(status) %>% 
  mutate(perc = n / nrow(years) * 100)

perc_label <- c("0", "20", "40", "60")
status_perc %>%
  ggplot(aes(x = status, y = perc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = paste0(perc_label, "%")) +
  geom_text(aes(label = paste0(round(perc,2), "%")), position = position_dodge(width = 0.9), vjust = -0.4) + 
  labs(title = "Percentage of status for all flights between years 2005 to 2007", x = "Flight Status", y = "Percentage") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 

# Delay rate by month (Line chart)
month_label <- c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov","Dec") 
month_delay <- years %>% 
  select(Month, status) %>%
  group_by(Month) %>%
  summarize(num_delays = sum(status == 'Delayed'),
            num_flights = n(),
            delay_rate = sum(status == 'Delayed') / n())

plot_month <- month_delay %>%
  ggplot(aes(x = Month, y = delay_rate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  scale_x_discrete(labels = month_label) +
  labs(title = "Month", y ="") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 
plot_month

# Delay rate by day of month
day_delay <- years %>% 
  select(DayofMonth, status) %>%
  group_by(DayofMonth) %>%
  summarize(num_delays = sum(status == 'Delayed'),
            num_flights = n(),
            delay_rate = sum(status == 'Delayed') / n())

plot_day <- day_delay %>%
  ggplot(aes(x = DayofMonth, y = delay_rate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  labs(title = "Day of Month", y ="") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 
plot_day

# Delay rate by day of week
week_label <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
week_delay <- years %>% 
  select(DayOfWeek, status) %>%
  group_by(DayOfWeek) %>%
  summarize(num_delays = sum(status == 'Delayed'),
            num_flights = n(),
            delay_rate = sum(status == 'Delayed') / n())

plot_week <- week_delay %>%
  ggplot(aes(x = DayOfWeek, y = delay_rate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  scale_x_discrete(labels = week_label) +
  labs(title = "Day of Week", y = "") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 
plot_week

# Delay rate by hour #

# Categorising all DepTime as 'Normal' or 'Unusual'
all_hours <- years %>%
  select(DepTime, status) %>%
  drop_na(DepTime) %>%
  mutate(dep_hour = ifelse(
    nchar(DepTime) == 1 | nchar(DepTime) == 2 | DepTime > 2400,
    "Unusual", "Normal"))

# Calculating % of unusual DepTime 
perc_hour <- all_hours %>%
  count(dep_hour) %>%
  mutate(hour_perc = n / nrow(all_hours) * 100)

# Calculating % of "Unusual" DepTime 
perc_hour <- all_hours %>%
  count(dep_hour) %>%
  mutate(hour_perc = n / nrow(all_hours) * 100)

# Creating plot labels for `hour_perc` 
perc_label1 <- c("0", "25", "50", "75", "100")

# Bar plot of % of Normal vs Unusual timings
perc_hour %>%
  ggplot(aes(x = dep_hour, y = hour_perc)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  scale_y_continuous(labels = paste0(perc_label1, "%")) +
  geom_text(aes(label = paste0(round(hour_perc,2), "%")), position = position_dodge(width = 0.9), vjust = -0.4) + 
  labs(title = "Percentage of Normal vs Unusual Departure Timings", x = "", y = "Percentage") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

# Data cleansing
normal_hours <- all_hours %>%
  filter(dep_hour == "Normal")
    
normal_hours1 <- normal_hours %>%
  filter(nchar(DepTime) == 3) %>%
  mutate(new_hour = signif(DepTime, 1)) %>%
  mutate(new_hour = case_when(
    new_hour == 100 ~ "0100",
    new_hour == 200 ~ "0200",
    new_hour == 300 ~ "0300",
    new_hour == 400 ~ "0400",
    new_hour == 500 ~ "0500",
    new_hour == 600 ~ "0600",
    new_hour == 700 ~ "0700",
    new_hour == 800 ~ "0800",
    new_hour == 900 ~ "0900",
    TRUE ~ "1000"
  ))
normal_hours1$new_hour <- as.factor(normal_hours1$new_hour)

normal_hours2 <- normal_hours %>%
  filter(nchar(DepTime) == 4) %>%
    mutate(new_hour = signif(DepTime, 2))
normal_hours2$new_hour <- as.factor(normal_hours2$new_hour)

normal_hours <- rbind(normal_hours1, normal_hours2)
summary(normal_hours$new_hour)

# Delay rate by hour
hour_delay <- normal_hours %>% 
  select(new_hour, status) %>%
  group_by(new_hour) %>%
  summarize(num_delays = sum(status == 'Delayed'),
            num_flights = n(),
            delay_rate = sum(status == 'Delayed') / n())

plot_hour <- hour_delay %>%
  ggplot(aes(x = new_hour, y = delay_rate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  labs(title = "Hour", y = "") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 
plot_hour

# Grid of all plots (cowplot)
p <- plot_grid(plot_month, plot_day, plot_week, plot_hour, scale = 1)
title <- ggdraw() + draw_label("Delay Rates based on:", fontface='bold', size = 17)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

## ST2195 COURSEWORK PROJECT QUESTION 2
## Date: 09 Mar 2022
## Author: celestlee

# Setting working directory 
setwd("~/Desktop/data_files/dataverse_files")

# Loading and importing relevant libraries
install.packages("ragg")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("cowplot")
install.packages("reshape2")

library(ragg)
library(ggplot2)
library(dplyr)
library(tidyr)
library(cowplot)
library(reshape2)

# creating tables saved in csv files
planes <- read.csv("plane-data.csv", header = TRUE)

# Loading files -- Years 2005 to 2007 and binding it into data frame "years"
years <- rbind(read.csv("2005.csv.bz2"), # takes a really long time to load!
               read.csv("2006.csv.bz2"))

str(years)
summary(years)

######################################################################
# 2. Do older planes suffer more delays?
######################################################################

# Creating new dataframe
carrier <- years %>%
  select(UniqueCarrier, TailNum, DepDelay, ArrDelay, Year, CarrierDelay, WeatherDelay, NASDelay, SecurityDelay, LateAircraftDelay)
colnames(carrier)[2] <- "tailnum"

colSums(is.na(planes)|planes==0) 

perc <- planes %>% 
  filter(year < 1980) %>%
  count(year) %>% 
  mutate(perc = n / nrow(planes) * 100)


planes$year <- as.numeric(planes$year)
planes <- subset(planes, is.na(year) == F)
summary(planes$year)
plane_year <- planes %>%
  select(tailnum, year) %>%
  filter(year != 0)

colnames(plane_year)[2] <- "planeyear"
carrier <- carrier %>%
  inner_join(plane_year) ## joining by tailnum
carrier$Year <- as.integer(as.character(carrier$Year))

summary(carrier)
str(carrier)


# Histogram of year of manufacture of planes
hist <- plane_year %>%
  ggplot(aes(x=planeyear)) +
  geom_histogram(fill = "steelblue", binwidth = 1) +
  labs(title = "Year of Manufacture", y = "Frequency") + 
  geom_vline(xintercept=c(1980), linetype="dotted") +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold")) +
  theme(legend.position = "none") 

# Preparing data for line charts
plane_age <- carrier %>%
  mutate(age = Year - planeyear) %>%
  group_by(age) %>%
  summarise(
    mean_depdelay = mean(DepDelay, na.rm = TRUE),
    mean_arrdelay = mean(ArrDelay, na.rm = TRUE))

summary(plane_age$age)
plane_age <- plane_age %>%
  filter(age > 0)

# Line chart of Mean departure delay (all)
p1 <- ggplot(plane_age, aes(x=age, y=mean_depdelay, group=1)) +
  geom_line(color = "blue") +
  labs(x = "Years of service", y = "Mean Departure Delay (in minutes)") +
  geom_vline(xintercept=c(25), linetype="dotted") +
  theme_classic()

# Line chart of Mean arrival delay (all)
p2 <- ggplot(plane_age, aes(x=age, y=mean_arrdelay, group=1)) +
  geom_line(color = "blue") +
  labs(x = "Years of service", y = "Mean Arrival Delay (in minutes)") +
  geom_vline(xintercept=c(25), linetype="dotted") +
  theme_classic()


# Grid of all plots (cowplot)
bottom_row1 <- plot_grid(p1,p2)
plot_grid(hist, bottom_row1, nrow = 2)


# Pre 1980 (Older) Line chart
older_planes <- plane_age %>%
  filter(age > 25)
colnames(older_planes) <- c("age", "Mean Departure Delay", "Mean Arrival Delay")
older_planes <- melt(older_planes, id.vars = "age")

plot_cols = c("#e48f1b", "#619ed6")

p3 <- ggplot(older_planes, aes(x = age, y = value)) + 
  geom_line(aes(color = variable)) +
  labs(title = "Planes manufactured before the 1980s" ,x = "Years of service", y = "Mean Delay (in minutes)") + 
  scale_color_manual(values = plot_cols) +
  ylim(2,15) +
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

# Post 1980 (Younger) Line chart
younger_planes <- plane_age %>%
  filter(age < 25)
colnames(younger_planes) <- c("age", "Mean Departure Delay", "Mean Arrival Delay")
younger_planes <- melt(younger_planes, id.vars = "age")

p4 <- ggplot(younger_planes, aes(x = age, y = value)) + 
  geom_line(aes(color = variable)) +
  labs(title = "Planes manufactured after the 1980s" ,x = "Years of service", y = "Mean Delay (in minutes)") + 
  scale_color_manual(values = plot_cols) +
  ylim(2,15) +
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

# Grid plot of both line charts
plot_grid(p3,p4)

# Grouped bar chart of delay factors
delay_factors <- carrier %>%
  mutate(age = Year - planeyear) %>%
  filter(age > 0) %>%
  group_by(age) %>%
  summarise(
    carrier_delay = mean(CarrierDelay, na.rm = TRUE),
    weather_delay = mean(WeatherDelay, na.rm = TRUE),
    nas_delay = mean(NASDelay, na.rm = TRUE),
    security_delay = mean(SecurityDelay, na.rm = TRUE),
    aircraft_delay = mean(LateAircraftDelay, na.rm = TRUE))
colnames(delay_factors) <- c("age", "Carrier Delay", "Weather Delay", "NAS Delay", "Security Delay", "Late Aircraft Delay")

delay_factors <- melt(delay_factors, id.vars = "age")
delay_factors <- delay_factors %>%
  mutate(group = ifelse(
    age > 25, "Over 25 Years", "Below 25 Years"))
delay_factors <- dcast(delay_factors, group + variable ~ ., sum)
colnames(delay_factors)[3] <- "value"

grouped_bar <- delay_factors %>%
  ggplot(aes(fill=group, y=value, x=variable)) + 
  geom_bar(position="dodge", stat = "identity", width = 0.7) + 
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = plot_cols) +
  labs(title = "Delay Factors based on Years of service ", x = "", y = "Mean Delay (minutes)") + 
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

# Preparing data for 3x1 line chart
delay_lines <- carrier %>%
  mutate(age = Year - planeyear) %>%
  filter(age > 0) %>%
  group_by(age) %>%
  summarise(
    mean_carrierdelay = mean(CarrierDelay, na.rm = TRUE),
    mean_NASdelay = mean(NASDelay, na.rm = TRUE),
    mean_aircraftdelay = mean(LateAircraftDelay, na.rm = TRUE))

# 3x1 Line chart
p5 <- ggplot(delay_lines, aes(x=age, y=mean_carrierdelay, group=1)) +
  geom_line(color = "blue") +
  ylim(1,7) +
  labs(x = "Years of service", y = "Mean Carrier Delay (in minutes)") +
  theme_classic()  

p6 <- ggplot(delay_lines, aes(x=age, y=mean_NASdelay, group=1)) +
  geom_line(color = "blue") +
  ylim(1,7) +
  labs(x = "Years of service", y = "Mean NAS Delay (in minutes)") +
  theme_classic()  

p7 <- ggplot(delay_lines, aes(x=age, y=mean_aircraftdelay, group=1)) +
  geom_line(color = "blue") +
  ylim(1,7) +
  labs(x = "Years of service", y = "Mean Late Aircraft Delay (in minutes)") +
  theme_classic() 

# Grid plot
bottom_row2 <- plot_grid(p5, p6, p7, nrow = 1)
plot_grid(grouped_bar, bottom_row2, ncol = 1)

##########################################################################################
# 3.How does the number of people flying between different locations change over time?
##########################################################################################

## ST2195 COURSEWORK PROJECT QUESTION 3
## Date: 09 Mar 2022
## Author: celestlee

# Origin-Dest pairs
od_pairs <- years %>% 
  group_by(Year, Origin, Dest) %>% 
  summarize(num_trips = n()) %>%
  arrange(desc(num_trips))
od_pairs$combi <- paste0(od_pairs$Origin,"/",od_pairs$Dest)
od_pairs$combi <- as.factor(od_pairs$combi)
od_pairs$Year <- as.factor(od_pairs$Year)

sum_od <- od_pairs %>%
  group_by(combi) %>%
  mutate(sum_trips = sum(num_trips) / 3) %>%
  arrange(desc(sum_trips))
sum_od <- head(sum_od, n = 30) # top 10 OD in 3 years

### For own reference (OD with most traffic in 3 years -- Plot grouped chart of this)
sum_od %>% 
  ggplot(aes(y = combi, x = sum_trips)) + 
  geom_bar(stat = "identity", width = 0.7, fill = "steelblue") + 
  scale_x_continuous(labels = comma) +
  labs(title = "Origin-Destination Combinations with most trips", x = "Count", y = "Origin-Destination") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(legend.position = "right") 

top_od_pairs <- od_pairs %>%
  filter(combi == "SAN/LAX" |
         combi == "OGG/HNL" |
         combi == "LGA/DCA" |
         combi == "LGA/BOS" |
         combi == "LAX/SAN" |
         combi == "LAX/LAS" |
         combi == "LAS/LAX" |
         combi == "HNL/OGG" |
         combi == "DCA/LGA" |
         combi == "BOS/LGA")
plot_cols = c("#e48f1b", "steelblue", "#aeaeae")
p1 <- top_od_pairs %>% 
  ggplot(aes(fill = Year, y = combi, x = num_trips)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.7) + 
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = plot_cols) +
  labs(title = "Top 10 Origin-Destination Combinations", x = "Number of trips", y = "Origin-Destination") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold")) 

unusual_pairs <- od_pairs %>%
  filter(Year == 2005) %>%
  mutate(num_2005 = num_trips) 
unusual_pairs = subset(unusual_pairs, select = c("combi", "num_2005"))

unusual_pairs1 <- od_pairs %>%
  filter(Year == 2006) %>%
  mutate(num_2006 = num_trips) 
unusual_pairs1 = subset(unusual_pairs1, select = c("combi", "num_2006"))

unusual_pairs2 <- od_pairs %>%
  filter(Year == 2007) %>%
  mutate(num_2007 = num_trips) 
unusual_pairs2 = subset(unusual_pairs2, select = c("combi", "num_2007"))

odd_pairs <- unusual_pairs %>%
  inner_join(unusual_pairs1) %>%
  inner_join(unusual_pairs2) 
odd_pairs$max <- pmax(odd_pairs$num_2005, odd_pairs$num_2006, odd_pairs$num_2007)
odd_pairs$min <- pmin(odd_pairs$num_2005, odd_pairs$num_2006, odd_pairs$num_2007)
odd_pairs$max_diff <- odd_pairs$max - odd_pairs$min
odd_pairs1 <- odd_pairs %>%
  arrange(desc(max_diff))
odd_pairs1 <- head(odd_pairs1, n = 10)

odd_pairs1 <- melt(odd_pairs1, id.vars = "combi")
odd_pairs1 <- odd_pairs1 %>%
  mutate(case_when(
    variable == "num_2005" ~ "2005",
    variable == "num_2006" ~ "2006",
    variable == "num_2007" ~ "2007")) 
colnames(odd_pairs1) <- c("combi", "variable", "value", "Year")
odd_pairs1 <- odd_pairs1 %>%
  filter(variable != "max_diff")
odd_pairs1 <- head(odd_pairs1, n = 30)
  
p2 <- odd_pairs1 %>% 
  ggplot(aes(fill = Year, y = combi, x = value)) + 
  geom_bar(position = "stack", stat = "identity", width = 0.7) + 
  scale_x_continuous(labels = comma) +
  scale_fill_manual(values = plot_cols) +
  labs(title = "Origin-Destination Combinations with significant changes in traffic", x = "Number of trips", y = "Origin-Destination") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold")) 

plot_grid(p1,p2)

# Line chart of top 5 combinations
line_pairs <- years %>% 
  select(Year, Month, DayofMonth, Origin, Dest) %>%
  group_by(Year, Month, DayofMonth, Origin, Dest) %>%
  summarize(num_trips = n()) %>%
  arrange(desc(num_trips))

line_pairs$combi <- paste0(line_pairs$Origin,"/",line_pairs$Dest)
line_pairs$combi <- as.factor(line_pairs$combi)
line_pairs$Year <- as.factor(line_pairs$Year)

top_5 <- line_pairs %>%
  filter(combi == "SAN/LAX" |
         combi == "LAX/SAN" | 
         combi == "LAX/LAS" |
         combi == "LAS/LAX" |
         combi == "BOS/LGA")
top_5$date <- as.Date(paste0(top_5$Year,"-",top_5$Month,"-",top_5$DayofMonth))

top_5 <- top_5 %>%
  group_by(month = floor_date(date, unit = "month"))

mean_line_pairs <- top_5 %>%
  group_by(combi, month) %>%
  summarise(
    mean_trips = mean(num_trips)
  )

plot_cols2 = c("steelblue", "#e48f1b", "#aeaeae", "#f7d027", "steelblue2")
p3 <- ggplot(mean_line_pairs, aes(x = month, y = mean_trips, color = combi)) + 
  geom_line(size = 0.5) +
  scale_color_manual(values = plot_cols2) + 
  labs(title = "Traffic of Top 5 Origin-Destination Combinations over 3 years" , x = "", y = "Mean number of trips") + 
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

# Line chart of top 5 combinations with most changes
change_5 <- line_pairs %>%
  filter(combi == "OGG/HNL" |
         combi == "HNL/OGG" | 
         combi == "KOA/HNL" |
         combi == "HNL/LIH" |
         combi == "HNL/KOA")
change_5$date <- as.Date(paste0(change_5$Year,"-",change_5$Month,"-",change_5$DayofMonth))

change_5 <- change_5 %>%
  group_by(month = floor_date(date, unit = "month"))

mean_change_pairs <- change_5 %>%
  group_by(combi, month) %>%
  summarise(
    mean_trips = mean(num_trips))

p4 <- ggplot(mean_change_pairs, aes(x = month, y = mean_trips, color = combi)) + 
  geom_line(size = 0.5) +
  scale_color_manual(values = plot_cols2) + 
  labs(title = "Traffic of Origin-Destination Combinations with significant increases over 3 years" , x = "", y = "Mean number of trips") + 
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

plot_grid(p3,p4)


## ST2195 COURSEWORK PROJECT QUESTION 4
## Date: 09 Mar 2022
## Author: celestlee

################################################################################################
# 4. Can you detect cascading failures as delays in one airport create delays in others?
################################################################################################

# Setting working directory 
setwd("~/Desktop/data_files/dataverse_files")

install.packages("ragg")
install.packages("RSQLite")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("geosphere")
install.packages("igraph")
library(ragg)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(reshape2)
library(scales)

future::plan()

# Loading files -- Years 2005 to 2007 and binding it into data frame "years"
years <- rbind(read.csv("2005.csv.bz2"), # takes a really long time to load!
               read.csv("2006.csv.bz2"))

str(years)
summary(years)

# Preparing data frame

# Setting variables as factors
years$Month <- as.factor(years$Month)
years$DayofMonth <- as.factor(years$DayofMonth) 

# Adding new column to data for flight status
years$dep_delayed <- "No"
years$dep_delayed[years$DepDelay > 0] <- "Yes"
years$arr_delayed <- "No"
years$arr_delayed[years$ArrDelay > 0] <- "Yes"
years$dep_delayed <- as.factor(years$dep_delayed)
years$arr_delayed <- as.factor(years$arr_delayed)
summary(years$dep_delayed)
summary(years$arr_delayed)

### Plot one
# plot for delay rate per month (diffrent from question 1 -- ArrDelay has been factored in)
month_label <- c("Jan","Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov","Dec") 
month_delay <- years %>% 
  select(Month, dep_delayed, arr_delayed) %>%
  group_by(Month) %>%
  summarise(num_delays = sum(dep_delayed == 'Yes' | arr_delayed == 'Yes'),
            num_flights = n(),
            delay_rate = num_delays / n())

p1 <- month_delay %>%
  ggplot(aes(x = Month, y = delay_rate, group = 1)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue") + 
  scale_x_discrete(labels = month_label) +
  labs(title = "Delay Rate per Month", y = "Delay Rate") +
  theme_classic() +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 

# histogram for number of flights by month
bar_grouped <- years %>%
  select(Month, Year) %>%
  group_by(Month, Year) %>%
  summarise(num_flights =  n())
bar_grouped$Year <- factor(bar_grouped$Year)

plot_cols = c("#e48f1b", "steelblue", "#aeaeae")

p2 <- bar_grouped %>%
  ggplot(aes(fill = Year, y = num_flights, x = Month)) + 
  geom_bar(position="stack", stat = "identity", width = 0.7) + 
  scale_fill_manual(values = plot_cols) +
  scale_x_discrete(labels = month_label) +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Number of flights", x = "", y = "Frequency") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

plot_grid(p1,p2, align = "h")

# third plot: num_arr and num_dep in a month (month with highest delay rate)
first_month <- years %>%
  select(Month, dep_delayed, arr_delayed, DayofMonth) %>%
  filter(Month == 12) %>%
  group_by(DayofMonth) %>%
  summarise(num_arr = sum(arr_delayed == 'Yes'),
            num_flights = n(),
            arr_rate = num_arr / n(),
            num_dep = sum(dep_delayed == 'Yes'),
            num_flights = n(),
            dep_rate = num_dep / n())

first_month <- first_month[c(1,4,6)]
colnames(first_month) <- c("DayofMonth", "Arrival Delay Rate", "Departure Delay Rate")
first_month <- melt(first_month, id.vars = "DayofMonth")
str(first_month)


p3 <- ggplot(first_month, aes(x = DayofMonth, y = value, group = variable)) + 
  geom_line(aes(color = variable)) +
  labs(title = "Delay Rate in December" , x = "", y = "") + 
  scale_color_manual(values = plot_cols) +
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

day_aug <- years %>%
  select(Month, DayofMonth, DepTime, CRSDepTime, DepDelay, dep_delayed, arr_delayed) %>%
  filter(Month == 12, 
         DayofMonth == 22 |
         DayofMonth == 23) %>%
  drop_na(DepTime) %>%
  filter(nchar(DepTime) > 2)

day_aug1 <- day_aug %>%
  filter(nchar(DepTime) == 3) %>%
  mutate(DepTime = signif(DepTime, 1)) %>%
  mutate(DepTime = case_when(
    DepTime == 100 ~ "0100",
    DepTime == 200 ~ "0200",
    DepTime == 300 ~ "0300",
    DepTime == 400 ~ "0400",
    DepTime == 500 ~ "0500",
    DepTime == 600 ~ "0600",
    DepTime == 700 ~ "0700",
    DepTime == 800 ~ "0800",
    DepTime == 900 ~ "0900",
    TRUE ~ "1000"))


day_aug2 <- day_aug %>%
  filter(nchar(DepTime) == 4) %>%
  mutate(DepTime = signif(DepTime, 2)) %>%
  mutate(DepTime = case_when(
    DepTime == 2400 ~ "2400",
    DepTime > 2400 ~ paste0("0",as.character(DepTime - 2400)),
    TRUE ~ as.character(DepTime)))

day_aug <- rbind(day_aug1, day_aug2)
day_aug$DepTime <- factor(day_aug$DepTime)
summary(day_aug$DepTime)
day_delay <- day_aug %>% 
  select(DayofMonth, DepTime, dep_delayed, arr_delayed) %>%
  group_by(DepTime, DayofMonth) %>%
  summarise(num_arr = sum(arr_delayed == 'Yes'),
            num_flights = n(),
            arr_rate = num_arr / n(),
            num_dep = sum(dep_delayed == 'Yes'),
            num_flights = n(),
            dep_rate = num_dep / n())
day_delay$DepTime <- as.character(day_delay$DepTime)

day_delay <- day_delay[c(1,2,5,7)]
colnames(day_delay) <- c("DepTime", "DayofMonth", "Arrival Delay Rate", "Departure Delay Rate")
day_delay$DepTime <- factor(day_delay$DepTime)

day_delay <- melt(day_delay, id.vars = c("DepTime", "DayofMonth"))
str(day_delay)

p4 <- day_delay %>%
  filter(DayofMonth == 22) %>%
  arrange(DepTime) %>%
  ggplot(aes(x = DepTime, y = value, group = variable)) + 
  geom_line(aes(color = variable)) +
  labs(title = "Delay Rate on 22nd December" , x = "", y = "") + 
  scale_color_manual(values = plot_cols) +
  theme_bw() +
  theme(legend.title=element_blank()) +
  theme(axis.text.x = element_text(vjust = 0.5, hjust=1)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

plot_grid(p3, p4, ncol=1)

## Manually checking for cascading failures due to delays
check <- years %>%
  select(Year, Month, DayofMonth, CRSDepTime, DepTime, CRSArrTime, ArrTime, TailNum, FlightNum, Origin, Dest, arr_delayed, dep_delayed) %>%
  filter(Year == 2005,
         Month == 12,
         DayofMonth == 22,
         arr_delayed == "Yes",
         dep_delayed == "Yes",
         DepTime > 1100,
         DepTime < 1300)

## found 2 examples of 1st degree cascading failures due to delays
check <- check %>%
  filter(TailNum == "N957SW" |
         TailNum == "N835AE")
table(check)


## ST2195 COURSEWORK PROJECT QUESTION 5
## Date: 22 Mar 2022
## Author: celestlee

# Setting working directory 
setwd("~/Desktop/data_files/dataverse_files")


install.packages("ragg")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("cowplot")
install.packages("future")
install.packages("mlr3")
install.packages("mlr3verse")
install.packages("ranger")
library(ragg)
library(ggplot2)
library(tidyr)
library(dplyr)
library(cowplot)
library(future)
library(mlr3verse)
library(mlr3learners)
library(mlr3pipelines)
library(mlr3tuning)
library(mlr3viz)
library(paradox)
library(glmnet)
library(ranger)

future::plan()

airports <- read.csv("airports.csv", header = TRUE)
planes <- read.csv("plane-data.csv", header = TRUE, na.strings=c("","NA"))

# Loading files -- Years 2005 to 2007 and binding it into data frame "years"
years <- rbind(read.csv("2005.csv.bz2"), # takes a really long time to load!
               read.csv("2006.csv.bz2"),
               read.csv("2007.csv.bz2"))

# Checking missing values (missing values or empty values)
colSums(is.na(years)|years=='')

# Feature engineering -- create new feature for status of flights
years$status <- NA
years$status[years$Diverted == 1] <- "Diverted"
years$status[years$DepDelay > 0] <- "Delayed"
years$status[years$ArrDelay > 0] <- "Delayed"
years$status[years$Cancelled == 1] <- "Cancelled"
years$status <- ifelse(years$Diverted != 1 & years$DepDelay <= 0 & 
                         years$Cancelled != 1 & years$ArrDelay <= 0, "On Time", years$status)
years$status <- as.factor(years$status)
summary(years$status)

#### Exploratory data analysis (Against delay rate) ####

## CRSDepTime, CRSArrTime
summary(years$CRSDepTime) ## Based on data frame, timings with "0" refers to 24:00, or 00:00

main <- years %>% # Included a few rows for convenience -- For train & test set at later part
  select(Year, CRSDepTime, DepDelay, DepTime, ArrDelay, CarrierDelay, LateAircraftDelay,
         NASDelay, status, TailNum, Dest) %>%
  drop_na(DepDelay)  # NA values are because flight is cancelled

# Calculating percentage of abnormal CRSDepTime 
unusual_CRSDepTime <- main %>% 
  select(CRSDepTime) %>%
  summarise(num_2_digits = sum(nchar(CRSDepTime) == 2),
            num_all = n(),
            perc = num_2_digits / num_all * 100) ## 0.13%
# Since there is only a small % of "abnormal" (2 digit) records, I will remove these records 
## from the dataset.
summary(main) 
## single digit CRSDepTime represents the minutes in the hour 2400, eg CRSDepTime == 8 = 2408;
## same thing for DepTime.
## timings over 2400 indicates next day -- hence take Time - 2400
crs_dep1 <- main %>%
  filter(nchar(CRSDepTime) == 3) %>%
  filter(nchar(DepTime) == 3) %>%
  mutate(CRSDepTime = signif(CRSDepTime, 1)) %>%
  mutate(CRSDepTime = case_when(
    CRSDepTime == 100 ~ "0100",
    CRSDepTime == 200 ~ "0200",
    CRSDepTime == 300 ~ "0300",
    CRSDepTime == 400 ~ "0400",
    CRSDepTime == 500 ~ "0500",
    CRSDepTime == 600 ~ "0600",
    CRSDepTime == 700 ~ "0700",
    CRSDepTime == 800 ~ "0800",
    CRSDepTime == 900 ~ "0900",
    TRUE ~ "1000")) %>%
  mutate(DepTime = signif(DepTime, 1)) %>%
  mutate(DepTime = case_when(
    DepTime == 100 ~ "0100",
    DepTime == 200 ~ "0200",
    DepTime == 300 ~ "0300",
    DepTime == 400 ~ "0400",
    DepTime == 500 ~ "0500",
    DepTime == 600 ~ "0600",
    DepTime == 700 ~ "0700",
    DepTime == 800 ~ "0800",
    DepTime == 900 ~ "0900",
    TRUE ~ "1000"))
crs_dep1$CRSDepTime <- factor(crs_dep1$CRSDepTime)
crs_dep1$DepTime <- factor(crs_dep1$DepTime)

crs_dep2 <- main %>%
  filter(nchar(CRSDepTime) == 4) %>%
  mutate(CRSDepTime = signif(CRSDepTime, 2)) %>%
  filter(nchar(DepTime) == 4) %>%
  mutate(DepTime = signif(DepTime, 2)) %>%
  mutate(DepTime = case_when(
  DepTime > 2400 ~ paste0("0",as.character(DepTime - 2400)),
  TRUE ~ as.character(DepTime)))

crs_dep2$CRSDepTime <- factor(crs_dep2$CRSDepTime)
crs_dep2$DepTime <- factor(crs_dep2$DepTime)

crs_dep3 <- main %>%
  filter(nchar(CRSDepTime) == 1 |
           nchar(CRSDepTime) == 2) %>%
  mutate(CRSDepTime = case_when(
    nchar(CRSDepTime) == 1 ~ "2400",
    nchar(CRSDepTime) == 2 ~ "2400",
    TRUE ~ as.character(CRSDepTime))) %>%
  filter(nchar(DepTime) == 1 |
           nchar(DepTime) == 2) %>%
  mutate(DepTime = case_when(
    nchar(DepTime) == 1 ~ "2400",
    nchar(DepTime) == 2 ~ "2400",
    TRUE ~ as.character(DepTime)))

crs_dep3$CRSDepTime <- factor(crs_dep3$CRSDepTime)
crs_dep3$DepTime <- factor(crs_dep3$DepTime)

crs_dep <- rbind(crs_dep1, crs_dep2, crs_dep3)
summary(crs_dep)
rm(crs_dep1, crs_dep2, crs_dep3)

### plot of CRSDepTime and DepTime against mean delay rates ###
crs_deptime <- crs_dep %>% 
  select(CRSDepTime, DepDelay) %>%
  group_by(CRSDepTime) %>%
  summarise(mean_delay = mean(DepDelay, na.rm = TRUE)) %>%
  arrange(CRSDepTime)

deptime_delay <- crs_dep %>% 
  select(DepTime, DepDelay) %>%
  group_by(DepTime) %>%
  summarise(mean_delay = mean(DepDelay, na.rm = TRUE)) %>%
  arrange(DepTime)

p1 <- ggplot(crs_deptime, aes(x = CRSDepTime, y = mean_delay, group = 1)) + 
  geom_line(color = "steelblue", size = 0.7) +
  geom_point(color = "steelblue") +
  labs(title = "Scheduled Departure Time" , x = "", y = "") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 

p2 <- ggplot(deptime_delay, aes(x = DepTime, y = mean_delay, group = 1)) + 
  geom_line(color = "steelblue", size = 0.7) +
  geom_point(color = "steelblue") +
  labs(title = "Actual Departure Time" , x = "", y = "") + 
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12),
        axis.title.x = element_blank()) 

title <- ggdraw() + draw_label("Mean Departure Delay (minutes) based on",
                               fontface='bold', size = 17)
p <- plot_grid(p1,p2)
plot_grid(title, p, ncol=1, rel_heights=c(0.1, 1))

## Age of planes (See question 2 plot)
### Delay rates increases and peaks when a plane is at 25 years of service, and 
### decreases thereafter.

### Scatterplot of ArrDelay against DepDelay ###
arr_dep_delay <- main[c(1:100000),]
arr_dep_delay <- arr_dep_delay %>%
  drop_na(ArrDelay)

p3 <- ggplot(arr_dep_delay, aes(x=DepDelay, y=ArrDelay)) + 
  geom_point(alpha = 0.2, size = 0.3) +
  labs(title = "Scatter Plot of Arrival Delay against Departure Delay (minutes)",
       x = "Departure Delay", y = "Arrival Delay") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(plot.title = element_text(face = "bold", hjust=0.5, vjust = 1, size = 12))  


## Line chart of iata against Depdelay
airport_all <- main %>%
  select(Dest, status)
colnames(airport_all)[1] <- "iata"

airport_delay <- airport_all %>% 
  group_by(iata) %>%
  summarise(num_delays = sum(status == 'Delayed'),
            num_flights = n(),
            delay_rate = num_delays / n())

p4 <- airport_delay %>%
  ggplot(aes(x = iata, y = delay_rate, group = 1)) + 
  geom_line(color = "steelblue", size = 0.7) + 
  geom_hline(aes(yintercept = mean(delay_rate), color = "Mean Delay Rate"), linetype = "dotted") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Departure Delay Rates based on Airport", x = "All Airports", y = "Delay Rate") + 
  theme_bw() +
  theme(legend.title=element_blank()) + 
  theme(axis.text.x = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(hjust = 0.5,size = 13, face = "bold"))

plot_grid(p3,p4, align = 'h')

## Conclusion:
## CRSDepTime, DepTime, ArrTime, Age of Plane, Carrier Delay, NASDelay, Late Aircraft Delay and 
## Airport are variables that will be included.
## Plane manufacturer can't be used as it contains too many NA's, imputing large proportion of 
## dataset will cause inaccuracy.

# Splitting the dataset into the Training set and Test set

original_plane <- planes %>%
  select(tailnum, year)
  
original <- crs_dep 
colnames(original)[10] <- "tailnum"
colnames(original)[11] <- "iata"

original <- original %>%
  inner_join(original_plane) ## joining by tailnum
str(original)
original$year <- as.numeric(original$year)

summary(original) # year(plane) has missing values
# impute mean
original$year[is.na(original$year)] = median(original$year, na.rm=TRUE)
# remove NA's for ArrDelay as ArrDelay = NA indicates no ArrDelay

original <- original %>%
  drop_na(ArrDelay) %>%
  mutate(age = Year - year) %>%
  filter(age > 0) # there will be several negative values, also a small proportion, cleaning up data

# encoding variables
encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order, exclude = NULL))
  x
}
original$iata <- factor(original$iata)
original$iata_encoded <- encode_ordinal(original[["iata"]])
original$CRSDepTime_encoded <- encode_ordinal(original[["CRSDepTime"]])
original$DepTime_encoded <- encode_ordinal(original[["DepTime"]])

# checking for missing values
colSums(is.na(original)|original=='')
summary(original)
str(original) 

# subsetting to columns needed
original <- original[c(3,5:8,13:16)]
str(original)

## Predicting departure delay
task_original <- TaskRegr$new(original, id = "Delay", target = "DepDelay")
print(task_original)

task_original$feature_names
task_original$target_names

task_original$filter(rows = 1:1000000)
task_original$select(setdiff(task_original$feature_names, "DepDelay"))

## checking significance of variables
model = lm(DepDelay ~., data = original) 
summary(model) # Adjusted R-squared:  0.8886 

measure <- msr("regr.mse")

learner_lm <- lrn("regr.lm")
gr_lm <- po("imputemean") %>>%
  po(learner_lm)
glrn_lm <-  GraphLearner$new(gr_lm)

set.seed(1)
train_set <- sample(task_original$nrow, 0.7 * task_original$nrow)
test_set <- setdiff(seq_len(task_original$nrow), train_set)
glrn_lm$train(task_original, row_ids = train_set)
glrn_lm$predict(task_original, row_ids = test_set)$score()
### regr.mse = 68.4995

learner_ridge <- lrn("regr.glmnet")
learner_ridge$param_set$values <- list(alpha = 0, lambda = 0.001)
gr_ridge <- po("scale") %>>%
  po("imputemean") %>>%
  po(learner_ridge)
glrn_ridge <- GraphLearner$new(gr_ridge)
glrn_ridge$train(task_original, row_ids = train_set)
glrn_ridge$predict(task_original, row_ids = test_set)$score()
### regr.mse = 68.50137

learner_ridge2 <- lrn("regr.glmnet")
learner_ridge2$param_set$values <- list(alpha = 0)
gr_ridge2 <- po("scale") %>>%
  po("imputemean") %>>%
  po(learner_ridge2)
glrn_ridge2 <- GraphLearner$new(gr_ridge2)

tune_lambda <- ParamSet$new (list(
  ParamDbl$new("regr.glmnet.lambda", lower = 0.03, upper = 2)
))
tuner <- tnr("grid_search")
terminator <- trm("evals", n_evals = 20)

at_ridge <- AutoTuner$new(
  learner = glrn_ridge2,
  resampling = rsmp("cv", folds = 3),
  measure = measure,
  search_space = tune_lambda,
  terminator = terminator,
  tuner = tuner
)

at_ridge$train(task_original, row_ids = train_set)
at_ridge$predict(task_original, row_ids = test_set)$score()
### regr.mse = 68.49699

##### random forests ####

learner_rf <- lrn('regr.ranger') 
learner_rf$param_set$values <- list(min.node.size = 4)
gr_rf <- po('scale') %>>%
  po('imputemean') %>>%
  po(learner_rf)
glrn_rf <- GraphLearner$new(gr_rf)
tune_ntrees <- ParamSet$new (list(
  ParamInt$new('regr.ranger.num.trees', lower = 50, upper = 600)))

at_rf <- AutoTuner$new(
  learner = glrn_rf,
  resampling = rsmp('cv', folds = 3),
  measure = measure,
  search_space = tune_ntrees,
  terminator = terminator,
  tuner = tuner)

at_rf$train(task_original, row_ids = train_set)
at_rf$predict(task_original, row_ids = test_set)$score()
### regr.mse = 33.12802


#### Benchmarking ####
set.seed(123)

# list of learners
lrn_list <- list(
  glrn_lm,
  glrn_ridge,
  at_ridge,
  at_rf
)

# set the benchmark design and run the comparisons
bm_design <- benchmark_grid(task = task_original, resamplings = rsmp('cv', folds = 3), 
                            learners = lrn_list)
bmr <- benchmark(bm_design, store_models = TRUE)

autoplot(bmr) + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


bmr$aggregate(measure)
#nr      resample_result task_id
#1:  1 <ResampleResult[22]>   Delay
#2:  2 <ResampleResult[22]>   Delay
#3:  3 <ResampleResult[22]>   Delay
#4:  4 <ResampleResult[22]>   Delay
#learner_id resampling_id iters
#1:                 imputemean.regr.lm            cv     3
#2:       scale.imputemean.regr.glmnet            cv     3
#3: scale.imputemean.regr.glmnet.tuned            cv     3
#4: scale.imputemean.regr.ranger.tuned            cv     3
#regr.mse
#1: 68.32703
#2: 68.32712
#3: 68.32669
#4: 31.95522


### Random forest performed the best over the other regression models
### with MSE of 31.955






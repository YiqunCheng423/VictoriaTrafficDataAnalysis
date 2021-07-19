library(tidyverse)
library(lubridate)
library(wordcloud2)
library(leaflet)
library(readxl)
library(treemap)

# 1
accident_raw_data <- read.csv('ACCIDENT.csv')
head(accident_raw_data)
accident_raw_data$ACCIDENTDATE <- as.Date(accident_raw_data$ACCIDENTDATE, format="%d/%m/%Y")
accident_raw_data <- accident_raw_data %>% 
  mutate(date = ymd(accident_raw_data$ACCIDENTDATE)) %>% 
  mutate_at(vars(date), funs(year, month, day))
head(accident_raw_data)

names(accident_raw_data)[names(accident_raw_data) == 'ACCIDENTDATE'] <- 'ACCIDENT_DATE'
names(accident_raw_data)[names(accident_raw_data) == 'year'] <- 'YEAR'
names(accident_raw_data)[names(accident_raw_data) == 'month'] <- 'MONTH'
names(accident_raw_data)[names(accident_raw_data) == 'day'] <- 'DAY'
names(accident_raw_data)[names(accident_raw_data) == 'ACCIDENTTIME'] <- 'ACCIDENT_TIME'
names(accident_raw_data)[names(accident_raw_data) == 'Day.Week.Description'] <- 'DAY_WEEK_DESCRIPTION'
names(accident_raw_data)[names(accident_raw_data) == 'Light.Condition.Desc'] <- 'LIGHT_CONDITION_DESCRIPTION'


##
accident_date_time <- accident_raw_data[c('ACCIDENT_NO', 'ACCIDENT_DATE', 'YEAR', 'MONTH', 'DAY',
                                          'ACCIDENT_TIME', 'DAY_OF_WEEK', 'DAY_WEEK_DESCRIPTION')]
head(accident_date_time)

## checking
accident_date_time %>% 
  summarise(missing_value_ACCIDENT_NO = sum(is.na(ACCIDENT_NO)),
            missing_value_ACCIDENT_DATE = sum(is.na(ACCIDENT_DATE)), 
            missing_value_YEAR = sum(is.na(YEAR)), 
            missing_value_MONTH = sum(is.na(MONTH)),
            missing_value_DAY = sum(is.na(DAY)),
            missing_value_ACCIDENT_TIME = sum(is.na(ACCIDENT_TIME)),
            missing_value_DAY_WEEK_DESCRIPTION = sum(is.na(DAY_WEEK_DESCRIPTION)))

## wrangling
accident_year_month_count <- accident_date_time %>% 
  group_by(YEAR) %>% 
  summarise(COUNT = n())
accident_year_month_count$YEAR <- as.character(accident_year_month_count$YEAR)
head(accident_year_month_count)

accident_time <- accident_date_time[c('ACCIDENT_NO', 'ACCIDENT_TIME', 'DAY_WEEK_DESCRIPTION')]
head(accident_time)
accident_time$TIME_RANGE <- "3 to 5"
for (row in 1:nrow(accident_time)) 
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('00', '01', '02'))
  {
    accident_time[row,'TIME_RANGE'] <- "0 to 2"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('03', '04', '05'))
  {
    accident_time[row,'TIME_RANGE'] <- "3 to 5"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('06', '07', '08'))
  {
    accident_time[row,'TIME_RANGE'] <- "6 to 8"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('09', '10', '11'))
  {
    accident_time[row,'TIME_RANGE'] <- "9 to 11"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('12', '13', '14'))
  {
    accident_time[row,'TIME_RANGE'] <- "12 to 14"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('15', '16', '17'))
  {
    accident_time[row,'TIME_RANGE'] <- "15 to 17"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('18', '19', '20'))
  {
    accident_time[row,'TIME_RANGE'] <- "18 to 20"
  }
}
for (row in 1:nrow(accident_time))
{
  if (substring(accident_time[row, 'ACCIDENT_TIME'], 1, 2) %in% c('21', '22', '23'))
  {
    accident_time[row,'TIME_RANGE'] <- "21 to 23"
  }
}

accident_time_count <- accident_time %>% 
  group_by(TIME_RANGE) %>% 
  summarise(COUNT = n())

## graph
ggplot(accident_time_count, aes(x="", y=COUNT, fill=TIME_RANGE)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0) + 
  theme_minimal() +
  theme_void()

# accident
accident <- accident_raw_data[c('ACCIDENT_NO', 'ACCIDENT_DATE', 'YEAR', 'MONTH', 'DAY',
                                          'DAY_WEEK_DESCRIPTION', 'ACCIDENT_TIME', 'LIGHT_CONDITION', 
                                          'LIGHT_CONDITION_DESCRIPTION', 'NODE_ID', 'SPEED_ZONE', 'SEVERITY')]
head(accident)

## wrangling
location_raw_data <- read.csv('NODE.csv')
head(location_raw_data)
names(location_raw_data)[names(location_raw_data) == 'Lat'] <- 'LAT'
names(location_raw_data)[names(location_raw_data) == 'Long'] <- 'LONG'

location <- location_raw_data[c('ACCIDENT_NO', 'NODE_ID', 'LGA_NAME', 'DEG_URBAN_NAME', 'LAT', 'LONG', 'POSTCODE_NO')]
head(location)

# write.csv(location,'LOCATION.csv')

location %>% 
  group_by(DEG_URBAN_NAME)

location_count <- location %>% 
  group_by(LAT, LONG) %>% 
  summarise(COUNT = n()) %>% 
  arrange(COUNT)


## location
leaflet(location_count) %>% 
  setView(lng = 144.9806, lat = -37.8126, zoom = 6) %>% 
  addTiles() %>% 
  addCircleMarkers(location_count$LONG, 
                   ~location_count$LAT, 
                   radius = location_count$COUNT / 10, 
                   stroke = FALSE, 
                   color = 'grey',
                   fillOpacity = 0.5) 


# location count 2
## wrangling 
location_year <- merge(location, accident_date_time[c('ACCIDENT_NO', 'YEAR')], by.x = 'ACCIDENT_NO')
head(location_year)
location_year %>% 
  summarise(missing_value_year = sum(is.na(YEAR)))

## GRAPH
location_year_count <- location_year %>% 
  group_by(YEAR, DEG_URBAN_NAME) %>% 
  summarise(COUNT = n())

# Create the barplot
location_year_count %>% 
  ggplot(aes(x = YEAR, y = COUNT, fill = DEG_URBAN_NAME)) +
  geom_bar(stat="identity") + 
  scale_fill_brewer(palette="Paired") + 
  theme_minimal() + 
  labs(title = 'Total number of accidents from 2006 to 2020 in each area of Victoria', 
       x = 'Year', y = 'Total number of accidents')

# person
person_raw_data <- read.csv('PERSON.csv')
head(person_raw_data)
names(person_raw_data)[names(person_raw_data) == 'Age.Group'] <- 'AGE_GROUP'
names(person_raw_data)[names(person_raw_data) == 'Road.User.Type.Desc'] <- 'Road_User_Type_Desc'

person <- person_raw_data[c('ACCIDENT_NO', 'SEX', 'AGE', 'AGE_GROUP', 'Road_User_Type_Desc')]
person <- person %>% 
  filter(Road_User_Type_Desc == 'Drivers') %>%
  select('ACCIDENT_NO', 'SEX', 'AGE', 'AGE_GROUP')
head(person)

person_accident <- merge(person, accident[c('ACCIDENT_NO', 'YEAR', 'MONTH')], by.x = 'ACCIDENT_NO') %>% 
  filter(YEAR %in% (2006 : 2016))

## checking
person_accident %>% 
  summarise(missing_value_sex = sum(is.na(SEX)), missing_value_age = sum(is.na(AGE)))
person_accident <- person_accident %>% 
  na.omit

boxplot(person_accident$AGE, main = 'Age')
boxplot.stats(person_accident$AGE)$out

## wrangling
person_count_year <- person_accident %>% 
  filter(AGE %in% (16:90) & SEX != 'U') %>% 
  group_by(SEX, AGE_GROUP, YEAR) %>% 
  summarise(TOTAL_NUMBER_OF_PERSON = n()) %>% 
  arrange(YEAR)
head(person_count_year)
person_count_month <- person_accident %>% 
  filter(AGE %in% (16:90) & SEX != 'U') %>% 
  group_by(SEX, AGE_GROUP, MONTH) %>% 
  summarise(TOTAL_NUMBER_OF_PERSON = n()) %>% 
  arrange(MONTH)
head(person_count_month)

person_count_year$YEAR <- as.character(person_count_year$YEAR)
person_count_month$MONTH <- as.character(person_count_month$MONTH)

## bar ?????
person_count_month %>% 
  ggplot(aes(x = MONTH, y = TOTAL_NUMBER_OF_PERSON, fill = AGE_GROUP)) + 
  geom_bar(stat = 'identity') + 
  facet_grid(SEX ~ AGE_GROUP) + 
  labs(title = 'Total number of person in each month')

## heat map
ggplot(data = person_count_year, mapping = aes(x = YEAR, y = AGE_GROUP, fill = TOTAL_NUMBER_OF_PERSON)) +
  geom_tile() +
  xlab(label = "Year") + 
  ylab('Age Group') + 
  facet_grid(~ SEX)

# atmosphere
atmosphere_raw_data <- read.csv('ATMOSPHERIC_COND.csv')
head(atmosphere_raw_data)
names(atmosphere_raw_data)[names(atmosphere_raw_data) == 'Atmosph.Cond.Desc'] <- 'ATMOSPHERE'

atmosphere <- atmosphere_raw_data[c('ACCIDENT_NO', 'ATMOSPH_COND', 'ATMOSPHERE')]
head(atmosphere)

# road
road_raw_data <- read.csv('ROAD_SURFACE_COND.csv')
head(road_raw_data)
names(road_raw_data)[names(road_raw_data) == 'Surface.Cond.Desc'] <- 'ROAD_SURFACE_CONDITION'

road <- road_raw_data[c('ACCIDENT_NO', 'SURFACE_COND', 'ROAD_SURFACE_CONDITION')]
head(road)

# accident_atmosphere_road
accident_atmosphere <- merge(accident[c('ACCIDENT_NO', 'YEAR','LIGHT_CONDITION', 
                                        'LIGHT_CONDITION_DESCRIPTION', 'SPEED_ZONE', 'SEVERITY')], 
                             atmosphere, by.x = 'ACCIDENT_NO')
accident_atmosphere_road <- merge(accident_atmosphere, road, by.x = 'ACCIDENT_NO')
head(accident_atmosphere_road)

# checking
accident_atmosphere_road %>% 
  summarise(missing_value_speed_zone = sum(is.na(SPEED_ZONE)), 
            missing_value_light = sum(is.na(ATMOSPH_COND)),
            missing_value_surface = sum(is.na(SURFACE_COND)),)

boxplot(accident_atmosphere_road$SPEED_ZONE)
boxplot.stats(accident_atmosphere_road$SPEED_ZONE)$out
boxplot(accident_atmosphere_road$LIGHT_CONDITION)
boxplot(accident_atmosphere_road$ATMOSPH_COND)
boxplot(accident_atmosphere_road$SURFACE_COND)

accident_atmosphere_road <- accident_atmosphere_road %>% 
  filter(SPEED_ZONE < 200)

accident_speed_count <- accident_atmosphere_road %>% 
  group_by(SPEED_ZONE) %>%
  summarise(TOTAL_NUMBER_OF_ACCIDENTS = n()) %>% 
  arrange(SPEED_ZONE) 

accident_speed_count$SPEED_ZONE <- as.character(accident_speed_count$SPEED_ZONE)

##WordCloud

wordcloud2(accident_speed_count, size = 1.6)

## wrangling
accident_light_count <- accident_atmosphere_road %>% 
  filter(YEAR == 2016) %>% 
  group_by(LIGHT_CONDITION) %>% 
  summarise(COUNT = n()) 
accident_light_count$LIGHT_CONDITION <- as.character(accident_light_count$LIGHT_CONDITION)

accident_atmosphere_count <- accident_atmosphere_road %>% 
  filter(YEAR == 2016) %>% 
  group_by(ATMOSPH_COND) %>% 
  summarise(COUNT = n())
accident_atmosphere_count$ATMOSPH_COND <- as.character(accident_atmosphere_count$ATMOSPH_COND)

accident_surface_count <- accident_atmosphere_road %>% 
  filter(YEAR == 2016) %>% 
  group_by(SURFACE_COND) %>% 
  summarise(COUNT = n())
accident_surface_count$SURFACE_COND <- as.character(accident_surface_count$SURFACE_COND)

## cleaning

## graph
ggplot(accident_light_count, aes(x = LIGHT_CONDITION, y = COUNT)) +
  geom_segment(aes(x = LIGHT_CONDITION, xend = LIGHT_CONDITION, y = 0, yend = COUNT)) +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  ylab('Total number of accidents') + 
  xlab('Light condition')

ggplot(accident_atmosphere_count, aes(x = ATMOSPH_COND, y = COUNT)) +
  geom_segment(aes(x = ATMOSPH_COND, xend = ATMOSPH_COND, y = 0, yend = COUNT)) +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) + 
  ylab('Total number of accidents') + 
  xlab('Atmosphere condition')

ggplot(accident_surface_count, aes(x = SURFACE_COND, y = COUNT)) +
  geom_segment(aes(x = SURFACE_COND, xend = SURFACE_COND, y = 0, yend = COUNT)) +
  geom_point(size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2)  + 
  ylab('Total number of accidents') + 
  xlab('Surface condition')

## wrangling
# graph
racv_raw_data <- read_excel('RACV Young Adult Licensing Trends 2017.xlsx')
names(racv_raw_data)[1] <- "AGE_GROUP"
head(racv_raw_data)

racv <- racv_raw_data %>%
  gather(key = YEAR, value = PERCENT, -AGE_GROUP)
racv$YEAR <- as.integer(racv$YEAR)
racv <- racv %>% 
  filter(YEAR %in% 2016)
head(racv)

racv %>% 
  ggplot(aes(x = AGE_GROUP, y = PERCENT)) +
  geom_bar(stat="identity", fill = "#FF6666") + 
  labs(x = 'Age group', y = 'Driving license holding rate(%)')
  

## wrangling
person_accident_atmosphere_road <- merge(person_accident, 
                                         accident_atmosphere_road[c('ACCIDENT_NO', 
                                                                    'LIGHT_CONDITION', 
                                                                    'ATMOSPH_COND', 
                                                                    'SURFACE_COND')], 
                       by.x = 'ACCIDENT_NO')

person_total <- person_accident_atmosphere_road %>% 
  select(ACCIDENT_NO, SEX, AGE, YEAR, MONTH, LIGHT_CONDITION, ATMOSPH_COND, SURFACE_COND) %>% 
  mutate(
  AGE_GROUP = case_when(
    AGE >= 18 & AGE <= 24 ~ "Age 18-24",
    AGE >= 25 & AGE <= 29 ~ "Age 25-29",
    AGE >= 30 & AGE <= 34 ~ "Age 30-34",
    AGE >= 35 & AGE <= 39 ~ "Age 35-39",
    AGE >= 40 & AGE <= 44 ~ "Age 40-44",
    AGE >= 45 & AGE <= 49 ~ "Age 45-49",
    AGE >= 50 & AGE <= 54 ~ "Age 50-54",
    AGE >= 55 & AGE <= 59 ~ "Age 55-59",
    AGE >= 60 & AGE <= 64 ~ "Age 60-64",
    AGE >= 65 & AGE <= 69 ~ "Age 65-69",
    AGE >= 70 & AGE <= 90 ~ "Age 70-90",
  ))
head(person_total)

# checking
person_total %>% 
  summarise(Total_missing = sum(is.na(AGE_GROUP)))
person_total <- person_total %>% 
  na.omit 

person_total_count <- person_total %>% 
  group_by(AGE) %>% 
  summarise(COUNT = n())

## 
person_total_count %>% 
  ggplot(aes(x = AGE, y = COUNT)) + 
  geom_point() + 
  geom_smooth(method = 'lm') +
  ylab("Total number of person") + 
  xlab('Age')

##
person_2016 <- person_total %>% 
  filter(YEAR == 2016)

total_drivers <- nrow(person_2016)

person_2016_count <- person_2016 %>% 
  group_by(AGE_GROUP) %>% 
  summarise(COUNT = n())

person_2016_count$PERCENT <- round(person_2016_count$COUNT / total_drivers, 2)

treemap(person_2016_count,
        index="AGE_GROUP",
        vSize="COUNT",
        type="index", 
        title = "The percentage of each age group"
)


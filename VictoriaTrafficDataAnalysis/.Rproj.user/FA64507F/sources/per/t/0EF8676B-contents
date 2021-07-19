library(rgeos)
library(maptools)
library(leaflet)
library(here)
library(data.table)
library(lubridate)
library(openxlsx)

## read shp file

melb_mapdat <- readShapeSpatial(here::here("./VIC_LGA_POLYGON_SHP_GDA2020.shp")) # https://data.gov.au/dataset/ds-dga-bdf92691-c6fe-42b9-a0e2-a4cd716fa811/details?q=
# melb_mapdat <- readShapeSpatial(here::here("./Plan-Melbourne-Shape-Files/Administrative/Metro Partnership Boundaries_region.shp")) # https://www.planmelbourne.vic.gov.au/maps/spatial-data
plot(melb_mapdat)

## accident data
location_raw_data <- fread('NODE.csv')
head(location_raw_data)
names(location_raw_data)[names(location_raw_data) == 'Lat'] <- 'LAT'
names(location_raw_data)[names(location_raw_data) == 'Long'] <- 'LONG'
location_raw_data <- location_raw_data[, c('ACCIDENT_NO', 'NODE_ID', 'LGA_NAME', 'DEG_URBAN_NAME', 'LAT', 'LONG', 'POSTCODE_NO')]
location_raw_data[, LGA_NAME := NA]

## accidents
accident_raw_data <- fread('ACCIDENT.csv')
accident_raw_data <- unique(accident_raw_data, by = "ACCIDENT_NO")

# person
person_raw_data <- fread('PERSON.csv')
person_raw_data <- unique(person_raw_data, by = "ACCIDENT_NO")
person_raw_data[,
  AGE_GROUP := fcase(
    AGE >= 18 & AGE <= 24, "Age 18-24",
    AGE >= 25 & AGE <= 29, "Age 25-29",
    AGE >= 30 & AGE <= 34, "Age 30-34",
    AGE >= 35 & AGE <= 39, "Age 35-39",
    AGE >= 40 & AGE <= 44, "Age 40-44",
    AGE >= 45 & AGE <= 49, "Age 45-49",
    AGE >= 50 & AGE <= 54, "Age 50-54",
    AGE >= 55 & AGE <= 59, "Age 55-59",
    AGE >= 60 & AGE <= 64, "Age 60-64",
    AGE >= 65 & AGE <= 69, "Age 65-69",
    AGE >= 70 & AGE <= 90, "Age 70-90"
  )
]

# merge
clean_data <- merge(location_raw_data, person_raw_data, by = "ACCIDENT_NO", all.x = TRUE)
clean_data <- merge(clean_data, accident_raw_data[, -"NODE_ID"], by = "ACCIDENT_NO", all.x = TRUE)

# clean
clean_data[, ACCIDENTDATE := dmy(ACCIDENTDATE)]
clean_data[, year_ := year(ACCIDENTDATE)]

# claculate area
LGA_NAMEs <- as.character(melb_mapdat@data$LGA_NAME)
for (ii in seq(length(LGA_NAMEs))) {
  message(ii)
  logic_v <- point.in.polygon(clean_data$LONG, clean_data$LAT, melb_mapdat@polygons[[ii]]@Polygons[[1]]@coords[, 1], melb_mapdat@polygons[[ii]]@Polygons[[1]]@coords[, 2])
  clean_data[, LGA_NAME := ifelse(logic_v == 1, LGA_NAMEs[ii], LGA_NAME)]
}

# merge accidents number & clean
region_counts <- clean_data[, .N, by = .(LGA_NAME)][!is.na(LGA_NAME)]
melb_mapdat_copy <- copy(melb_mapdat)
setDT(melb_mapdat_copy@data)
melb_mapdat_copy@data[, "index"] <- seq(melb_mapdat_copy@data[, .N])
tmpdat <- merge(melb_mapdat_copy@data, region_counts, all.x = TRUE, by = "LGA_NAME")
setorder(tmpdat, index)
tmpdat[, seq_ := rowid(LGA_NAME)]
tmpdat[seq_ >= 2, LGA_NAME := paste0(LGA_NAME, seq_)]
melb_mapdat_copy@data <- tmpdat


######################## for shiny
# group counts
age_loc_counts   <- clean_data[, .N, by = .(`LGA_NAME`, `Age Group`)]
age_year_counts  <- clean_data[, .N, by = .(`year_`, `AGE`, `SEX`)]
age_loc2_counts  <- clean_data[, .N, by = .(`DEG_URBAN_NAME`, `AGE`)]
year_date_counts <- clean_data[, .N, by = .(`year_`, `ACCIDENTDATE`)]
inj_loc2_counts  <- clean_data[, .N, by = .(`DEG_URBAN_NAME`, `Inj Level Desc`)]

saveRDS(age_loc_counts  , "age_loc_counts.rds")
saveRDS(age_year_counts , "age_year_counts.rds")
saveRDS(age_loc2_counts , "age_loc2_counts.rds")
saveRDS(year_date_counts, "year_date_counts.rds")
saveRDS(inj_loc2_counts , "inj_loc2_counts.rds")
saveRDS(clean_data , "clean_data.rds")

saveRDS(melb_mapdat_copy , "melb_mapdat_copy.rds")






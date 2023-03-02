#Гарсия Формеса Хорхе  – регион Вурятия
#урожайность пшеницы в период с 1999 по 2003 год взяв для рассчета средние суммы активных температур за эти годы
# 12 ближайших метеостанций но рассчитав колонку di самостоятельно, 
#как долю месяца, когда среднедневные температуры были выше 8 градусов, но учитывая.

rm(list=ls())

library(rnoaa)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(tidyverse)
library(rnoaa)
station_data = ghcnd_stations()
station_data = read.csv("station_data.csv")


Buriatia = data.frame(id = "BURIATIA", latitude = 51.834811, longitude = 107.584545)
Buriatia_around = meteo_nearby_stations(lat_lon_df = Buriatia, station_data = station_data,
                                           var = c("PRCP", "TAVG"),
                                           year_min = 1991, year_max = 2003)
Buriatia_table = Buriatia_around[[1]]

Buriatia_id1 = Vladivostok_around[["BURIATIA"]][["id"]][[1]]

str(Buriatia_around)
all_Buriatia_data = meteo_tidy_ghcnd(stationid = Buriatia_id1)

Buriatia_table = Buriatia_around[[1]]
summary(Buriatia_table)

all_i = data.frame()

all_Buriatia = data.frame()

for(i in 1:2) 
{all_i = meteo_tidy_ghcnd(stationid = Buriatia_around[["BURIATIA"]][["id"]][i])
all_i = all_i[ ,c("id","date","tavg")] 
print(all_i)
all_Buriatia=rbind(all_Buriatia, all_i)}

Buriatia_data = all_Buriatia %>% 
  mutate(date=ymd(date),
         year=year(date),
         month=month(date)) %>%
  mutate(tavg=case_when( tavg<50 ~ 0, TRUE ~ tavg)/10) %>% 
  filter (year>1999 & year < 2003) %>% 
  group_by(id,year,month) %>% 
  summarize(tsum = sum(tavg,na.rm=TRUE)) %>% 
  mutate(tsum=case_when(tsum==NA~0, TRUE ~ tsum)) %>% ungroup() %>% 
  group_by(month) %>% summarize(St = mean(tsum))

afi=c(0.00,0.00,0.00,32.11,26.31,25.64,32.20,18.73,
      16.30,13.83,0.00,0.00)
bfi=c(0.00,0.00,0.00,11.30,9.26,9.03,8.16,6.59,5.73,
      4.87,0.00,0.00)
di=c(0.00,0.00,0.00,0.33,1.00,1.00,1.00,0.32,0.00,
     0.00,0.00,0.00)
y=1.0
Kf=300
Qj=1600
Lj=2.2
Ej=25


Buriatia_data = Buriatia_data %>% 
  mutate(Fi=(afi)+(bfi)*y*(Buriatia_data$St))
Buriatia_data = Buriatia_data %>% mutate(Yj=(((Buriatia_data$Fi)*(di)*Kf)/(Qj*Lj*(100-Ej))))

YIELD=sum(Buriatia_data$Yj);YIELD

#Ответ: 17.8777 ц/га

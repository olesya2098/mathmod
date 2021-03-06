#������������ �.�. ��� 124
#��� ������� 27 ����������� ����������� ������� � 2012 ����, ���� ��� �������� ������� ����� �������� ���������� �� ���������� 9 ���, � 24 ��������� ������������
#27 ������ - ����������� ����
setwd("D:/Gaivoronskaya/mathmod1")
getwd()
#������������� ������
install.packages("tidyverse")
install.packages("rnoaa")
#��������� ������ ������
library("tidyverse")
library("rnoaa")
library("lubridate")
#��������� �������
station_data = ghcnd_stations()
write.csv(station_data,file="stations.csv")
station_data=read.csv("stations.csv")


#����� ��������� ������� ���� �������, �������� ������ ������� ��������� � ������� ������ �������,������ ������� � ������ ������� � ������������ ��� �������
Habarovsk = data.frame(id = "HABAROVSK", latitude = 48.28,  longitude = 135.4)
Habarovsk_around = meteo_nearby_stations(lat_lon_df = Habarovsk, station_data = station_data,
                                         limit = 24, var = c("PCR","TAVG"),
                                         year_min = 2003, year_max = 2012)

#Habarovsk_around ��� ������ ������������ ��������� �������� �������� �������, ���������� �������������� ������������ ��������������� �� �� 
# ������������ �� ���������, �������� ��� ������ ��������� ������� ����� ������������� ������������ ���������, ��� �� �� � ���������� ��������
Habarovsk_id = Habarovsk_around[["HABAROVSK"]][["id"]][1]
summary(Habarovsk_id)
# ��� ��������� ������� �� ����� �������������� ������ ��������� 
# ���������� ������� ������� ������ ������ �� ������
Habarovsk_table=Habarovsk_around[[1]]
summary(Habarovsk_table)
# � ������� Habarovsk_table ��������� 24 ��������, ������������� �� ���������� �� ���������
# ���������� ������ ����������� �������
# ������� �������������� �������������� ������������
Habarovsk_stations=Habarovsk_table
str(Habarovsk_stations)

# ������ �������� 24 ������������ ������������� ������ ����������
# ��� ��������� ������� �� ����� �������������� ������ 
# ������� �������������� �������������� ������������
Habarovsk_stations$id

# ����� �������� ��� ������ � 1 ������������ ���������� ������� meteo_tidy_ghcnd
all_Habarovsk_data=meteo_tidy_ghcnd(stationid = Habarovsk_id)
# ��������� ��� �� �������
summary(all_Habarovsk_data)


# ������� ����, � ������� �� ����������� ������ ������ ��� ���� ������������
# c������� ������, ���� ������� ��� ������ ���� ������������ (�����������)
all_Habarovsk_meteodata = data.frame()
# ������� ���� ��� ����� ������������
stations_names=Habarovsk_stations$id
stations_names=stations_names[1:24] 

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd( stationid = sname,
                              date_min = "2003-01-01",
                              date_max = "2012-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  
  
  
  
  one_meteo=one_meteo %>% mutate(tavg=(tmax+tmin)/2)}
one_meteo=one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)
all_Habarovsk_meteodata=rbind(all_Habarovsk_meteodata, one_meteo)}




# ���������� ���������� ����������
write.csv(all_Habarovsk_meteodata,"all_Habarovsk_meteodata.csv")
# ��������� ������ all_krasnodar_meteodata.csv
all_Habarovsk_meteodata=read.csv("all_Habarovsk_meteodata.csv")
# ������� ��� ���������� 
str(all_Habarovsk_meteodata)

# ������� ���, �����, ����
all_Habarovsk_meteodata=all_Habarovsk_meteodata %>% mutate(year=year(date), 
                                                           month=month(date), 
                                                           day=day(date))

# ��������� NA � 0 � ��� tavg<5
all_Habarovsk_meteodata[is.na(all_Habarovsk_meteodata$tavg),"tavg"] = 0
all_Habarovsk_meteodata[all_Habarovsk_meteodata$tavg<5, "tavg"] = 0
summary(all_Habarovsk_meteodata)





# ����������� ������������ �� id,������� � ����� � ������������ �������������
# �� ���� �������, ����� ����������� ������ �� ������� � ������ ������� �� �������
# ��� ���� ������������
group_meteodata =all_Habarovsk_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## ���������� � ������� �� ������� ������ ##
### ���� ��������
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# ��������� �� ����.1. ������� ������
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# ��������� �� ����. 1. ������� ������
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# ��������� ����� ���� i-�� ������,
#�������� � ������ ��������� ��������, � ������
#����� ����  � ������, ��������� �� ����.1
y=1.0
# ����������� ��� ���������� ������ - �������, ��� ��� ���� �������� ������
Kf=300
# ����������� ������������� ��� ������� 
Qj=1600
# ������������ ������ ��������
Lj=2.2
# ����� ������ �������� �������� ���������
Ej=25
# ����������� ��������� �������� 
# ���������� Fi �� �������
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#���������� Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  ����������� ������ 
Yield = (sum(sumT_month$Yi)) 
Yield 
# ��������� 16.05 �/��






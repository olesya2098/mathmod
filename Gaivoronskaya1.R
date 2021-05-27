#Гайворонская О.В. ПАЭ 124
#для региона 27 рассчитайте урожайность пшеницы в 2012 году, взяв для рассчета средние суммы активных температур за предыдущие 9 лет, с 24 ближайших метеостанций
#27 регион - Хабаровский край
setwd("D:/Gaivoronskaya/mathmod1")
getwd()
#устанавливаем пакеты
install.packages("tidyverse")
install.packages("rnoaa")
#открываем нужные пакеты
library("tidyverse")
library("rnoaa")
library("lubridate")
#скачиваем станции
station_data = ghcnd_stations()
write.csv(station_data,file="stations.csv")
station_data=read.csv("stations.csv")


#После получения всписка всех станций, получите список станций ближайших к столице вашего региона,создав таблицу с именем региона и координатами его столицы
Habarovsk = data.frame(id = "HABAROVSK", latitude = 48.28,  longitude = 135.4)
Habarovsk_around = meteo_nearby_stations(lat_lon_df = Habarovsk, station_data = station_data,
                                         limit = 24, var = c("PCR","TAVG"),
                                         year_min = 2003, year_max = 2012)

#Habarovsk_around это список единственным элементом которого является таблица, содержащая идентификаторы метеостанций отсортированных по их 
# удалленности от Хабаровск, очевидно что первым элементом таблицы будет идентификатор метеостанции Хабаровск, его то мы и попытаемся получить
Habarovsk_id = Habarovsk_around[["HABAROVSK"]][["id"]][1]
summary(Habarovsk_id)
# для получения таблицы со всеми метеостанциями вокруг Хабаровск 
# необходимо выбрать целиком первый объект из списка
Habarovsk_table=Habarovsk_around[[1]]
summary(Habarovsk_table)
# в таблице Habarovsk_table оказалось 24 объектов, ранжированных по расстоянию от Хабаровск
# сформируем список необходимых станций
# выведем индетификаторы отфильрованных метеостанций
Habarovsk_stations=Habarovsk_table
str(Habarovsk_stations)

# список содержит 24 метеостанции расположенных вблизи Хабаровска
# для получения таблицы со всеми метеостанциями вокруг 
# выведем индетификаторы отфильрованных метеостанций
Habarovsk_stations$id

# чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_Habarovsk_data=meteo_tidy_ghcnd(stationid = Habarovsk_id)
# посмотрим что мы скачали
summary(all_Habarovsk_data)


# создать цикл, в котором бы скачивались нужные данные для всех метеостанций
# cоздадим объект, куда скачаем все данные всех метеостанций (колличество)
all_Habarovsk_meteodata = data.frame()
# создаем цикл для наших метеостанций
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




# записываем полученные результаты
write.csv(all_Habarovsk_meteodata,"all_Habarovsk_meteodata.csv")
# считываем данные all_krasnodar_meteodata.csv
all_Habarovsk_meteodata=read.csv("all_Habarovsk_meteodata.csv")
# смотрим что получилось 
str(all_Habarovsk_meteodata)

# добавим год, месяц, день
all_Habarovsk_meteodata=all_Habarovsk_meteodata %>% mutate(year=year(date), 
                                                           month=month(date), 
                                                           day=day(date))

# превратим NA в 0 и где tavg<5
all_Habarovsk_meteodata[is.na(all_Habarovsk_meteodata$tavg),"tavg"] = 0
all_Habarovsk_meteodata[all_Habarovsk_meteodata$tavg<5, "tavg"] = 0
summary(all_Habarovsk_meteodata)





# сгруппируем метеостанции по id,месяцам и годам и проссумируем темперетатуру
# по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам
# для всех метеостанций
group_meteodata =all_Habarovsk_meteodata %>% group_by(id,year,month)
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))

## Подготовка к расчету по формуле Урожая ##
### Ввод констант
afi = c(0.000,0.000,0.000,32.110,26.310,25.640,23.200,18.730,16.300,13.830,0.000,0.000)
# константа по табл.1. Создаем вектор
bfi = c(0.000,0.000,0.000,11.300,9.260,9.030,8.160,6.590,5.730,4.870,0.000,0.000)
# константа по табл. 1. Создаем вектор
di = c(0.000,0.000,0.000,0.330,1.000,1.000,1.000,0.320,0.000,0.000,0.000,0.000)
# отношение числа дней i-го месяца,
#входящих в период вегетации культуры, к общему
#числу дней  в месяце, константа по табл.1
y=1.0
# Коэффициент для экспозиции склона - считаем, что все поля идеально ровные
Kf=300
# Коэффициент использования ФАР посевом 
Qj=1600
# калорийность урожая культуры
Lj=2.2
# сумма частей основной побочной продукции
Ej=25
# стандартная влажность культуры 
# Рассчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##  Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield 
# Результат 16.05 ц/га






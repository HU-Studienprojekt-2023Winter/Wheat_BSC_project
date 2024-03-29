install.packages('dplyr')
library(dplyr)
install.packages("tidyr")
library(tidyr)

# whatever ----------------------------------------------------------------


glimpse(df)
str(df)

data<-read.csv("./data/climate.csv")
data
glimpse(data)
str(data)
data$Termal_sum_<-rowSums(data[,c("DailyMean_Temperature")])
data$row_sum <- rowSums(data[ , c(1,3)], na.rm=TRUE)
glimpse(data)
Thermal_sum<-data%>%
  group_by("DFG_year","sowing_date")
Thermal_sum
Thermal_sum%>%mutate()
list(Thermal_sum)
###by_cyl %>% summarise(
##disp = mean(disp),
##hp = mean(hp)
##das verwenden aber mit mutate tauschen
data
glimpse("DFG_year")
data["DFG_year"&&"sowing_date",]

climate<-read.csv("./data/climate.csv")
climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  group_by(y,m) %>% 
  summarise()

climate %>% 
  dplyr::filter(DFG_year=="DFG2019") %>% 
  dplyr::select(y,m) %>% 
  dplyr::distinct()
ear_sum<-read.csv("./data/ear_summarized.csv")
ear_sum
glimpse(ear_sum)

#das unten verwenden bsp
ear_sum%>%
  group_by(nitrogen,appl,timeid) %>% 
  summarise()

# Week 7 Challanges -------------------------------------------------------
library(tidyr)
library(dplyr)
data<-read.csv("./data/climate.csv")
glimpse(data)
# ich brauche reihe : DFG_year, sowing_date, DayTime, DailyMean_Temperature
#x=days after sowing
climate<-data
climate
climate[c("DFG_year", "sowing_date", "DailyMean_Temperature","DayTime")]
glimpse(data)
install.packages("ggplot2")
library(ggplot2)

 new_climate<-data
 new_climate%>%group_by(sowing_date,DFG_year)%>%
   mutate(days_after_sowing=as.Date(DayTime,format("%Y-%m-%d")),
          days_after_sowing= days_after_sowing-min(days_after_sowing),
          day_after_sowing=as.numeric(days_after_sowing)
                       ) %>%
   ggplot(.)+
   #geom_boxplot()+
   geom_line(
     aes(x=days_after_sowing,y=Acc_Temperature, colour=DFG_year,
         group=interaction(sowing_date,DFG_year),
         linetype=sowing_date),
     linewidth=1)+
   
   
   #facet_grid(DFG_year~sowing_date)+
   theme_bw()+
   theme(legend.position = c(.15,.1),
         axis.text.x = element_text(angle = 90,vjust=0.2)
   )+
   xlab("Days after sowing")+
   ylab("Thermal time (C°d)")
 
   
# climate -----------------------------------------------------------------
climate %>% 
  ggplot(.)+
  #geom_boxplot()+
  geom_line(
    aes(x=DayTime,y=Acc_Temperature, colour=DFG_year,
               group=interaction(sowing_date,DFG_year),
        linetype=sowing_date),
    linewidth=0.5)+

 
  #facet_grid(DFG_year~sowing_date)+
  theme_bw()+
  theme(legend.position = c(.15,.1),
        axis.text.x = element_text(angle = 90,vjust=0.2)
        )+
  xlab("Days after sowing")+
  ylab("Thermal time (C°d)")
  
climate %>% 
  # dplyr::filter(DFG_year%in%c("DFG2019","DFG2020")) %>% 
  group_by(DFG_year,sowing_date) %>% 
  mutate(DayTime=as.Date(DayTime,format="%Y-%m-%d"),
         DAS=as.numeric(DayTime-min(DayTime))) %>% 
  ggplot(aes(x=DAS,y=Acc_Temperature,color=DFG_year,
             group=interaction(sowing_date,DFG_year)))+
  # geom_point()+
  geom_line(aes(linetype=sowing_date),linewidth=1)+
  theme_bw()+
  theme(legend.position = c(.1,.65))+
  labs(x="Days after sowing",y= "Thermal sum (°Cd)")+
  guides(color=guide_legend(title="Year"))

#entering from daejeon to sejong............................................................ 
#reading data
rse1 <- read.csv("D:/BILAL/sejong to daejeon OD/RSE수집이력_2020년9월21일_17시~20시.txt", header = TRUE)

#data has issue, so we read txt and then we have to split it into columns and give names
library(stringr)
library(dplyr)
data<- str_split_fixed(rse1$ID.CLCT_DT.OBU_IDNT_NMBR.CTYP.OBU_KIND, " ",5)
data<- as.data.frame(data)
names(data)<- c("id","date_time","car_num","ctyp","obukind")
y<- data[order(data$car_num,data$date_time),]

#getting O-D of each car
z3<- y %>% group_by(car_num) %>% slice(c(1,n()))
z4<- z3 %>% group_by(car_num) %>% slice(1)

#taking out 64 and 44 ids origin only and placing true for 64 column 
z5<- z4[z4$id %in% c("64","44","43"),]
z5$value<- "TRUE"
z5$from_64 <- ifelse(z5$id == 64, "TRUE", "FALSE")
z5$from_44 <- ifelse(z5$id == 44, "TRUE", "FALSE")
z5$from_43 <- ifelse(z5$id == 43, "TRUE", "FALSE")

#combine it with the original O-D file 
z6<- merge(x= z3,y= z5, by = "car_num", all.x = TRUE)
z6<- z6[,-c(6,7,8,9)]

#ordering by car num and time of each car num
z66<- z6[order(z6$car_num,z6$date_time.x),]

#taking out the O values traveling to 64 and 44 Destinations
z7<- subset(z66 , (value == "TRUE"))
z7<- subset(z7 , (id.x != 64))
z7<- subset(z7 , id.x != 44)
z7<- subset(z7 , id.x != 43)

# count_res_O<- count(z7, id.x)
# count_res_O<- count_res_O[order(count_res_O$n),]
# count_res_D<- count(z7, from_64)
# count_res_D<- count_res_D[order(count_res_D$n),]

#OD counts for 64
count_64_O<- subset(z7, z7$from_64==TRUE)
x<- table(count_64_O$id.x)
t<- as.data.frame(x)
names(t)<- c("to", "count_64")
#t$to<- 64
# write.csv(t, "D:/BILAL/sejong to daejeon OD/OD64_output.csv")

#OD counts for 44
count_44_O<- subset(z7, z7$from_44==TRUE)
xx<- table(count_44_O$id.x)
tt<- as.data.frame(xx)
names(tt)<- c("to", "count_44")
#tt$to <- 44 
# write.csv(tt, "D:/BILAL/sejong to daejeon OD/OD44_output.csv")

#OD counts for 43
count_43_O<- subset(z7, z7$from_43==TRUE)
xxx<- table(count_43_O$id.x)
ttt<- as.data.frame(xxx)
names(ttt)<- c("to", "count_43")
#tt$to <- 43 
# write.csv(ttt, "D:/BILAL/sejong to daejeon OD/OD43_output.csv")



#OD table for 64 and 44
ODtable_counts<- merge(x= t, y=tt, all.x = TRUE, all.y = TRUE, by = "to")
ODtable_counts<- merge(x= ODtable_counts, y=ttt, all.x = TRUE, all.y = TRUE, by = "to")


xxxx<- table(z7$id.x)
tttt<- as.data.frame(xxxx)
names(tttt)<- c("to", "total_count")
ODtable_counts<- merge(x= ODtable_counts, y=tttt, all.x = TRUE,by = "to")
# write.csv(ODtable_counts, "D:/BILAL/sejong to daejeon OD/OD_output.csv")


# install.packages("xlsx")...requires java so it is not working
# #install.packages("writexl")
# library(writexl)
# write.table(z7, "D:/BILAL/sejong to daejeon OD/SD_output.txt", sep="\t")
# write.csv(z7, "D:/BILAL/sejong to daejeon OD/SD_output.csv")
# write_xlsx(z7, "D:/BILAL/sejong to daejeon OD/SD_output.xlsx")
# write_xlsx(ODtable_counts, "D:/BILAL/sejong to daejeon OD/OD_counts.xlsx")


# #df[!duplicated(df$ID),]
# 
# x<- rse1[!duplicated(rse1$OBU_IDNT_NMBR),]
# 
# write.csv(x,"D:/BILAL/output.csv", row.names = TRUE)


#plotting the OD map
library(data.table)
library(ggplot2)
library(tidyverse1)
library(ggmap)
library(sf)
#install.packages("mapview")
library(mapview)
# install.packages("maps")
library(maps)
library(sf)
library(dplyr)
library(stringr)
library(sp)


data_2<- read.csv("D:/BILAL/sejong to daejeon OD/TB_RSE_MSTR_빌랄.csv")
data_2<- data_2[,c(1,6,7,8)]
colnames(data_2)[1]<- "to"
# data_2<- data_2[1:2000,]
data_2<- merge(ODtable_counts, data_2, by = "to", all.x = TRUE)

#plotting the given lat and long of data_2
chi_dat<- as.data.table(data_2)
coordinates(chi_dat)<- c("LCTN_X","LCTN_Y")
crs.geo1<- CRS("+proj=longlat")
proj4string(chi_dat) = crs.geo1
# plot(chi_dat, pch=20, col="steelblue")

#making map of sejong city using google map
#install.packages("tidyverse1")

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')

locations_sf<- st_as_sf(data_2, coords = c("LCTN_X","LCTN_Y"), crs=4326)
#view based on continuous variable
mapview(locations_sf, cex = "count_43", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "count_44", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "count_64", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "total_count", highlight = TRUE, legend = TRUE, burst = FALSE)



data3<- data_2
data3$X_64<- 127.3077023
data3$y_64<- 36.48032139
data3$X_44<- 127.275287
data3$y_44<- 36.467798
data3$X_43<- 127.264536
data3$y_43<- 36.467439

write.csv(data3, "D:/BILAL/sejong to daejeon OD/OD_output_17_20.csv")

#entering from sejong to daejeon...........................................................................
#reading data
rse <- read.csv("D:/BILAL/sejong to daejeon OD/RSE수집이력_2020년9월21일_17시~20시.txt", header = TRUE)

#data has issue, so we read txt and then we have to split it into columns and give names
library(stringr)
library(dplyr)
data<- str_split_fixed(rse$ID.CLCT_DT.OBU_IDNT_NMBR.CTYP.OBU_KIND, " ",5)
data<- as.data.frame(data)
names(data)<- c("id","date_time","car_num","ctyp","obukind")
y<- data[order(data$car_num,data$date_time),]

#getting O-D of each car
z3<- y %>% group_by(car_num) %>% slice(c(1,n()))
z4<- z3 %>% group_by(car_num) %>% slice(n())

#taking out 64 and 44 ids destination only and placing true for 64 column 
z5<- z4[z4$id %in% c("64","44","43"),]
z5$value<- "TRUE"
z5$to_64 <- ifelse(z5$id ==64, "TRUE", "FALSE")
z5$to_44 <- ifelse(z5$id ==44, "TRUE", "FALSE")
z5$to_43 <- ifelse(z5$id ==43, "TRUE", "FALSE")

#combine it with the original O-D file 
z6<- merge(x= z3,y= z5, by = "car_num", all.x = TRUE)
z6<- z6[,-c(6,7,8,9)]

#ordering by car num and time of each car num
z66<- z6[order(z6$car_num,z6$date_time.x),]

#taking out the O values traveling to 64 and 44 Destinations
z7<- subset(z66 , (value == "TRUE"))
z7<- subset(z7 , (id.x != 64))
z7<- subset(z7 , id.x != 44)
z7<- subset(z7 , id.x != 43)

# count_res_O<- count(z7, id.x)
# count_res_O<- count_res_O[order(count_res_O$n),]
# count_res_D<- count(z7, to_64)
# count_res_D<- count_res_D[order(count_res_D$n),]

#OD counts for 64
count_64_O<- subset(z7, z7$to_64==TRUE)
x<- table(count_64_O$id.x)
t<- as.data.frame(x)
names(t)<- c("from", "count_64")
#t$to<- 64
# write.csv(t, "D:/BILAL/sejong to daejeon OD/OD64_output.csv")

#OD counts for 44
count_44_O<- subset(z7, z7$to_44==TRUE)
xx<- table(count_44_O$id.x)
tt<- as.data.frame(xx)
names(tt)<- c("from", "count_44")
#tt$to <- 44 
# write.csv(tt, "D:/BILAL/sejong to daejeon OD/OD44_output.csv")

#OD counts for 43
count_43_O<- subset(z7, z7$to_43==TRUE)
xxx<- table(count_43_O$id.x)
ttt<- as.data.frame(xxx)
names(ttt)<- c("from", "count_43")
#tt$to <- 43 
# write.csv(ttt, "D:/BILAL/sejong to daejeon OD/OD43_output.csv")



#OD table for 64 and 44
ODtable_counts<- merge(x= t, y=tt, all.x = TRUE, all.y = TRUE, by = "from")
ODtable_counts<- merge(x= ODtable_counts, y=ttt, all.x = TRUE, all.y = TRUE, by = "from")


xxxx<- table(z7$id.x)
tttt<- as.data.frame(xxxx)
names(tttt)<- c("from", "total_count")
ODtable_counts<- merge(x= ODtable_counts, y=tttt, all.x = TRUE,by = "from")
# write.csv(ODtable_counts, "D:/BILAL/sejong to daejeon OD/OD_output.csv")


# install.packages("xlsx")...requires java so it is not working
# #install.packages("writexl")
# library(writexl)
# write.table(z7, "D:/BILAL/sejong to daejeon OD/SD_output.txt", sep="\t")
# write.csv(z7, "D:/BILAL/sejong to daejeon OD/SD_output.csv")
# write_xlsx(z7, "D:/BILAL/sejong to daejeon OD/SD_output.xlsx")
# write_xlsx(ODtable_counts, "D:/BILAL/sejong to daejeon OD/OD_counts.xlsx")


# #df[!duplicated(df$ID),]
# 
# x<- rse[!duplicated(rse$OBU_IDNT_NMBR),]
# 
# write.csv(x,"D:/BILAL/output.csv", row.names = TRUE)


#plotting the OD map
library(data.table)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(sf)
#install.packages("mapview")
library(mapview)
# install.packages("maps")
library(maps)
library(sf)
library(dplyr)
library(stringr)
library(sp)


data_2<- read.csv("D:/BILAL/sejong to daejeon OD/TB_RSE_MSTR_빌랄.csv")
data_2<- data_2[,c(1,6,7,8)]
colnames(data_2)[1]<- "from"
# data_2<- data_2[1:2000,]
data_2<- merge(ODtable_counts, data_2, by = "from", all.x = TRUE)

#plotting the given lat and long of data_2
chi_dat<- as.data.table(data_2)
coordinates(chi_dat)<- c("LCTN_X","LCTN_Y")
crs.geo1<- CRS("+proj=longlat")
proj4string(chi_dat) = crs.geo1
# plot(chi_dat, pch=20, col="steelblue")

#making map of sejong city using google map
#install.packages("tidyverse")

register_google(key = 'AIzaSyCRNk5UxmpemxrqUxQKymycSSVBT5CpYsU')

locations_sf<- st_as_sf(data_2, coords = c("LCTN_X","LCTN_Y"), crs=4326)
#view based on continuous variable
mapview(locations_sf, cex = "count_43", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "count_44", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "count_64", highlight = TRUE, legend = TRUE, burst = FALSE)

mapview(locations_sf, cex = "total_count", highlight = TRUE, legend = TRUE, burst = FALSE)



data3<- data_2
data3$X_64<- 127.3077023
data3$y_64<- 36.48032139
data3$X_44<- 127.275287
data3$y_44<- 36.467798
data3$X_43<- 127.264536
data3$y_43<- 36.467439

write.csv(data3, "D:/BILAL/sejong to daejeon OD/p_2/OD_output_17_20_to.csv")





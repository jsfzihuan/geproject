#*******************************************************************************
#******************Precipatation in Ningxia************************************
#********Created by Shaofei (Shawn) Jin & Sen Ge*******************************
#******************************************************************************

# Something needs to note.
# comment is coming soon
# I just want to print the data out first.

# FOR precipation

prec_data1=read.table("2017prec/201709.txt",header = F)

names(prec_data1)=c("ID","day","hour","prec")

prec_data1$day=as.character(prec_data1$day)
prec_data1$hour=as.character(prec_data1$hour)

prec_data1=transform(prec_data1,
                     hour_data=substr(prec_data1$hour,1,2))

# Extrem precipitation

prec_data2=prec_data1[which(prec_data1$prec >=200),]



prec_data2$hour_data=as.numeric(prec_data2$hour_data)
prec_data2 <- within(prec_data2,{
  stan_hour=NA
  stan_hour[hour_data>=0 & hour_data <=5] = 2
  stan_hour[hour_data>=6 & hour_data <=11] = 8
  stan_hour[hour_data>=12 & hour_data <=17] = 14
  stan_hour[hour_data>=18 & hour_data <=23] = 20
})

#View(prec_data2)

prec_data3=aggregate(prec~ID+day+hour+stan_hour,data=prec_data2,
                     FUN="max")

ID_geo=read.table("2017prec/nxsta.txt")
names(ID_geo)=c("ID","lon","lat","atl")

ID_geo$ID=as.character(ID_geo$ID)

prec_data3=transform(prec_data3,
                     lat=NA,
                     lon=NA,
                     alt=NA)

for (i in levels(factor(ID_geo$ID))) {
  station_position=which(prec_data3$ID == i)
  #print(i)
  if (length(station_position)>1) {
    for (j in 1:length(station_position)) {
      print(length(station_position))
      m=station_position[j]
      prec_data3$lat[m]=ID_geo[which(ID_geo$ID==i),2]
      prec_data3$lon[m]=ID_geo[which(ID_geo$ID==i),3]
      prec_data3$alt[m]=ID_geo[which(ID_geo$ID==i),4]
    }
  }
  if (length(station_position)==1) {
      m=station_position
      prec_data3$lat[m]=ID_geo[which(ID_geo$ID==i),2]
      prec_data3$lon[m]=ID_geo[which(ID_geo$ID==i),3]
      prec_data3$alt[m]=ID_geo[which(ID_geo$ID==i),4]
  }
}


# The folloing code is baby-level.

#prec_201707=prec_data3
#prec_201705=prec_data3
#prec_201706=prec_data3
#prec_201708=prec_data3
#prec_201709=prec_data3

prec_2017=rbind(prec_201705,
                prec_201706,
                prec_201707,
                prec_201708,
                prec_201709)

write.table(prec_2017,"textdata.txt")
write.csv(prec_2017,"Prec_2017.csv")
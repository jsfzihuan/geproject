


install.packages("ncdf4","RNetCDF")  # ncdf4 or RNetCDF
library(lubridate)
library(ncdf4)
# library(RNetCDF)
# 
# data1=open.nc("qf17.nc")
# names_data1=print.nc(data1)

data2=nc_open("q.nc")
varnames=names(data2$var)

x=ncvar_get(data2,varid = "longitude")
y=ncvar_get(data2,varid = "latitude")

data2$dim$time$units
time=ncvar_get(data2,varid = "time")

time1=(time-1)*3600+3600
# time_ge=as.Date(time, origin=c("1900-01-01 00:00:00"))

time_ge_i=as.POSIXct(time1, origin = "1900-01-01")
time_ge=ymd_hms(time_ge_i)

#as_date(time, origin=lubridate::origin)
#%Y年%m月%d日%H时%M分%S秒
#as.Date("1014600", "%y%c")

as.Date("116033", "%y%H")
dataset=matrix(NA,nrow = 33*41*10*612,ncol = 7)
for (i in 1:length(varnames)) {
  file1_i=ncvar_get(data2,varid = varnames[i])
  
  for (j in 1:10){
    file1_i_j=file1_i[,,j,]
    file1_i_j=as.data.frame(as.vector(file1_i_j))
    dataset[(33*41*612*(j-1)+1):(33*41*612*j),i]=file1_i_j[,1]
    #dataset[,]=rbind(dataset[,1],file1_i_j[,1])
  }
}

dataset=as.data.frame(dataset)
lonlat <- as.matrix(expand.grid(x,y,1:10,time_ge))

dataset1 = data.frame(cbind(lonlat,dataset))

names(dataset1)=c("lon","lat","level","time",
                      "w","d","r","t","q","vo","v")

dataset1$lon=as.numeric(dataset1$lon)
dataset1$lat=as.numeric(dataset1$lat)
dataset1$time=as.character(dataset1$time)

dataset1=transform(dataset1,
                   daytime=substr(dataset1$time,1,10),
                   hourtime=substr(dataset1$time,12,13))
dataset1$daytime=as.Date(dataset1$daytime,format="%Y-%m-%d")
# write.csv(dataset1,"dataset1.csv")

test_prec=read.table("20140601201.txt",header = F)

names(test_prec)=c("ID","jingdu","weidu",
                   "prec","timeday","timehour")

# test_prec_i=aggregate(cbind(prec,timeday,timehour)~ID+jingdu+weidu,
#                       data=test_prec,FUN="mean")

# interplorate 

 test_prec$timeday=as.Date(as.character(test_prec$timeday),
                           format="%Y/%m/%d")
#                           
test_prec$timeday=as.character(test_prec$timeday)                          
test_prec$timehour=substr(as.character(test_prec$timehour),1,2)

# test_prec=transform(test_prec,
#                     ymdh=ymd(timeday),
#                     hour=hour(timehour))



# extract the data for the same

for (i in levels(factor(test_prec$timeday))) {
  for (j in levels(factor(test_prec$timehour))) {
    position=which(dataset1$daytime==i & dataset1$hourtime==j)
    for (k in 1:length(position)) {
      dataset1[position[k],14]= "1"
      #dataset1[position[m],14]= "1"
    }
  }
}

dataset1[is.na(dataset1$V14)] == "0"

# factor(dataset1$V14,
#        levels = c(NA,"1"),
#        labels = c("0", "1"))

dataset1$V14[is.na(dataset1$V14)] = "0"




#others rubish====
str(data2$var$w)

str(data2$dim)
names(data2$dim)


file1_w=ncvar_get(data2,varid = "w")

str(file1_w)
dim(file1_w)

file1_w_100=ncvar_get(data2,
                      varid = "w",
                      count = c(-1,-1,1,-1))
file1_w_100_mean=apply(file1_w_100, 
                       c(1,2), FUN="mean")

x=ncvar_get(data2,varid = "longitude")
y=ncvar_get(data2,varid = "latitude")


image(x,y,file1_w_100[, ,1 ])
contour(y,x,file1_w_100_mean,nlevels = 12)

grid <- expand.grid(lon=x, lat=y)
# cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(file1_w_100_mean ~ x * y, data=grid, pretty=T)

lonlat <- as.matrix(expand.grid(x,y,1:10))
file1_w_100_mean_1 <- as.vector(file1_w_100_mean)
file1_w_100_mean_plot= data.frame(cbind(lonlat,file1_w_100_mean_1 ))
names(file1_w_100_mean_plot)=c("lon","lat","wmean")

library(ggplot2)
 ggplot(file1_w_100_mean_plot,
        aes(x=lon,y=lat))+
   geom_tile(aes(fill=wmean))
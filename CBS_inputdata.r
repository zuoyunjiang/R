### 2020-12-21 ###
### IGA ###
### ZYJ zuoyunjiang@iga.ac.cn ###
###  CBS forcing clm_data (42.26N,128.05E)(273,581)###
library(readxl)
library(ncdf4)
library(sqldf)
######################take the input data flds begin #####################################

data <- read_excel('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/2003年长白山气象30分钟数据.xlsx',1)
View(data)
WIND <- as.numeric(data$近地面风速[-1])
PSRF <- as.numeric(data$大气压[-1])
TBOT <- as.numeric(data$冠层上方空气温度[-1])
RH <- as.numeric(data$冠层上方空气湿度[-1])
FSDS <- as.numeric(data$太阳辐射[-1])
PRECTmms <- as.numeric(data$日降水量[-1])
## lrad - 3h ##
a <- list.files('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/lrad/')
a <- a[c(97:192)]
a
a_dir <- paste('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/lrad/',a,sep = '')
a_dir
n <- length(a_dir)
nc_data <- nc_open(filename = a_dir[1])
lrad <- ncvar_get(nc_data, varid = 'lrad')
FLDS <- lrad[581,273,]
nc_close(nc_data)
for(i in 2:n){
  nc_data <- nc_open(filename = a_dir[i])
  lrad <- ncvar_get(nc_data, varid = 'lrad')
  FLDS <- cbind(FLDS,lrad[581,273,])
  nc_close(nc_data)
}
write.table(FLDS,file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/lrad/flds.csv',sep = ',',col.names = NA, qmethod = 'double')
#################  take the input data flds end #######################################
###################### shu ju cha zhi begin########################################################
#### flds data 3h->0.5h ####
## the first month
test <- FLDS[,1]
x <- c(seq(1,1488,6))

data <- test[1:2]
x <- c(1,7)
lm.flds <- lm(data ~ x)
result <- summary(lm.flds)
a <- result$coefficients[2,1]
b <- result$coefficients[1,1]
x2 <- a*2+b
x3 <- a*3+b
x4 <- a*4+b
x5 <- a*5+b
x6 <- a*6+b
merge_data <- rbind(data[1],x2,x3,x4,x5,x6)

for(i in 2:247){
  data <- test[c(i,i+1)]
  x <- c(1,7)
  lm.flds <- lm(data ~ x)
  result <- summary(lm.flds)
  a <- result$coefficients[2,1]
  b <- result$coefficients[1,1]
  x2 <- a*2+b
  x3 <- a*3+b
  x4 <- a*4+b
  x5 <- a*5+b
  x6 <- a*6+b
  merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6)
}

data <- c(test[248],FLDS[1,2])
x <- c(1,7)
lm.flds <- lm(data ~ x)
result <- summary(lm.flds)
a <- result$coefficients[2,1]
b <- result$coefficients[1,1]
x2 <- a*2+b
x3 <- a*3+b
x4 <- a*4+b
x5 <- a*5+b
x6 <- a*6+b
merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6)
merge_data1 <- cbind(merge_data) # cbind the first month
#@@@@@@@@@@@@@      one month end         @@@@@@@@@@@@#

##@@@@@@@@ the 2:95 month@@@@@@@@@@#
for(k in 2:95){
  test <- FLDS[,k]
  data <- test[1:2]
  x <- c(1,7)
  lm.flds <- lm(data ~ x)
  result <- summary(lm.flds)
  a <- result$coefficients[2,1]
  b <- result$coefficients[1,1]
  x2 <- a*2+b
  x3 <- a*3+b
  x4 <- a*4+b
  x5 <- a*5+b
  x6 <- a*6+b
  merge_data <- rbind(data[1],x2,x3,x4,x5,x6)
  
  for(i in 2:247){
    data <- test[c(i,i+1)]
    x <- c(1,7)
    lm.flds <- lm(data ~ x)
    result <- summary(lm.flds)
    a <- result$coefficients[2,1]
    b <- result$coefficients[1,1]
    x2 <- a*2+b
    x3 <- a*3+b
    x4 <- a*4+b
    x5 <- a*5+b
    x6 <- a*6+b
    merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6)
  }
  
  data <- c(test[248],FLDS[1,k+1])
  x <- c(1,7)
  lm.flds <- lm(data ~ x)
  result <- summary(lm.flds)
  a <- result$coefficients[2,1]
  b <- result$coefficients[1,1]
  x2 <- a*2+b
  x3 <- a*3+b
  x4 <- a*4+b
  x5 <- a*5+b
  x6 <- a*6+b
  merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6) # one month 
  
  # year
  merge_data1 <- cbind(merge_data1,merge_data) # cbind the next month
  
}
# the last month 96
test <- FLDS[,96]
data <- test[1:2]
x <- c(1,7)
lm.flds <- lm(data ~ x)
result <- summary(lm.flds)
a <- result$coefficients[2,1]
b <- result$coefficients[1,1]
x2 <- a*2+b
x3 <- a*3+b
x4 <- a*4+b
x5 <- a*5+b
x6 <- a*6+b
merge_data <- rbind(data[1],x2,x3,x4,x5,x6)

for(i in 2:247){
  data <- test[c(i,i+1)]
  x <- c(1,7)
  lm.flds <- lm(data ~ x)
  result <- summary(lm.flds)
  a <- result$coefficients[2,1]
  b <- result$coefficients[1,1]
  x2 <- a*2+b
  x3 <- a*3+b
  x4 <- a*4+b
  x5 <- a*5+b
  x6 <- a*6+b
  merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6)
}

data <- c(test[248],FLDS[1,2])
x <- c(1,7)
lm.flds <- lm(data ~ x)
result <- summary(lm.flds)
a <- result$coefficients[2,1]
b <- result$coefficients[1,1]
x2 <- a*2+b
x3 <- a*3+b
x4 <- a*4+b
x5 <- a*5+b
x6 <- a*6+b
merge_data <- rbind(merge_data,data[1],x2,x3,x4,x5,x6)
merge_data1 <- cbind(merge_data1,merge_data)
write.table(merge_data1,file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/lrad/flds-30min.csv',sep = ',',row.names = F, qmethod = 'double')
############################ cha zhi end  ###################################

setwd('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/30nc/')


flds_data <- read.table(file = 'D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/lrad/flds-30min.csv',sep = ',')
flds_data <- flds_data[2:1488,]
file_list <- list.files('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/')
file_list <- file_list[1:8]
file_dir <- paste('D:/AAAA-资料E盘/CLM/DATA/长白山/30分钟/',file_list,sep = '')
file_num <- length(file_dir)

#
clm_data <- read_excel(file_dir[1],1)
WIND <- as.numeric(clm_data$冠层上方风速[-1])
PSRF <- as.numeric(clm_data$大气压[-1])
TBOT <- as.numeric(clm_data$冠层上方空气温度[-1])
RH <- as.numeric(clm_data$冠层上方空气湿度[-1])
FSDS <- as.numeric(clm_data$太阳辐射[-1])
PRECTmms <- as.numeric(clm_data$日降水量[-1])
ZBOT <- rep(2,17520)
year <- clm_data$年[-1]
month <- clm_data$月[-1]
clm_data_1 <- data.frame(year, month, WIND,PSRF,TBOT,RH,FSDS,PRECTmms)

for(j in 1:file_num){
clm_data <- read_excel(file_dir[j],1)
# for(s in 1:28){
#   for(h in 1:17520)
#     if(clm_data[h,s] == -99999.){
#       WIND[s] = NA
#   }
# }
WIND <- as.numeric(clm_data$冠层上方风速[-1])
PSRF <- as.numeric(clm_data$大气压[-1])
TBOT <- as.numeric(clm_data$冠层上方空气温度[-1])
RH <- as.numeric(clm_data$冠层上方空气湿度[-1])
FSDS <- as.numeric(clm_data$太阳辐射[-1])
PRECTmms <- as.numeric(clm_data$日降水量[-1])
ZBOT <- rep(2,17520)
year <- clm_data$年[-1]
month <- clm_data$月[-1]
clm_data <- data.frame(year, month, WIND,PSRF,TBOT,RH,FSDS,PRECTmms)

month_factor <- sqldf("select DISTINCT month from clm_data")
month_factor <- as.numeric(as.matrix(month_factor))
month_factor_1 <- formatC(month_factor,width = 2,flag = '0')


  for(i in 1:12){
  
  num <- length(clm_data$month[which(month == month_factor[i])])
  # create dim 
  dimX <- ncdim_def('lon','',1:1,unlim = FALSE,create_dimvar = FALSE)
  dimy <- ncdim_def('lat','',1:1,unlim = FALSE,create_dimvar = FALSE)
  dims <- ncdim_def('scalar','',1:1,unlim = FALSE,create_dimvar = FALSE)
  c <- paste('days since ',year[2],'-',month_factor_1[i] ,'-01 00:00:00',sep = '')
  dimtime <- ncdim_def('time',longname = 'Time axis', units = c,vals = ((0:(num-1))/48.),unlim = FALSE,create_dimvar = TRUE,calendar = 'noleap')

  #mv <- 1.e30
  EDGEE <- ncvar_def('EDGEE',longname = 'eastern edge in atmospheric data', units = 'degrees E', dims, prec = 'double')
  EDGEN <- ncvar_def('EDGEN',longname = 'northern edge in atmospheric data', units = 'degrees N', dims, prec = 'double')
  EDGES <- ncvar_def('EDGES',longname = 'southern edge in atmospheric data', units = 'degrees N', dims, prec = 'double')
  EDGEW <- ncvar_def('EDGEW',longname = 'western edge in atmospheric data', units = 'degrees E', dims, prec = 'double')
  FLDS <- ncvar_def('FLDS','W/m2',list(dimX, dimy, dimtime),longname = 'incident longwave (FLDS)',prec = 'double')
  FSDS <- ncvar_def('FSDS','W/m2',list(dimX, dimy, dimtime),longname = 'incident solar (FSDS)',prec = 'double')
  LAITXY <- ncvar_def('LAITXY', longname = 'latitude', units = 'degrees N', list(dimX,dimy),prec = 'double')
  LONGXY <- ncvar_def('LONGXY', longname = 'longitude',units = 'degrees E', list(dimX,dimy),prec = 'double')
  PRECTmms <- ncvar_def('PRECTmms','mm/s',list(dimX, dimy, dimtime),longname = 'precipitation (PRECTmms)',prec = 'double')
  PSRF <- ncvar_def('PSRF','Pa',list(dimX, dimy, dimtime),longname = 'pressure at the lowest atm level (PSRF)',prec = 'double')
  RH <- ncvar_def('RH','%',list(dimX, dimy, dimtime),longname = 'relative humidity at the lowest atm level (RH)',prec = 'double')
  TBOT <- ncvar_def('TBOT','K',list(dimX, dimy, dimtime),longname = 'temperature at the lowest atm level (TBOT)',prec = 'double')
  # time <- ncvar_def('time','days since 2005-01-01 00:00:00',dimtime,longname = 'Time axis',prec = 'double',missval = mv)
  WIND <- ncvar_def('WIND','m/s',list(dimX, dimy, dimtime),longname = 'wind at the lowest atm level (WIND)',prec = 'double')
  ZBOT <- ncvar_def('ZBOT','m',list(dimX, dimy, dimtime),longname = 'observational height',prec = 'double')

  # create new nc file 
  filename <- paste(year[2],'-',month_factor_1[i],'.nc',sep = '')
  nc_data <- nc_create(filename = filename, list(EDGEE,EDGEN,EDGES,EDGEW,FLDS,FSDS,LAITXY,LONGXY,PRECTmms,PSRF,RH,TBOT,WIND,ZBOT))

  # input the data to var
  m <- length(clm_data$month[which(clm_data$month == month_factor[i])])
  ncvar_put(nc_data,EDGEE,128.05)
  ncvar_put(nc_data,EDGEN,42.25)
  ncvar_put(nc_data,EDGES,42.25)
  ncvar_put(nc_data,EDGEW,128.05)
  #### flds
  colflds <- i + 12*(j-1)
  flds <- flds_data[1:m,colflds]
  ncvar_put(nc_data,FLDS,flds)
  #### fsds
  fsds <- clm_data$FSDS[which(clm_data$month == month_factor[i])]
  fsds_1 <- clm_data$FSDS[which(clm_data$month == month_factor[i])]
  for(k in 1:m){
    if(fsds[k]== -99999){
      fsds_1[k] = clm_data_1$FSDS[which(clm_data_1$month == i)][k]
    }
  }
  ncvar_put(nc_data,FSDS,fsds_1)

  ncvar_put(nc_data,LAITXY,42.25)
  ncvar_put(nc_data,LONGXY,128.05)

  #### pr
  pr <- clm_data$PRECTmms[which(clm_data$month == month_factor[i])]
  pr_1 <- clm_data$PRECTmms[which(clm_data$month == month_factor[i])]
  for(k in 1:m){
    if(pr[k]== -99999){
      pr_1[k] = clm_data_1$PRECTmms[which(clm_data_1$month == i)][k]
    }
  }
  pr_1 <- pr_1/1800.  ## mm/s
  ncvar_put(nc_data,PRECTmms,pr_1)
  #### ps
  ps <- clm_data$PSRF[which(clm_data$month ==month_factor[i])]
  ps_1 <- clm_data$PSRF[which(clm_data$month ==month_factor[i])]
  for(k in 1:m){
    if(ps[k]== -99999){
      ps_1[k] = clm_data_1$PSRF[which(clm_data_1$month == i)][k]
    }
  }
  ncvar_put(nc_data,PSRF,(ps_1*1000.))


  ######rh
  rh <- clm_data$RH[which(clm_data$month == month_factor[i])]
  rh_1 <- clm_data$RH[which(clm_data$month == month_factor[i])]
  for(k in 1:m){
    if(rh[k]== -99999){
      rh_1[k] = clm_data_1$RH[which(clm_data_1$month == i)][k]
    }
  }
  ncvar_put(nc_data,RH,rh_1)

  #### t
  t <- clm_data$TBOT[which(clm_data$month ==month_factor[i])]
  t_1 <- clm_data$TBOT[which(clm_data$month ==month_factor[i])]
  for(k in 1:m){
    if(t[k]== -99999){
      t_1[k] = clm_data_1$TBOT[which(clm_data_1$month == i)][k]
    }
  }
  ncvar_put(nc_data,TBOT,(t_1 + 273.15)) #  k

  ### wind
  wind <- clm_data$WIND[which(clm_data$month ==month_factor[i])]
  wind_1 <- clm_data$WIND[which(clm_data$month ==month_factor[i])]
  for(k in 1:m){
    if(wind[k] == -99999){
      wind_1[k] = clm_data_1$WIND[which(clm_data_1$month == i)][k]
    }
  }
  ncvar_put(nc_data,WIND,wind_1)
  #### fsds
  z <- rep(2,m)
  ncvar_put(nc_data,ZBOT,z)

  # varid=0 means it is a global attribute
  ncatt_put( nc_data, 0, "institute", "IGA") 
  ncatt_put( nc_data, 0, "history", "Jun, 2020, the file was created at IGA for CLM-mircrobe simulation --Yunjiang Zuo: zuoyunjiang@iga.ac.cn")
  ncatt_put( nc_data, 0, "site_location", "Changbaishan Station, one of CERN sites in China, Lat: 42.25; Longitude: 128.05")

  ## close nc file
  nc_close(nc_data)
  }
}

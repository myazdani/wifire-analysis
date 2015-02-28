
## wifi_hourly_hsv_analysis.R
##
## 
library(data.table)
#####-------------------------------------
##### setup data
#####-------------------------------------
#dt = fread("~/Documents/wifire/processedData/features/output_SD_all.csv", header = FALSE)
dt = fread("~/Documents/wifire/processedData/features/output_SD_all_days.csv", header = FALSE)
new.col.names = c("filepath", paste0("H", c(1:180)), paste0("S", c(1:256)), paste0("V", c(1:256)))
setnames(dt, names(dt), new.col.names)

dt$day = sapply(dt$filepath, FUN = function(x) strsplit(x, split = "/")[[1]][7])

get.hour = function(x){
  timestamp = strsplit(x, split = "/")[[1]][length(strsplit(x, split = "/")[[1]])]
  timestamp = as.POSIXct(as.numeric(strsplit(timestamp, split = ".jpg")[[1]][1]), origin = "1970-01-01")
  return(hour(timestamp))
}

dt$hour = sapply(dt$filepath, get.hour)

res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(day, hour), .SDcols = 2:693])
rm(dt)
res.H = res[,c(1,2,which(grepl("H", names(res))))]
res.S = res[,c(1,2,which(grepl("S", names(res))))]
res.V = res[,c(1,2,which(grepl("V", names(res))))]
rm(res)
library(ggplot2)
library(reshape)
res.H.m = melt(res.H, id.vars = c("day", "hour"))
res.S.m = melt(res.S, id.vars = c("day", "hour"))
res.V.m = melt(res.V, id.vars = c("day", "hour"))

ggplot(res.H.m, aes(x = variable, y = value, colour = day)) + geom_point() + facet_wrap(~ hour, nrow = 3) 
ggplot(res.S.m, aes(x = variable, y = value, colour = day)) + geom_point() + facet_wrap(~ hour, nrow = 3) 
ggplot(res.V.m, aes(x = variable, y = value, colour = day)) + geom_point() + facet_wrap(~ hour, nrow = 3) 

#####-------------------------------------
#####
#####-------------------------------------
return_mds_per_day = function(df){
  days = unique(df$day)
  days.mds = data.frame(day = NA, hour = NA, coord.1 = NA, coord.2 = NA)
  for (i in c(1:length(days))){
    df.temp = subset(df, day == days[i])
    df.temp$day = NULL
    row.names(df.temp) = df.temp$hour
    dist.matrix = dist(df.temp[, -which(names(df.temp) == "hour")], method = "euclidean")
    fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim 
    mds.1 = fit$points[,1]
    mds.2 = fit$points[,2]
    
    if (max(mds.1) < max(abs(mds.1))) { mds.1 = -1*mds.1}
    #if (max(mds.2) < max(abs(mds.2))) { mds.2 = -1*mds.2}
    
    days.mds = rbind(days.mds, data.frame(day = days[i], hour = row.names(fit$points), coord.1 = mds.1, coord.2 = mds.2))
  }
  return(days.mds[-1,])
}

mds.H = return_mds_per_day(res.H)
ggplot(mds.H, aes(x = coord.1, y = coord.2, label = hour)) + facet_wrap( ~ day, nrow = 2) + geom_text() + ggtitle("MDS of Hue Distributions")
mds.S = return_mds_per_day(res.S)
ggplot(mds.S, aes(x = coord.1, y = coord.2, label = hour)) + facet_wrap( ~ day, nrow = 2) + geom_text() + ggtitle("MDS of Saturation Distributions")
mds.V = return_mds_per_day(res.V)
ggplot(mds.V, aes(x = coord.1, y = coord.2, label = hour)) + facet_wrap( ~ day, nrow = 2) + geom_text() + ggtitle("MDS of Value Distributions")

#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
first.row = c(c(0:12), c(11:1))
dist.hours = toeplitz(first.row)
rownames(dist.hours) = unique(res.S$hour[order(as.integer(res.S$hour))])
colnames(dist.hours) = unique(res.S$hour[order(as.integer(res.S$hour))])

#####-------------------------------------
##### setup hour distance matrix
#####-------------------------------------
get.dist.df = function(x){
  if (is.matrix(x)){ 
    # if it is a matrix already, then assume that it is a proper distance matrix
    z = x
  } 
  else{
    z = as.matrix(dist(x)) 
  }
  z[lower.tri(z,diag=TRUE)]=NA  #Prepare to drop duplicates and meaningless information
  z=as.data.frame(as.table(z))  #Turn into a 3-column table
  z=na.omit(z)  #Get rid of the junk we flagged above
  z=z[order(-abs(z$Freq)),] 
  
  z$Var1 = as.character(z$Var1)
  z$Var2 = as.character(z$Var2)
  ordered.pair = c()
  for (i in c(1:nrow(z))){
    if (z$Var1[i] > z$Var2[i]){
      ordered.pair = c(ordered.pair, 
                       paste(z$Var1[i], "-",z$Var2[i]))
    }
    else{
      ordered.pair = c(ordered.pair,
                       paste(z$Var2[i], "-",z$Var1[i]))
    }
  }
  z$pairs = ordered.pair
  z$Var1 = NULL
  z$Var2 = NULL
  return(z)
}

dist.hours.df = get.dist.df(dist.hours)
names(dist.hours.df)[1] = "hours.dist"

hours.HSV.list = list()
days = unique(res.H$day)
library(plyr)
for (i in c(1:length(days))){
  res.H.temp = subset(res.H, day == days[i])
  res.H.temp$day = NULL
  res.S.temp = subset(res.S, day == days[i])
  res.S.temp$day = NULL
  res.V.temp = subset(res.V, day == days[i])
  res.V.temp$day = NULL
  
  ## get H distances
  row.names(res.H.temp) = res.H.temp[,1]
  H.dist = get.dist.df(res.H.temp[,-1])
  names(H.dist)[1] = "hue.dist"
  ## get S distances
  row.names(res.S.temp) = res.S.temp[,1]
  S.dist = get.dist.df(res.S.temp[,-1])
  names(S.dist)[1] = "saturation.dist"
  ## get V distances
  row.names(res.V.temp) = res.V.temp[,1]
  V.dist = get.dist.df(res.V.temp[,-1])
  names(V.dist)[1] = "value.dist"
  hours.HSV = join_all(list(dist.hours.df, H.dist, S.dist, V.dist))
  hours.HSV$date = days[i]
  hours.HSV.list[[i]] = hours.HSV
}


hours.HSV.res = do.call(rbind, hours.HSV.list)

ggplot(hours.HSV.res, aes(x = hue.dist, y = hours.dist, label = pairs)) + geom_text() + facet_wrap( ~ date, nrow = 2) + geom_text() + stat_smooth(method = lm)
ggplot(hours.HSV.res, aes(x = saturation.dist, y = hours.dist, label = pairs)) + geom_text() + facet_wrap( ~ date, nrow = 2) + geom_text() + stat_smooth(method = lm)
ggplot(hours.HSV.res, aes(x = value.dist, y = hours.dist, label = pairs)) + geom_text() + facet_wrap( ~ date, nrow = 2) + geom_text() + stat_smooth(method = lm)

find.corr.hue = function(df){
  return(data.frame(hue.corr = cor(df$hue.dist, df$hours.dist),sat.corr =  cor(df$saturation.dist, df$hours.dist), val.corr = cor(df$value.dist, df$hours.dist)))
}

fire.corr.hue = ddply(hours.HSV.res, .(date), find.corr.hue)



###################################




## wifi_hourly_ZONES_hsv_analysis.R
##
## 
library(data.table)
#####-------------------------------------
##### setup data
#####-------------------------------------
#dt = fread("~/Documents/wifire/processedData/features/output_SD_all.csv", header = FALSE)
dt = fread("~/Documents/wifire/processedData/features/output_VAL_ZONES_3__ALL_DAYS_SD.csv", header = FALSE)
#new.col.names = c("filepath", paste0("H", c(1:1620)))
new.col.names = c("filepath", paste0("S", c(1:2304)))
setnames(dt, names(dt), new.col.names)


dt$day = sapply(dt$filepath, FUN = function(x) strsplit(x, split = "/")[[1]][7])

dt$hour.chunk = sapply(dt$filepath, FUN = function(x) strsplit(x, split = "/")[[1]][8])

res = as.data.frame(dt[,lapply(.SD, FUN = function(x) as.double(mean(x))), by = list(day, hour.chunk), .SDcols = 2:2305])
first_col = 8*256+1
last_col = 9*256

res = cbind(res[,c(1,2)],res[,c(paste0("S", c(first_col:last_col)))])
rm(dt)
library(ggplot2)
library(reshape)
res.H.m = melt(res, id.vars = c("day", "hour.chunk"))

ggplot(res.H.m, aes(x = variable, y = value, colour = day)) + geom_point() + facet_wrap(~ hour.chunk, nrow = 3) 

#####-------------------------------------
#####
#####-------------------------------------
return_mds_per_day = function(df){
  days = unique(df$day)
  days.mds = data.frame(day = NA, hour.chunk = NA, coord.1 = NA, coord.2 = NA)
  for (i in c(1:length(days))){
    df.temp = subset(df, day == days[i])
    df.temp$day = NULL
    row.names(df.temp) = df.temp$hour.chunk
    dist.matrix = dist(df.temp[, -which(names(df.temp) == "hour.chunk")], method = "euclidean")
    fit <- cmdscale(dist.matrix,eig=TRUE, k=2) # k is the number of dim 
    mds.1 = fit$points[,1]
    mds.2 = fit$points[,2]
    
    if (max(mds.1) < max(abs(mds.1))) { mds.1 = -1*mds.1}
    #if (max(mds.2) < max(abs(mds.2))) { mds.2 = -1*mds.2}
    
    days.mds = rbind(days.mds, data.frame(day = days[i], hour.chunk = row.names(fit$points), coord.1 = mds.1, coord.2 = mds.2))
  }
  return(days.mds[-1,])
}

mds.H = return_mds_per_day(res)
ggplot(mds.H, aes(x = coord.1, y = coord.2, label = hour.chunk)) + facet_wrap( ~ day, nrow = 2) + geom_text() + ggtitle("MDS of Hue Distributions")

#####-------------------------------------
##### setup hour.chunk distance matrix
#####-------------------------------------
first.row = c(c(0:3), c(3:0))
dist.hour.chunks = toeplitz(first.row)
hour.chunks = unique(res$hour.chunk)
rownames(dist.hour.chunks) = hour.chunks[order(hour.chunks)]
colnames(dist.hour.chunks) = hour.chunks[order(hour.chunks)]

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

dist.hour.chunks.df = get.dist.df(dist.hour.chunks)
names(dist.hour.chunks.df)[1] = "hours.dist"

hours.H.list = list()
days = unique(res$day)
library(plyr)
for (i in c(1:length(days))){
  res.H.temp = subset(res, day == days[i])
  res.H.temp$day = NULL
  ## get H distances
  row.names(res.H.temp) = res.H.temp[,1]
  H.dist = get.dist.df(res.H.temp[,-1])
  names(H.dist)[1] = "hue.dist"
  hours.H = join_all(list(dist.hour.chunks.df, H.dist))
  hours.H$date = days[i]
  hours.H.list[[i]] = hours.H
}


hours.HSV.res = do.call(rbind, hours.H.list)

ggplot(hours.HSV.res, aes(x = hue.dist, y = hours.dist, label = pairs)) + geom_text() + facet_wrap( ~ date, nrow = 2) + geom_text() + stat_smooth(method = lm)


find.corr.hue = function(df){
  return(data.frame(hue.corr = cor(df$hue.dist, df$hours.dist)))
}

fire.corr.hue = ddply(hours.HSV.res, .(date), find.corr.hue)
print(fire.corr.hue)

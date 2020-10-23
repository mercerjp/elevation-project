#comparing invertebrate abundances as a proxy for resistance before and after a drought.
#investigating years 2002, 2003, 2004 in England
#summer of 2003 had a drought
#James Mercer 2020

library(tidyverse)
#get UK national river from archive packages
library(rnrfa)
library(googleway)
library(googlePolylines)
library(effects)
library(devtools)
library(ggiraphExtra)
library(ggiraph)
library(mgcv)
library(visreg)
library(cowplot)
library(darksky)
library(zoo)
library(scales)
#Load data
data <- read.csv("England_Invertebrate_Surveys_2015_to_2019.csv")

#check columns, should have 20
ncol(data)

#remove NAs
data <- na.omit(data)

#convert Ordnance Survey (OS) grid reference to lat/long coordinates
latlong <- osg_parse(data$NGR_10_FIG, coord_system = c("WGS84"))

#Convert list to dataframe
LatLongDat <- data.frame(matrix(unlist(latlong), ncol=length(latlong)),stringsAsFactors=FALSE)

#rename columns and insert into dataframe
data$Latitude <- LatLongDat$X2
data$Longitude <- LatLongDat$X1 

LatLongDat <- LatLongDat %>% 
  rename(latitude = X2, longitude = X1)

#change order
LatLongDat <- LatLongDat[,c(2,1)]

#remove duplicates 
dedupLatLong <- unique(LatLongDat[ , 1:2])

#divide dataset smaller into smaller lists
LatLongList <- split(dedupLatLong, (seq(nrow(dedupLatLong))-1) %/% 500) 

#choose individual lists and encode using google polylines (10) 'encode' and lapply
#because the dataframe is too big
EncodedList <- lapply(LatLongList, encode)
  
#get elevation for data points (10) #API key: INPUT YOUR OWN API KEY FOR GOOGLE ELEVATION

elevation_API <- function(x){
  a <- google_elevation(polyline = x,
                   key = "ENTER OWN API KEY", simplify = TRUE)
  return(a)
}
LatLongElev <- lapply(EncodedList, elevation_API)

#convert to dataframe
elevation <- do.call("rbind", LatLongElev)
#rename lat and long in elevation to match data 
elevation$Latitude <- elevation$location.lat
elevation$Longitude <- elevation$location.lng

elevation$location.lat <- NULL
elevation$location.lng <- NULL

#match elevation dataset with big dataset by lat and long
#set latitude and longitude to five/four decimal places for elevation and the data because the google API trimmed them (in order to match up properly)
#save data to do this in excel
write.csv(data,'data_v2.csv')
write.csv(elevation, 'elevation_v2.csv')

#in excel on both .csv files use =LEFT(CONCATENATE(E2), 8) (where E2 is the cell being changed + 8 is the character limit)
dissdata <- read.csv("data_2_correctdp.csv")

elevation <- read.csv("elevation_2_correctdp")

#merge by one of two columns because of slight errors between lat and long
data2 <- merge(dissdata, elevation, by = "Latitude", all = FALSE)
data2$Longitude.y <- NULL
data2$Longitude <- data2$Longitude.x
data2$Longitude.x <- NULL
data2$X <- NULL
data2$X.x <- NULL
data2$X.y <- NULL
#rearrange columns so lat and long are next to eachother
data3 <- data2[,c(1,24,2,3,4:23)]


#make seperate values for highland vs lowland data
lowland <- subset(data3, elevation < 200)
highland <- subset(data3, elevation >= 200)

#name columns
lowland$Land_Type <- "Lowland"
highland$Land_Type <- "Highland"

#recombine datasets
data4 <- rbind(lowland, highland)

#remove other net sampling (only do this if didn't remove in excel first. dissdata_v2.csv only includes this way of sampling)
#data5 <- subset(data4, SAMPLE_METHOD_DESCRIPTION == "3-MIN POND NET (BT001): 3-min active sampling, 1-min hand search as per BT001")

data5 <- data4
#save data5

write.csv(data5, 'data5_v2.csv')

#load data5
data5 <- read.csv("data5_v2.csv")



#subset in years

# format date field
data5$SAMPLE_DATE <- as.Date(data5$SAMPLE_DATE,
                                     format = "%d/%m/%Y")
#subset based on year
data.2015 <- subset(data5, format(as.Date(SAMPLE_DATE),"%Y")==2015)
data.2016 <- subset(data5, format(as.Date(SAMPLE_DATE),"%Y")==2016)
data.2017 <- subset(data5, format(as.Date(SAMPLE_DATE),"%Y")==2017)
data.2018 <- subset(data5, format(as.Date(SAMPLE_DATE),"%Y")==2018)
data.2019 <- subset(data5, format(as.Date(SAMPLE_DATE),"%Y")==2019)

#shows sampling months
hist(data.2015$SAMPLE_DATE, "months")
hist(data.2016$SAMPLE_DATE, "months")
hist(data.2017$SAMPLE_DATE, "months")
hist(data.2018$SAMPLE_DATE, "months")
hist(data.2019$SAMPLE_DATE, "months")


#subset data based on month
#   data.2002.05 <- subset(data.2002, format(as.Date(SAMPLE_DATE),"%m")=="05")
#    data.2003.05 <- subset(data.2003, format(as.Date(SAMPLE_DATE),"%m")=="05")
#      data.2004.05 <- subset(data.2004, format(as.Date(SAMPLE_DATE),"%m")=="05")

#try darksky package - API for getting precipitation



#test

test2 <- get_forecast_for(50.04977, -5.63218, "2003-04-11T12:00:00", units = "uk", add_headers=TRUE)
test2 <- test2$daily
output_test2 <- rbind(test2)

test2 <- test2[1,]
test1 <- get_forecast_for(50.0498,-5.6322, "2013-10-16T12:00:00", units = "uk", add_headers=TRUE)
test1 <- as.data.frame(test1)
test1 <- test1[1,]

#darksky loop test#
data5 <- na.omit(read.csv("data5_v2.csv"))
#subset for december, january, feb for winter months because need to include them
#data.december <- subset(data5, format(as.Date(SAMPLE_DATE),"%m")=="12")
#data.january <- subset(data5, format(as.Date(SAMPLE_DATE),"%m")=="01")
#data.february <- subset(data5, format(as.Date(SAMPLE_DATE),"%m")=="02")
#data5_winter <- rbind(data.december, data.january, data.february)
#data5_winter$X <- NULL
data5_winter <- na.omit(read.csv("data5_v2_winter.csv"))


y <- NULL #set dataframe to fill
for(i in 1:length(darksky_api_start_test$SAMPLE_DATE)){
  fdf <- get_forecast_for(darksky_api_start_test$Latitude[i], darksky_api_start_test$Longitude[i], darksky_api_start_test$SAMPLE_DATE[i], units = "uk", add_headers=TRUE)
  fdf <- as.data.frame(fdf)
  fdf <- fdf[1,]
  y <- rbind.fill(y, fdf)
}
data5 <- na.omit(data5_winter)
#### full data darksky loop ##
#darksky loop #remember to format SAMPLE_DATE2 as yyyy/mm/ddT12:00:000
y <- NULL #set dataframe to fill
for(i in 1:length(data5_winter$SAMPLE_DATE2)){
  fdf <- get_forecast_for(data5_winter$Latitude[i], data5_winter$Longitude[i], data5_winter$SAMPLE_DATE2[i], units = "uk", add_headers=TRUE)
  fdf <- as.data.frame(fdf)
  fdf <- fdf[1,]
  y <- rbind.fill(y, fdf)
}
#combine datasets
data6 <- cbind(y$daily.precipIntensity, data5_winter)
#omit rows containing NAs in daily precipitation intensity
data7 <- data6[!is.na(data6$`y$daily.precipIntensity`),]
#rename precipitation column (average millimeters per hour)
data7$PrecipitationIntensity <- data7$`y$daily.precipIntensity`
data7$SAMPLE_DATE <- as.Date(data7$SAMPLE_DATE,
                             format = "%d-%m-%Y")
data9$SAMPLE_DATE <- as.Date(data9$SAMPLE_DATE,
                              format = "%d/%m/%Y")
data7$`y$daily.precipIntensity` <- NULL
data7$X <- NULL
data9$X.1 <- NULL
data9$X <- NULL
data7$SAMPLE_DATE2 <- NULL
data10 <- rbind(data7, data9)


#plot monthly averages 

monthlyprecip <- aggregate(PrecipitationIntensity ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data7, mean)
monthlyprecip$date <- as.yearmon(paste(monthlyprecip$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlyprecip$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")

lineyear2precip <- ggplot(monthlyprecip, aes(x=date, y=PrecipitationIntensity, group=Land_Type, color=Land_Type)) +
  geom_line()

#plot precipitation averages
lineyear2precip + labs(x = "Date", y = "Average Monthly Precipitation Intensity (mm/h)")
#very high december peak, try to remove

hist(data9_v2$PrecipitationIntensity, breaks=(1000), xlim=c(0,1), ylim=c(0,2000))
res <- hist(data9$SAMPLE_DATE, "months")
res$counts
#should remove december, january and february because all low values to be able to take average 
#subset them out then rbind
#data.march <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="03")
#data.april <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="04")
#data.may <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="05")
#data.june <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="06")
##data.july <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="07")
#data.august <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="08")
#data.september <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="09")
#data.october <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="10")
#data.november <- subset(data7, format(as.Date(SAMPLE_DATE),"%m")=="11")

#UPDATE: DUMB IDEA, don't do this because need to include all months for time series
#data8 <- rbind(data.march, data.april, data.may, data.june, data.july, data.august, data.september, data.october, data.november)
#we go again with the new data...
#plot monthly averages 

monthlyprecip <- aggregate(PrecipitationIntensity ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data8, mean)
monthlyprecip$date <- as.yearmon(paste(monthlyprecip$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlyprecip$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")

lineyear2precip <- ggplot(monthlyprecip, aes(x=date, y=PrecipitationIntensity, group=Land_Type, color=Land_Type)) +
  geom_line()

#plot precipitation averages
lineyear2precip + labs(x = "Date", y = "Average Monthly Precipitation Intensity (mm/h)")

#still massive peak after 2019
#delete 2019

data11 <- data10 %>% filter(SAMPLE_DATE <= "2019-01-01")

#plot again...

monthlyprecip <- aggregate(PrecipitationIntensity ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data10, mean)
monthlyprecip$date <- as.yearmon(paste(monthlyprecip$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlyprecip$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")

lineyear2precip <- ggplot(monthlyprecip, aes(x=date, y=PrecipitationIntensity, group=Land_Type, color=Land_Type)) +
  geom_line()

#plot precipitation averages
lineyear2precip + labs(x = "Date", y = "Average Monthly Precipitation Intensity (mm/h)")

#loess plot for precipitation
lineyear2preciploess <- ggplot((data10), aes(x=SAMPLE_DATE, y=PrecipitationIntensity, group=Land_Type, color=Land_Type)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(x=SAMPLE_DATE, y=PrecipitationIntensity, fill=Land_Type), alpha=0.3) 

lineyear2preciploess + labs(x = "Date", y = "Precipitation Intensity (mm/h)") + theme_bw() + theme(axis.text=element_text(size=12),
                                                                                                 axis.title=element_text(size=14,face="bold"))

#mean monthly precipitation intensity as part of dataset
data9$Year.Month <- format(data9$SAMPLE_DATE, '%Y-%m')
data10 <- data9 %>%
  group_by(Latitude, Longitude, Year.Month) %>%
  summarize(MonthlyPrecipIntensity = mean(PrecipitationIntensity))

data9 <- merge(x = data9, y = data10, by = c("Latitude","Longitude","Year.Month"), all.x = TRUE)



#much better
#save data set

data9 <- read.csv("data9_v2.csv")
data10 <- read.csv("data10.csv")

# format date field
data10$SAMPLE_DATE <- as.Date(data10$SAMPLE_DATE,
                             format = "%Y-%m-%d")
# format date field
data9$SAMPLE_DATE <- as.Date(data9$SAMPLE_DATE,
                              format = "%d/%m/%Y")
#run anova
res.aov1 <- aov(PrecipitationIntensity ~ Land_Type, data = monthlyprecip)
summary(res.aov1)
#save output
out <- capture.output(summary(res.aov1))

cat("Precipitation Anova", out, file="precipitation_anova.txt", sep="n", append=TRUE)


#scatter graph to show taxa sampling
year <- ggplot(data9, aes(x=SAMPLE_DATE, y=BMWP_N_TAXA)) + geom_point( alpha=1, size=1) + 
  (aes(color=Land_Type))
  
year + scale_color_gradient(low="blue", high="red") 



#lowess fit
lineyear2taxa <- ggplot(data10, aes(x=SAMPLE_DATE, y=BMWP_N_TAXA, group=Land_Type, color=Land_Type)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(x=SAMPLE_DATE, y=BMWP_N_TAXA, fill=Land_Type), alpha=0.3)

#lineyear2taxa <- ggplot(na.omit(monthlytaxa), aes(x=date, y=BMWP_N_TAXA, group=Land_Type, color=Land_Type)) +
 # geom_line() +
  #geom_point() +
  #geom_errorbar(aes(ymin=BMWP_N_TAXA-monthlytaxa$sd$BMWP_N_TAXA, ymax=BMWP_N_TAXA+monthlytaxa$sd$BMWP_N_TAXA), width=.1, alpha=0.5,
   #             position=position_dodge(0.05))

lineyear2taxa + labs(x = "Date", y = "Number of Taxa (BMWP)") + theme_bw() + theme(axis.text=element_text(size=12),
                                                                                   axis.title=element_text(size=14,face="bold"))
#linegraph
#monthly mean for anova because large amount of data so p value will be small
monthlytaxa <- aggregate(BMWP_N_TAXA ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data10 , mean )
monthlytaxa$date <- as.yearmon(paste(monthlytaxa$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlytaxa$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")

#make
#anova comparing lowland and highland differences in monthly taxa
res.aov2 <- aov(BMWP_N_TAXA ~ Land_Type, data = monthlytaxa)
summary(res.aov2)

#save output
out <- capture.output(summary(res.aov2))

cat("Monthly Taxa Anova", out, file="monthlytaxa_anova.txt", sep="n", append=TRUE)


#LIFE Index


lineyearLIFE <- ggplot(data10, aes(x=SAMPLE_DATE, y=LIFE_FAMILY_INDEX, group=Land_Type, color=Land_Type)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(x=SAMPLE_DATE, y=LIFE_FAMILY_INDEX, fill=Land_Type), alpha=0.3)

lineyearLIFE + labs(x = "Date", y = "LIFE Family Index Score") + theme_bw() + theme(axis.text=element_text(size=12),
                                                                                    axis.title=element_text(size=14,face="bold"))

#mean of LIFE index for anova
monthlylife <- aggregate(LIFE_FAMILY_INDEX ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data10 , mean )
monthlylife$date <- as.yearmon(paste(monthlylife$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlylife$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")


#anova for life index
res.aov3 <- aov(LIFE_FAMILY_INDEX ~ Land_Type, data = monthlylife)
summary(res.aov3)

#save output
out <- capture.output(summary(res.aov3))

cat("LIFE Anova", out, file="life_anova.txt", sep="n", append=TRUE)


#try mean of APST index
monthlyaspt <- aggregate(BMWP_ASPT ~ format(as.Date(SAMPLE_DATE),"%m") + format(as.Date(SAMPLE_DATE),"%Y") + Land_Type, data = data10 , mean )
monthlyaspt$date <- as.yearmon(paste(monthlyaspt$`format(as.Date(SAMPLE_DATE), "%Y")`, monthlyaspt$`format(as.Date(SAMPLE_DATE), "%m")`), "%Y %m")


lineyearaspt <- ggplot(data10, aes(x=SAMPLE_DATE, y=BMWP_ASPT, group=Land_Type, color=Land_Type)) +
  stat_smooth(method="loess", span=0.1, se=TRUE, aes(x=SAMPLE_DATE, y=BMWP_ASPT, fill=Land_Type), alpha=0.3) 
lineyearaspt + labs(x = "Date", y = "ASPT Value") + theme_bw()+ theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))


#anova for ASPT index
res.aov4 <- aov(BMWP_ASPT ~ Land_Type, data = monthlyaspt)
summary(res.aov4)

#save output
out <- capture.output(summary(res.aov4))

cat("ASPT Anova", out, file="aspt_anova.txt", sep="n", append=TRUE)


# do some boxplots highland vs lowland maybe
#histogram for sampling period

samplinghist <- ggplot(data10, aes(as.POSIXct(data10$SAMPLE_DATE))) + 
  geom_histogram(color="lightblue", fill="lightblue") + theme_bw() +
  scale_x_datetime(breaks = date_breaks("3 months"),
                   labels = date_format("%Y-%b"),
                  limits = c(as.POSIXct("2015-01-01"), 
                           as.POSIXct("2019-01-01")) )

samplinghist + labs(x = "Date", y = "Number of Samples") +theme(axis.text=element_text(size=12),
                                                                axis.title=element_text(size=14,face="bold"))


#make map of UK 

UK <- map_data("world") %>% filter(region=="UK")

#sample plots
samplemap <- ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="grey", alpha=0.3) +
  geom_point( data=data9, aes(x=Longitude, y=Latitude, color=elevation)) +
  theme_minimal() + ylim(50,59) + xlim(-8,3) + coord_map() +
  scale_color_gradient(low="blue", high="red", name = "Elevation (m)") +
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) 
samplemap + labs(x = "Longitude (°)", y = "Latitude (°)") +theme(axis.text=element_text(size=12),
                                                                 axis.title=element_text(size=14,face="bold"))
#### DATA ANALYSIS ####


#make glms measuring taxa


####Compare AIC with precipitation data
#make glms measuring taxa
fit4 <- glm(BMWP_N_TAXA ~ Latitude:Longitude + elevation + PrecipitationIntensity, data=data10, family=poisson())
modelvis1 <- visreg(fit4, "elevation", partial = FALSE, ylab = "f(Number of Taxa)", xlab = "Elevation (m)", gg = TRUE) + ylim(1,3) + theme_bw()
modelvis2 <- visreg(fit4, "PrecipitationIntensity", partial = FALSE, ylab = "f(Number of Taxa)", xlab = "Precipitation Intensity (mm/h)", gg = TRUE) + theme_bw()
modelvis1 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))
modelvis2 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))
summary(fit4)
plot(fit4)
#taxa fit has lowest AIC with lat:long, elevation and precipitation intensity

#AIC was INF (infinity) for LIFE index and ASPT which suggests overfitting. Therefore just do linear models.

#glm for LIFE index
fit5 <- lm(LIFE_FAMILY_INDEX ~ elevation, data=data10)
modelvis3 <- visreg(fit5, "elevation", partial = FALSE, ylab = "LIFE Family Index", xlab = "Elevation (m)", gg = TRUE) + theme_bw()
modelvis3 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))
summary(fit5)

fit6 <-  lm(LIFE_FAMILY_INDEX ~ PrecipitationIntensity, data=data10)
modelvis4 <- visreg(fit6, "PrecipitationIntensity", partial = FALSE, ylab = "LIFE Family Index", xlab = "Precipitation Intensity (mm/h)", gg = TRUE) + theme_bw()
modelvis4 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))
summary(fit6)

#ASPT index
fit7 <- lm(BMWP_ASPT ~ elevation, data=data10)
modelvis5 <- visreg(fit7, "elevation", partial = FALSE, ylab = "ASPT Index", xlab = "Elevation (m)", gg = TRUE) + theme_bw()
modelvis5 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))
summary(fit7)

fit8 <- lm(BMWP_ASPT ~ PrecipitationIntensity, data=data10)
modelvis6 <- visreg(fit8, "PrecipitationIntensity", partial = FALSE, ylab = "ASPT Index", xlab = "Precipitation Intensity (mm/h)", gg = TRUE) + theme_bw()
modelvis6 +theme(axis.text=element_text(size=12),
                 axis.title=element_text(size=14,face="bold"))

summary(fit8)




Dsquared(fit4, adjust = TRUE)


#flow data would fit into these model really nicely


############ BELOW WAS NOT USED, FAILED ATTEMPTS AT GETTING FLOW/RAINFALL DATA ------


#try using rnrfa package to retrieve flow data

#library(rnrfa)

#allstations <- catalogue() #get all site IDs from API

#extract distinct site_IDS

#sitesunique <- unique(data5$SITE_ID)
#length(sitesunique)
#sitesunique
#they use different site_IDs to my data (I think)
#try merging to make sure
#sitecheck <-  merge(data5, allstations, by.x=c("SITE_ID"),by.y=c("id"))
#sitenumbercheck <- unique(sitecheck$SITE_ID)
#rearrange columns to see if lat and longs kind of match
#sitescheck2 <- sitecheck[,c(1,2,3, 127,4:126, 128:130)]
#NOT ACCURATE

#ATTEMPT BELOW DOES NOT WORK
#try to align by lat and long
allstations <- catalogue() #get all site IDs from API

#rename latitue column
allstations$Latitude <- allstations$latitude
allstations$Longitude <- allstations$longitude #do same for longitude

allstations$latitude <- NULL
allstations$longitude <- NULL
#merge based on latitude
# sites <- merge(data5, allstations, by = "Latitude", all = FALSE)

#adjust order of longitude column
sites2 <- sites[,c(1,125,2:124)]

#not many merges
#merge based on grid reference
#rename grid reference column
allstations$NGR_10_FIG <- allstations$`grid-reference`$ngr
# sites <- merge(data5, allstations, by = "NGR_10_FIG", all = FALSE)
#still doesn't merge. limit grid reference to 6 figs

allstations$NGR_6_FIG <- ifelse(nchar(allstations$NGR_10_FIG) > 8, paste0(substring(allstations$NGR_10_FIG, 1, 8), ""), allstations$NGR_10_FIG)
data5$NGR_10_FIG <- as.character(data5$NGR_10_FIG) # do same for data5 but convert to character
data5$NGR_6_FIG <- ifelse(nchar(data5$NGR_10_FIG) > 8, paste0(substring(data5$NGR_10_FIG, 1, 8), ""), data5$NGR_10_FIG)
#merge based on NGR_6_FIG
sites <- merge(data5, allstations, by = "NGR_6_FIG", all = FALSE)

#try reducing figures again cause didn't merge enough
allstations$NGR_4_FIG <- ifelse(nchar(allstations$NGR_10_FIG) > 6, paste0(substring(allstations$NGR_10_FIG, 1, 6), ""), allstations$NGR_10_FIG)
data5$NGR_4_FIG <- ifelse(nchar(data5$NGR_10_FIG) > 6, paste0(substring(data5$NGR_10_FIG, 1, 6), ""), data5$NGR_10_FIG)
sites <- merge(data5, allstations, by = "NGR_4_FIG", all = FALSE)
sitenumbercheck <- unique(sites$SITE_ID)




sites <- data5$SITE_ID


info <- gdf(3005)

info$data

info3 <- gdf(sites2)
sites2 <- paste0("(", min(1), ":", max(5595), ")")






#-------------------------------------------------------------------------------------------------------
#Load the required libraries

library(rvest)
library(plyr)
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_45")
#install.packages('rJava')
#install.packages("xlsx")
library(rJava)
library(xlsx)
library(dplyr)
library(ggmap)
library(ggplot2)
library(gridExtra)

#-------------------------------------------------------------------------------------------------------

# *********** Load the hazardoes air pollutants: *****************************************************

haps.url <- "https://ofmext.epa.gov/AQDMRS/ws/list?name=param&pc=CORE_HAPS&resource=rawData"

haps.raw.data <- html(haps.url) %>%
  html_node("body > p") %>%
  html_text()

#Load the hazardous air pollutants into a data frame.
haps.data <- read.table(text=haps.raw.data, sep="\t",  col.names = c("Parameter.Code", "Parameter.Name"))
dim(haps.data)
View(haps.data)

#-------------------------------------------------------------------------------------------------------

# *********** Load the Annual pollution data for the years: 2009 to 2014 *******************************

GetAnnualPollutionData <- function(year) {
  
  #Prepare a full url by appending the year
  full_url <- paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_", year, ".zip", sep = '')
  
  
  #Download the file & get the data into a data frame.
  temp <- tempfile()
  download.file(full_url, temp)
  
  #Read the data into a data frame.
  data <- read.table(file = unz(temp, paste("annual_all_", year, ".csv", sep = '')), header = TRUE, sep = ",")
  
  #unlink the file handle
  unlink(temp)
  
  #putting a pause in between page reads, so this call is not treated as a denial of service attack.
  Sys.sleep(1)
  
  #return the dataframe
  return (data)
}


#Call the function 'GetAnnualPollutionData' to load the annual polllution data between 2009 to 2014
annual.pollution.data <- ldply(2009:2014, GetAnnualPollutionData)
dim(annual.pollution.data)
View(annual.pollution.data)

# Join haps.data with annual.pollution.data on Parameter.Code
data.merge<-merge(x = annual.pollution.data, y = haps.data, by = "Parameter.Code")
dim(data.merge)

#Pollution Data for the year 2014

data.tidy<-data.merge %>%
  select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Event.Type,Arithmetic.Mean) %>%
      filter(Event.Type  == "No Events" & Year ==2014  ) %>%
    arrange((Arithmetic.Mean)) 
#View(data.tidy)


#-------------------------------------------------------------------------------------------------------
#Analysis of PM2.5 Particles in 2014
haps.unique.PM <-data.tidy[grep(" PM2.5", data.tidy$Parameter.Name.x), ]
View(haps.unique.PM)

data.tidy.rmOutlier<-haps.unique.PM %>%
  select(Year,State.Name, City.Name ,Parameter.Name.x,Latitude,Longitude, Arithmetic.Mean ) %>%
  filter( Arithmetic.Mean <0.025 & Arithmetic.Mean > 0.001) %>%
  arrange((Arithmetic.Mean))
#View dataframe in spreadsheet form
#fix(data.tidy.rmOutlier)


map <- get_map(location='united states',zoom=4,maptype="toner-background",
               source='google',color='color')

ggmap(map,extent = 'normal', maprange = TRUE) + geom_point(
  aes(x=Longitude,y=Latitude,show_guide= TRUE, colour=Arithmetic.Mean),
  data.tidy.rmOutlier , alpha=0.9, na.rm= T) + scale_color_gradient(low="blue",high="red")


#-------------------------------------------------------------------------------------------------------

#Find and Remove outliers
##summary(haps.unique.PM )
##hist(haps.unique.PM$Arithmetic.Mean)




#-------------------------------------------------------------------------------------------------------

#Analysis of Lead Particles
haps.unique.PM <-data.tidy[grep("Lead", data.tidy$Parameter.Name.x), ]
View(haps.unique.PM)

data.tidy.rmOutlier<-haps.unique.PM %>%
  select(Year,State.Name, City.Name ,Parameter.Name.x,Latitude,Longitude, Arithmetic.Mean ) %>%
  filter( Arithmetic.Mean <0.015 & Arithmetic.Mean > 0.001) %>%
  arrange((Arithmetic.Mean))



map <- get_map(location='united states',zoom=4,maptype="toner-background",
               source='google',color='color')

ggmap(map,extent = 'normal', maprange = TRUE) + geom_point(
  aes(x=Longitude,y=Latitude,show_guide= TRUE, colour=Arithmetic.Mean),
  data.tidy.rmOutlier , alpha=0.9, na.rm= T) + scale_color_gradient(low="blue",high="red")


#-------------------------------------------------------------------------------------------------------


#-------------------------------------------------------------------------------------------------------
#*********Display State wise analysis for PM25 particle 2009-2014

# year wise changes in pm25 particles in Selected states.



#Analysis of PM2.5 Particles
haps.unique.particle <-data.merge[grep(" PM2.5", data.merge$Parameter.Name.x), ]

data.tidy.California<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="California" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))
data.tidy.Texas<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="Texas" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))
data.tidy.NewYork<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="New York" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))
data.tidy.pennsylvania<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="Pennsylvania" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))
data.tidy.Alabama<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="Alabama" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))
data.tidy.Hawaii<-haps.unique.particle %>%
  select(Year,State.Name, County.Name,Event.Type,Arithmetic.Mean) %>%
  filter(Event.Type  == "No Events" & State.Name=="Hawaii" & Arithmetic.Mean > 0.0011  ) %>%
  arrange((Arithmetic.Mean))




P.California <- ggplot(data=data.tidy.California, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("California") +
  geom_point()
P.Texas <- ggplot(data=data.tidy.Texas, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("Texas")+
  geom_point()
P.NewYork <- ggplot(data=data.tidy.NewYork, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("New York")+
  geom_point()
P.pennsylvania <- ggplot(data=data.tidy.pennsylvania, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("Pennsylvania")+
  geom_point()
P.Alabama <- ggplot(data=data.tidy.Alabama, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("Alabama")+
  geom_point()
P.Hawaii <- ggplot(data=data.tidy.Hawaii, aes(x=Year, y=Arithmetic.Mean, group=County.Name, colour=County.Name)) +
  geom_line() +ggtitle("Hawaii")+
  geom_point()

grid.arrange(P.California,P.Texas,P.pennsylvania,P.NewYork,P.Alabama,P.Hawaii)


#-------------------------------------------------------------------------------------------------------


# *********** Load the Annual pollution data for the years: 2009 to 2014 *******************************

GetDailyPollutionData <- function(year) {
  
  #Prepare a full url by appending the year
  full_url_daily_pm2.5 <- paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/daily_88502_", year, ".zip", sep = '')
  
  
  #Download the file & get the data into a data frame.
  temp <- tempfile()
  download.file(full_url_daily_pm2.5, temp)
  
  #Read the data into a data frame.
  data <- read.table(file = unz(temp, paste("daily_88502_", year, ".csv", sep = '')), header = TRUE, sep = ",")
  
  #unlink the file handle
  unlink(temp)
  
  #putting a pause in between page reads, so this call is not treated as a denial of service attack.
  Sys.sleep(1)
  
  #return the dataframe
  return (data)
}
#Call the function 'GetAnnualPollutionData' to load the annual polllution data between 2009 to 2014
daily.pollution.data <- ldply(2011:2014, GetDailyPollutionData)
dim(daily.pollution.data)
str(daily.pollution.data)
View(daily.pollution.data)
library(xts)
library(lubridate)
require(reshape2)
library(plyr)
library(dplyr)

#Plotting Time Series
#We use function xts, available in the package xts to plot time series.
generateTSAnalysis <-function(year)
{
 
  data.pm25<-daily.pollution.data %>%
    select(Date.Local,State.Name, County.Name ,Arithmetic.Mean) %>%
    filter(year(Date.Local) ==year & County.Name=="Queens" ) %>%
    arrange((Date.Local))
  
  data.pm25$DATE <- as.Date(data.pm25$Date.Local)
   pm25.TS <- xts(x=data.pm25$Arithmetic.Mean,order.by=data.pm25$DATE)
  return( pm25.TS)
  # sub.title= paste("Queens", year, sep="-")
  #  plot(pm25.TS,main="pm2.5 Data",sub=sub.title)
  
}


pm25.TS.2012 <- generateTSAnalysis(2012)
pm25.TS.2013 <- generateTSAnalysis(2013)
pm25.TS.2014 <- generateTSAnalysis(2014)
plot(pm25.TS.2012,main="pm2.5 Data",sub="Queens-2012")
plot(pm25.TS.2013,main="pm2.5 Data",sub="Queens-2013")
plot(pm25.TS.2014,main="pm2.5 Data",sub="Queens-2014")

#Decomposing a time series means separating it into its constituent components, 
#which are usually a trend component and an irregular component, and if it is a seasonal time series, 
#a seasonal component.

  data.pm25<-daily.pollution.data %>%
    select(Date.Local,State.Name, County.Name ,Arithmetic.Mean) %>%
    filter( County.Name=="Queens" ) %>%
    arrange((Date.Local))
  
  
  data.pm25TS<- ddply(data.pm25, .(Date.Local), subset, 
                      subset = Arithmetic.Mean == max(Arithmetic.Mean), select = c(Date.Local, Arithmetic.Mean) )
  
  #data.pm25$DATE <- as.Date(data.pm25TS$Date.Local)
  pm25.TS <- ts(c(data.pm25TS$Arithmetic.Mean),frequency=365,start=c(2011,1))
  pm25.TS.components <- decompose(pm25.TS)
  plot( pm25.TS.components)

  


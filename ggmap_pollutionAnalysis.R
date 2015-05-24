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

data.tidy<-data.merge %>%
  select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Event.Type,Arithmetic.Mean) %>%
      filter(Event.Type  == "No Events" & Year ==2014  ) %>%
    arrange((Arithmetic.Mean)) 
#View(data.tidy)

#Analysis of PM2.5 Particles
haps.unique.PM <-data.tidy[grep(" PM2.5", data.tidy$Parameter.Name.x), ]
View(haps.unique.PM)

#Analysis of Lead Particles
data.tidy.rmOutlier<-haps.unique.PM %>%
  select(Year,State.Name, City.Name ,Parameter.Name.x,Latitude,Longitude, Arithmetic.Mean ) %>%
  filter( Arithmetic.Mean <0.025 & Arithmetic.Mean > 0.001) %>%
  arrange((Arithmetic.Mean))
#View dataframe in spreadsheet form
fix(data.tidy.rmOutlier)


map <- get_map(location='united states',zoom=4,maptype="toner-background",
               source='google',color='color')

ggmap(map,extent = 'normal', maprange = TRUE) + geom_point(
  aes(x=Longitude,y=Latitude,show_guide= TRUE, colour=Arithmetic.Mean),
  data.tidy.rmOutlier , alpha=0.9, na.rm= T) + scale_color_gradient(low="blue",high="red")



#Find and Remove outliers
##summary(haps.unique.PM )
##hist(haps.unique.PM$Arithmetic.Mean)





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








#-------------------------------------------------------------------------------------------------------
#Load the required libraries

library(rvest)
library(plyr)
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_45")
install.packages('rJava')
install.packages("xlsx")
library(rJava)
library(xlsx)
library(knitr)

#-------------------------------------------------------------------------------------------------------

# *********** Load the hazardoes air pollutants: *****************************************************

haps.url <- "https://ofmext.epa.gov/AQDMRS/ws/list?name=param&pc=CORE_HAPS&resource=rawData"

haps.raw.data <- html(haps.url) %>%
  html_node("body > p") %>%
  html_text()

#Load the hazardous air pollutants into a data frame.
haps.data <- read.table(text=haps.raw.data, sep="\t",  col.names = c("Parameter.Code", "Parameter.Name"))

dim(haps.data)
kable(head(haps.data))
#View(haps.data)

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
kable(head(annual.pollution.data))



#Added by Sreejaya
#********************************************************************************************************

#Tidying and Transforming

library(dplyr)

#********************************************************************************************************

colnames(annual.pollution.data)

# Join haps.data with annual.pollution.data on Parameter.Code
#This will give us only the hazardous polluants data ONLY across the country.
data.merge <- merge(x = annual.pollution.data, y = haps.data, by = "Parameter.Code")
colnames(data.merge)
dim(data.merge)


data.tidy <- data.merge %>%
                 select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Event.Type,Arithmetic.Mean ) %>%
                     filter(Event.Type  == "No Events") %>%
                          arrange(desc(Year), State.Name)

kable(head(data.tidy))

#-----------------------------
# proxy data - Just take 3 cities and Compare.
#-----------------------------
data.tidy.yearCity <- data.merge %>%
    select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Event.Type,Arithmetic.Mean ) %>%
          filter(Event.Type  == "No Events" & City.Name == "Chattanooga"| City.Name =="Los Angeles" | City.Name =="New York") %>%
                arrange(desc(Year), State.Name)

kable(head(data.tidy.yearCity))


# Summarise data - group by city to get the ANNUAL mean pollution in that city
datatidy.pollutant <- data.tidy %>%
            select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Arithmetic.Mean ) %>%
                group_by(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x) %>%
                      summarise_each(funs(mean(Arithmetic.Mean)))

dim(datatidy.pollutant)
#View(datatidy.pollutant)
kable(head(datatidy.pollutant))

#-----------------------------
#Those less than 10 micrometers in diameter (PM10) are so small that they can get into the lungs, 
#potentially causing serious health problems. Ten micrometers is smaller than the width of a single human hair. 
#Fine particles (PM2.5).
#Filter out small particles
#-----------------------------
haps.unique.PM <-datatidy.pollutant[grep(" PM", datatidy.pollutant$Parameter.Name.x), ]
print(haps.unique.PM)



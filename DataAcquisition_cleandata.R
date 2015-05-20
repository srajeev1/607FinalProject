#-------------------------------------------------------------------------------------------------------
#Load the required libraries

library(rvest)
library(plyr)
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_45")
install.packages('rJava')
#install.packages("xlsx")
library(rJava)
library(xlsx)

#-------------------------------------------------------------------------------------------------------

# *********** Load the hazardoes air pollutants: *****************************************************

haps.url <- "https://ofmext.epa.gov/AQDMRS/ws/list?name=param&pc=CORE_HAPS&resource=rawData"

haps.raw.data <- html(haps.url) %>%
  html_node("body > p") %>%
  html_text()

#Load the hazardous air pollutants into a data frame.
haps.data <- read.table(text=haps.raw.data, sep="\t",  col.names = c("Parameter.Code", "Parameter.Name"))
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







#Added by Sreejaya
colnames(annual.pollution.data)

# Join haps.data with annual.pollution.data on Parameter.Code
data.merge<-merge(x = annual.pollution.data, y = haps.data, by = "Parameter.Code")
colnames(data.merge)

# Keep only  needed data
data.colSlice <-data.merge[,c("Year","State.Name", "County.Name","City.Name" ,"Parameter.Name.x","Latitude","Longitude",
                              "Pollutant.Standard",  "Event.Type" , "Arithmetic.Mean" )]


# Filter out event types like fire and all
annual.pollution.data.subset  <- subset(data.colSlice, Event.Type == "No Events")
head(annual.pollution.data.subset)

# reshape the data for modeling







#-------------------------------------------------------------------------------------------------------

# ********** Load the National Health Expenditure Summary Data *****************************************

# National health expenditure summary data
url.nhe <- "http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/NationalHealthExpendData/Downloads/NHEGDP13.zip"

#Download the file & get the data into a data frame.
td <- tempdir()
tf <- tempfile(tmpdir=td, fileext=".zip") 
download.file(url.nhe, tf)


#Read the data into a data frame.
nhe.data <- read.xlsx(file = unzip(tf, "NHE13_Summary_Final.xls"), header = FALSE, startRow=2, rowIndex = c(2,3,9,10,11), colIndex = c(1:55), sheetIndex = 1 )

#unlink the file handle
unlink(temp)


#Make rows as columns and columns as rows
nhe.data.final <- t(nhe.data)

View(nhe.data.final)

#Nullify row names - not needed.
rownames(nhe.data.final) <- NULL

#Make the row1 as the column names
colnames(nhe.data.final) <- nhe.data.final[1,]

#We do not need row1 now.
nhe.data.final = nhe.data.final[-1,]

#change the column 1 name to "Year"
colnames(nhe.data.final)[1] <- "Year"

#convert the year column to numeric
nhe.data.final[, 1] <- as.numeric(nhe.data.final[,1])

#View the final data
View(nhe.data.final)

#-------------------------------------------------------------------------------------------------------









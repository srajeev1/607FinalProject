#Load the required libraries
library(rvest)
library(plyr)


# Load the hazardoes air pollutants:
haps.url <- "https://ofmext.epa.gov/AQDMRS/ws/list?name=param&pc=CORE_HAPS&resource=rawData"

haps.raw.data <- html(haps.url) %>%
  html_node("body > p") %>%
  html_text()

haps.data <- read.table(text=haps.raw.data, sep="\t",  col.names = c("Parameter.Code", "Parameter.Name"))
View(haps.data)



GetAnnualData <- function(year) {
  #prepare a full url by appending the year
  full_url <- paste("http://aqsdr1.epa.gov/aqsweb/aqstmp/airdata/annual_all_", year, ".zip", sep = '')
  full_url
  #Download the file & get the data into a data frame.
  temp <- tempfile()
  download.file(full_url, temp)
  data <- read.table(file = unz(temp, paste("annual_all_", year, ".csv", sep = '')), header = TRUE, sep = ",")
  unlink(temp)
  return (data)
}

data <- ldply(2013:2014, GetAnnualData)
dim(data)
View(data)



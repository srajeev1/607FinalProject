#-------------------------------------------------------------------------------------------------------
#Load the required libraries

library(rvest)
library(plyr)
Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_45")
#install.packages('rJava')
#install.packages("xlsx")
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

#-------------------------------------------------------------------------------------------------------

# Join haps.data with annual.pollution.data on Parameter.Code
#This will give us only the hazardous polluants data ONLY across the country.
data.merge <- merge(x = annual.pollution.data, y = haps.data, by = "Parameter.Code")
colnames(data.merge)
dim(data.merge)

library(dplyr)

data.tidy <- data.merge %>%
  select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Event.Type,Arithmetic.Mean ) %>%
  filter(Event.Type  == "No Events") %>%
  arrange(desc(Year), State.Name)

kable(head(data.tidy))

#-----------------------------
# proxy data - Just take 1 Pollutant - Arsenic PM2.5 LC - for each year
#-----------------------------

# Summarise data - group by city to get the ANNUAL mean pollution in that city
datatidy.pollutant <- data.tidy %>%
  select(Year,State.Name, City.Name ,Parameter.Code,Parameter.Name.x,Latitude,Longitude, Arithmetic.Mean ) %>%
  filter(Parameter.Name.x=="Arsenic PM2.5 LC") %>%
  group_by(Year, Parameter.Name.x) %>%
  summarise_each(funs(mean(Arithmetic.Mean)))



dim(datatidy.pollutant)
View(datatidy.pollutant)
kable(head(datatidy.pollutant))



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

colnames(nhe.data.final)

#--------------------------------------------------------------------------------------
# Regression Analysis -  NHE, GDP, and a sample pollutant Data
#--------------------------------------------------------------------------------------


library(dplyr)
library(reshape2)
library(ggplot2)

nhe.data.df <-  as.data.frame(nhe.data.final)


colnames(nhe.data.df) <- c('Year', 'NHE_In_Billions', 'Population_In_Millions', 'GDP_In_Billions', 'NHE_PerCapitaAmt')

View(nhe.data.df)

nhe.data.df$Year <- as.numeric(as.character(nhe.data.df$Year))
nhe.data.df$NHE_In_Billions <- as.numeric(as.character(nhe.data.df$NHE_In_Billions))
nhe.data.df$Population_In_Millions <- as.numeric(as.character(nhe.data.df$Population_In_Millions))
nhe.data.df$GDP_In_Billions <- as.numeric(as.character(nhe.data.df$GDP_In_Billions))
nhe.data.df$NHE_PerCapitaAmt <- as.numeric(as.character(nhe.data.df$NHE_PerCapitaAmt))

ggplot(nhe.data.df, aes(x = Year, y = NHE_In_Billions)) + geom_point() + geom_smooth(method="lm") + 
  labs(x = "Year", y = "NHE in Billions", title = "National Health Expenditure (1960-2014) ") + theme(plot.title = element_text(size=20, face="bold", vjust=2))


#Just filter the 2000 to 2014 data for NHE and GDP
nhe.gdp.data <- select(nhe.data.df, Year, NHE_In_Billions, GDP_In_Billions) %>% 
                    filter(Year >= 2000)

#Reshapre into Long format
nhe.gdp.data.long <- melt(nhe.gdp.data, id.vars = c("Year"), variable.name = "Category", value.name = "Amount")


ggplot(nhe.gdp.data.long, aes(x = Year, y = Amount, color=Category, fill=Category)) + geom_point() + geom_smooth(method="lm") + 
  labs(title = "National Health Expenditure Vs GDP") + theme(plot.title = element_text(size=20, face="bold", vjust=2))

p1 <- ggplot(nhe.gdp.data.long, aes(x = Year, y = Amount, fill=Category)) + geom_bar(stat="identity", position="dodge", colour="black") + 
  geom_line() + geom_point() + geom_smooth(method="lm") + labs( y = "Amount in Billions", title = "National Health Expenditure Vs GDP") +  theme(plot.title = element_text(size=20, face="bold", vjust=2))


#Add a new calculated columen to the data frame:
nhe.gdp.data <- mutate(nhe.gdp.data, nhe.percentage = (NHE_In_Billions / GDP_In_Billions) * 100)

p2 <- ggplot(nhe.gdp.data, aes(x = Year, y = nhe.percentage)) + geom_point() + geom_line() + geom_smooth(method="lm") + labs( y = "NHE Percentage In GDP", title = "National Health Expenditure as % of  GDP") + theme(plot.title = element_text(size=20, face="bold", vjust=2))

#install.packages("gridExtra")
library(grid)
library(gridExtra)
library(ggplot2)
grid.arrange(p1, p2, nrow=2)



#Based on the trend line from this sample data, there appears to be a strong positive linear relationship between the GDP and NHE variables.

ggplot(datatidy.pollutant, aes(x = Year, y = Arithmetic.Mean)) + geom_point() + geom_smooth(method="lm") 
  labs(x = "Year", y = "NHE in Billions", title = "Pollutant - Arsenic PM2.5 LC")


library(dplyr)
nhe.data.reg <- select(nhe.data.df, Year, NHE_In_Billions, GDP_In_Billions) %>% 
  filter(Year >= 2008)

Y1 <- nhe.data.reg$NHE_In_Billions
X1 <- datatidy.pollutant$Arithmetic.Mean
frame <- as.data.frame(cbind(Y1,X1))

ggplot(frame, aes(x = X1, y = Y1)) + geom_point() + geom_smooth(method="lm") + 
  labs(x = "Pollutant Measure", y = "NHE in Billions", title = "Pollutant (Arsenic PM2.5) Vs NHE") + geom_point() + geom_line() + theme(plot.title = element_text(size=20, face="bold", vjust=2))

#Based on the trend line from this ~limited~ sample data, there appears to be a weak positive linear relationship between the pollutant and NHE variables.
#This relationship may be coincidental.Need more analyses with combination of HAPS (pollutants) to gain insights.

#-------------------------------------------------------------------------------------------------------





Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jre1.8.0_45")
install.packages('rJava')
install.packages("xlsx")
library(rJava)
library(xlsx)


url.nhe <- "http://www.cms.gov/Research-Statistics-Data-and-Systems/Statistics-Trends-and-Reports/NationalHealthExpendData/Downloads/NHE2013.zip" 

td <- tempdir()
tf <- tempfile(fileext=".zip")
download.file(url.nhe, tf)


#Read the data into a data frame
nhe.data <- read.xlsx(file=unzip(tf, "NHE2013.xls"), header=FALSE, sheetIndex=1, startRow=2, rowIndex=c(2,4,6,38,32,35,103,133,223,253,283,313,343,373,403,493), colIndex=c(1:55))

#unlink the file handle
unlink(tf)

#Make rows as colums and columns as rows

nhe.data.final <-t(nhe.data)
View(nhe.data.final)

#Nullify row names
rownames(nhe.data.final) <- NULL

#Make the row 1 as the column names

colnames(nhe.data.final) <- nhe.data.final[1,]

#delete the new row 1
nhe.data.final = nhe.data.final[-1,]

#Change the column 1 name to "year"

colnames(nhe.data.final)[1] <- "Year"

#Convert the year column to numeric
nhe.data.final[,1] <- as.numeric(nhe.data.final[,1])

View(nhe.data.final)



summary(nhe.data.final)

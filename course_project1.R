setwd("C:/Users/KOLeary/Documents/GitHub/Exploratory-Data-Analysis-Course-Project-2")

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
download.file(fileUrl, destfile = "./exdata-data-NEI_data.zip")

unzip("./exdata-data-NEI_data.zip") ##unzip file in working directory

emissdata <- readRDS("summarySCC_PM25.rds")
classcode <- readRDS("Source_Classification_Code.rds")

total <- data.frame(tapply(emissdata$Emissions, emissdata$year, sum)) ##sums emissions column by year factor

colnames(total) <- "Total" ##change column name to 'Total'

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.

hist(total$Total, main = "Global Active Power", xlab = "Global Active Power (kilowatts)", col = "red")

with(total, plot(Total, ylab = "Global Active Power (kilowatts)", xlab = "", type="l")) ##

plot(total)

str(total)
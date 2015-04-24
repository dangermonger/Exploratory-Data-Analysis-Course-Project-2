setwd("C:/Users/.../Exploratory-Data-Analysis-Course-Project-2")

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.

if (!file.exists("exdata-data-NEI_data.zip")) { ##check that file doesn't already exist
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" ##identify url of file
  download.file(fileUrl, destfile = "./exdata-data-NEI_data.zip") ##download file
  unzip("./exdata-data-NEI_data.zip") ##unzip file in working directory
}

emissdata <- readRDS("summarySCC_PM25.rds") ##assign handle and read emmission data
classcode <- readRDS("Source_Classification_Code.rds") ##assign handle and read classification codes

png(file="plot2.png", bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510

total <- data.frame("Total" = tapply(baltdata$Emissions, baltdata$year, sum)) ##sum emissions column by year factor and then name column

total <- cbind(Years = rownames(total), total, stringsAsFactors=FALSE) ##convert row names to column without converting to factor

rownames(total) <- NULL ## remove row names

with(total, plot(Years, Total, main = "PM.25 Emissions Baltimore 1999 - 2008", ylab = "Total Emmisions (tons)", xlab = "Years", type="l")) ##plot Years and Total, name labels and title, line style

dev.off() ##close png
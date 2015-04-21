## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

## 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

## 5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

## 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?






setwd("C:/Users/KOLeary/Documents/GitHub/Exploratory-Data-Analysis-Course-Project-2")

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.

fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" 
download.file(fileUrl, destfile = "./exdata-data-NEI_data.zip")

unzip("./exdata-data-NEI_data.zip") ##unzip file in working directory

emissdata <- readRDS("summarySCC_PM25.rds") ##assign handle to emmission data
classcode <- readRDS("Source_Classification_Code.rds") ##assign handle to classification codes

################################################Plot 1######################################

png(file="plot1.png", bg="transparent") ##open png file

total <- data.frame("Total" = tapply(emissdata$Emissions, emissdata$year, sum)) ##sum emissions column by year factor and then name column

total <- cbind(Years = rownames(total), total, stringsAsFactors=FALSE) ##convert row names to column without converting to factor

rownames(total) <- NULL ## remove row names

with(total, plot(Years, Total, main = "PM.25 Emissions US 1999 - 2008", ylab = "Total Emmisions (tons)", xlab = "Years", type="l")) ##plot Years and Total, name labels and title, line style 

dev.off() ##close png

################################################Plot 2######################################

png(file="plot2.png", bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510

c ##sum emissions column by year factor and then name column

total <- cbind(Years = rownames(total), total, stringsAsFactors=FALSE) ##convert row names to column without converting to factor

rownames(total) <- NULL ## remove row names

with(total, plot(Years, Total, main = "PM.25 Emissions Baltimore 1999 - 2008", ylab = "Total Emmisions (tons)", xlab = "Years", type="l")) ##plot Years and Total, name labels and title, line style

dev.off() ##close png

################################################Plot 3######################################

library(ggplot2)

png(file="plot3.png", bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510

total <- data.frame(with(baltdata, tapply(Emissions, list(year, type), sum))) ##checking between these two

total <- cbind(Years = rownames(total), total) ##convert row names to column

rownames(total) <- NULL ## remove row names

total$Years <- factor(total$Years)



qplot(Year,  Emissions,  data	=	total,	facets	=	.~type)

str(total)


baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510 (Baltimore)


baltdata$year <- factor(baltdata$year) ##convert year to factor

baltdata$type <- factor(baltdata$type) ##convert type to factor

str(baltdata)

qplot(year,  Emissions,	data	=	baltdata,	facets	=	.~type)



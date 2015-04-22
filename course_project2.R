## 1. Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## 2. Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

## 3. Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot answer this question.

## 4. Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

## 5. How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

## 6. Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?






setwd("C:/Users/KOLeary/Documents/GitHub/Exploratory-Data-Analysis-Course-Project-2")

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.
library(ggplot2)

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

png(file="plot3.png", bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510 (Baltimore)

library(dplyr) ##bring up dplr library

convertbl <- tbl_df(baltdata) ## convert table to tbl
rm("baltdata") ## remove other handle
convertbl ##print converted table

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
 group_by(type, year)%>% ##group by type and year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
 summarise(Emissions = sum(Emissions)) ##summarise the grouped data in columns and name and sum emmissions

qplot(year,  Emissions,  data	=	sumtable,	facets	=	.~type, geom	=	c("point",  "smooth"), method  =	"gam") + ##plot Year by Emissions, seperate by type, point, smooth and general additive model (gam)
  labs(x = "Years", y = "Total Emissions (tons)", title = "Total Emissions by Source Type, Baltimore 1999 - 2008")

dev.off() ##close png

################################################Plot 4######################################

png(file="plot4.png", bg="transparent") ##open png file

classcode$SCC <- as.numeric(as.character(classcode$SCC)) ##convert SCC column from factor to number while preserving content

listloc <- grep("*.oal.*", classcode$EI.Sector) ##identify the locations of strings containing "oal" in EI.Sector column

subclass <- classcode[listloc, ] ##subset classcode by list locations

subcol <- subclass[,1] ##subset the first column of resultant data, SCC

emissmatch <- emissdata[which(emissdata$SCC %in% subcol),] ##subset emissdata where SCC matches subcol contents

convertbl <- tbl_df(emissmatch) ## convert table to tbl
rm("emissmatch") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(year)%>% ##group by year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)/1000) ##summarise the grouped data in columns and name and sum emmissions. Divide for kilotons

qplot(year, Emissions, data = sumtable, stat = "identity", geom = "bar", fill = year) + ##plot Year by Emissions, bar chart and stat identity to stop data being binned
  labs(x = "Years", y = "Total Emissions (kilotons)", title = "Total Emissions in US by Coal Combustion 1999 - 2008") ##assign labels

dev.off() ##close png

################################################Plot 5######################################

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510 (Baltimore)

classcode$SCC <- as.numeric(as.character(classcode$SCC)) ##convert SCC column from factor to number while preserving content

listloc <- grep("Onroad", classcode$Data.Category) ##identify the locations of strings containing "oal" in EI.Sector column

subclass <- classcode[listloc, ] ##subset classcode by list locations

subcol <- subclass[,1] ##subset the first column of resultant data, SCC

emissmatch <- emissdata[which(emissdata$SCC %in% subcol),] ##sub

convertbl <- tbl_df(emissmatch) ## convert table to tbl
rm("emissmatch") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(year)%>% ##group by year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)) ##summarise the grouped data in columns and name and sum emmissions. Divide for kilotons


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
library(dplyr) ##bring up dplr library

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

emissmatch$year <- factor(emissmatch$year) ##convert year column to factor

convertbl <- tbl_df(emissmatch) ## convert table to tbl
rm("emissmatch") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(year)%>% ##group by year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)/1000) ##summarise the grouped data in columns and name and sum emmissions. Divide for kilotons


qplot(year, Emissions, data = sumtable, stat = "identity", geom = "bar", fill = year) + ##plot Year by Emissions, bar chart and stat identity to stop data being binned
  labs(x = "Years", y = "Total Emissions (kilotons)", title = "Total Emissions in US by Coal Combustion 1999 - 2008") ##assign labels


dev.off() ##close png

################################################Plot 5######################################

png(file="plot5.png", bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510 (Baltimore)

baltonroad <- baltdata[which(baltdata$type == "ON-ROAD"),] ##subset baltdata by rows where type is equal to ON-ROAD to filter by motor vehicles

baltonroad$year <- factor(baltonroad$year) ##convert year column to a factor vector

convertbl <- tbl_df(baltonroad) ## convert table to tbl
rm("baltonroad") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(year)%>% ##group by year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)) ##summarise the grouped data in columns and name and sum emmissions. 

qplot(year, Emissions, data = sumtable, stat = "identity", geom = "bar", fill = year) + ##plot Year by Emissions, bar chart and stat identity to stop data being binned
  labs(x = "Years", y = "Total Emissions (tons)", title = "Motor Vehicle Emissions in Baltimore City 1999 - 2008") ## add labels

dev.off() ##close png

################################################Plot 6######################################

bala <- subset(emissdata, emissdata$fips %in% c("24510","06037"))##subset data by rows where fips is equal to 24510 (Baltimore) and LA 06037

balaonroad <- bala[which(bala$type == "ON-ROAD"),] ##subset baltdata by rows where type is equal to ON-ROAD to filter by motor vehicles

balaonroad$year <- factor(balaonroad$year) ##convert year column to a factor vector

balaonroad$fips <- factor(balaonroad$fips) ##convert fips column to a factor vector

levels(balaonroad$fips) <- c("Los Angeles", "Baltimore")

convertbl <- tbl_df(balaonroad) ## convert table to tbl
rm("balaonroad") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(fips, year)%>% ##group by year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)) ##summarise the grouped data in columns and name and sum emmissions then..


qplot(year, Emissions, data = sumtable, facets  =	.~fips, stat = "identity", geom = "bar", fill = year) + ##plot Year by Emissions, bar chart and stat identity to stop data being binned
  labs(x = "Years", y = "Total Emissions (tons)", title = "Motor Vehicle Emissions in Los Angeles and Baltimore Cities 1999 - 2008") ## add labels

qplot(year, Emissions, data = sumtable, facets  =  .~fips, stat_smooth(method = "lm"), geom_point() ) + ##plot Year by Emissions, bar chart and stat identity to stop data being binned
  labs(x = "Years", y = "Total Emissions (tons)", title = "Motor Vehicle Emissions in Los Angeles and Baltimore Cities 1999 - 2008") ## add labels

mutate(sumtable, Change = Emissions/lag(Emissions))

mutate(sumtable, Change = Emissions/lag(Emissions) * 100)

growth <- function(x)x/lag(x)-1)
sumtable %>% 
  group_by(fips) %>% 
  mutate_each(funs(growth), Emissions)

100 *(4101.321 - 3931.12)/3931.12
100 *(346 - 88)/88
(Present - Past)/Past

R> sumtable/lag(sumtable,-1) - 1

R> data/lag(data,-1) - 1

data <- ts(data.frame(x1=c(1:10), x2=c(11:20), x3=c(21:30)), start = c(2010,3), frequency = 4)

Website <- rep(paste("Website",1:3),2)
Year <- c(rep(2013,3),rep(2014,3))
V1 <- c(10,20,50,20,30,70)
V2 <- c(5,15,30,15,30,45)
df <- data.frame(Website,Year,V1,V2)
df

growth <- function(x)x/lag(x)-1
df %>% 
  group_by(Website) %>% 
  mutate_each(funs(growth), V1, V2)


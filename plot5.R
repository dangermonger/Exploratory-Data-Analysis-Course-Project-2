setwd("C:/Users/.../Exploratory-Data-Analysis-Course-Project-2")

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.
library(ggplot2)##summon ggplot2
library(dplyr) ##summon dplyr

if (!file.exists("exdata-data-NEI_data.zip")) { ##check that file doesn't already exist
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" ##identify url of file
  download.file(fileUrl, destfile = "./exdata-data-NEI_data.zip") ##download file
  unzip("./exdata-data-NEI_data.zip") ##unzip file in working directory
}

emissdata <- readRDS("summarySCC_PM25.rds") ##assign handle and read emmission data
classcode <- readRDS("Source_Classification_Code.rds") ##assign handle and read classification codes

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
  labs(x = "Years", y = "Total Emissions (tons)", title = "Motor Vehicle Emissions in Baltimore City 1999 - 2008\n") ## add labels

dev.off() ##close png
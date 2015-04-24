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
  labs(x = "Years", y = "Total Emissions (kilotons)", title = "Total Emissions in US by Coal Combustion 1999 - 2008\n") ##assign labels


dev.off() ##close png
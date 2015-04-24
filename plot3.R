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

png(file="plot3.png", width=650, height=480, bg="transparent") ##open png file

baltdata <- emissdata[which(emissdata$fips == "24510"),] ##subset data by rows where fips is equal to 24510 (Baltimore)

convertbl <- tbl_df(baltdata) ## convert table to tbl
rm("baltdata") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl then...
  group_by(type, year)%>% ##group by type and year then... SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP
  summarise(Emissions = sum(Emissions)) ##summarise the grouped data in columns and name and sum emmissions

qplot(year,  Emissions,  data  =	sumtable,	facets	=	.~type, geom	=	c("point",  "smooth"), method  =	"gam") + ##plot Year by Emissions, seperate by type, point, smooth and general additive model (gam)
  labs(x = "Years", y = "Total Emissions (tons)", title = "Total Emissions by Source Type, Baltimore 1999 - 2008\n")

dev.off() ##close png
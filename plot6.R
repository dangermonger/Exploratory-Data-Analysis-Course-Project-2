setwd("C:/Users/.../Exploratory-Data-Analysis-Course-Project-2")

library(graphics) ##plotting functions for the "base" graphing systems, including plot, hist, boxplot and many others.
library(grDevices) ##contains all the code implementing the various graphics devices, including X11, PDF, PostScript, PNG, etc.
library(ggplot2)##summon ggplot2
library(dplyr) ##summon dplyr
library(scales) ##summon scales

if (!file.exists("exdata-data-NEI_data.zip")) { ##check that file doesn't already exist
  fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip" ##identify url of file
  download.file(fileUrl, destfile = "./exdata-data-NEI_data.zip") ##download file
  unzip("./exdata-data-NEI_data.zip") ##unzip file in working directory
}

emissdata <- readRDS("summarySCC_PM25.rds") ##assign handle and read emmission data
classcode <- readRDS("Source_Classification_Code.rds") ##assign handle and read classification codes

png(file="plot6.png", width=650, height=480, bg="transparent") ##open png file

bala <- subset(emissdata, emissdata$fips %in% c("24510","06037"))##subset data by rows where fips is equal to 24510 (Baltimore) and LA 06037

balaonroad <- bala[which(bala$type == "ON-ROAD"),] ##subset baltdata by rows where type is equal to ON-ROAD to filter by motor vehicles

balaonroad$year <- factor(balaonroad$year) ##convert year column to a factor vector

balaonroad$fips <- factor(balaonroad$fips) ##convert fips column to a factor vector

levels(balaonroad$fips) <- c("Los Angeles", "Baltimore") ##convert fips levels to city names

convertbl <- tbl_df(balaonroad) ## convert table to tbl
rm("balaonroad") ## remove other handle

sumtable <- convertbl %>% ##sumtable is assigned to convertbl handle then...
  group_by(fips, year)%>% ##group by year then... (SUBSEQUENT OPERATIONS ARE PERFORMED BY GROUP)
  summarise(Emissions = sum(Emissions))%>% ##summarise the grouped data in columns and name and sum emmissions then..
  mutate(Change = (Emissions-Emissions[1])/Emissions[1]) ##add percentage change column

ggplot(data=sumtable, aes(x=year, y=Change, group=1, colour=fips )) + ##identify data, identify labels, single line connecting points, colour by city
  geom_rect(data = sumtable,aes(fill = fips),xmin = -Inf,xmax = Inf, ymin = -Inf,ymax = Inf,alpha = 0.01) + ##create transparent rectangles over the facets that match the extent of the graph
  facet_grid(.~fips)+ ##side by side graphs
  geom_line()+ ##line style
  geom_point()+ ##point style
  geom_hline(yintercept=0, linetype="dashed") + ##introduce 0% line with dash style
  scale_y_continuous(labels = percent) + ##change y axis to percentage
  theme(legend.title=element_blank(), ##remove legend title/background
        legend.background=element_blank(), ##remove legend background
        plot.background=element_blank(), ##make plot background transparent
        strip.background = element_rect(colour="black")) + ##add outline to facet label
  xlab("Years") + ylab("Percent Change") + ##add label titles
  ggtitle("Percentage Change in Motor Vehicle Emissions - L.A. and Baltimore Cities 1999 - 2008\n") ##add chart title with new line at end for space

dev.off() ##close png
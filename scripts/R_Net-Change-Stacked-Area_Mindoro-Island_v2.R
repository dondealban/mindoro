# Script Description --------------------
# This script creates a stacked area graph of multi-temporal land cover data, particularly the land cover maps produced
# using Landsat data at four time-points: 1988, 2000, 2010, and 2015 for Mindoro Island, Philippines. The land cover maps
# consist of 8 categories including: forest, mangrove, grasland, rice paddy/bare soil, exposed rock, shrubs/other
# vegetation, and water.
#
# Script By:      Jose Don T De Alban
# Date Created:   20 Nov 2017
# Last Modified:  08 Apr 2021


# Set Working Directory -----------------
setwd("/Users/dondealban/Dropbox/Research/mindoro/stacked area/mindoro_island")

# Load Libraries and Data ---------------
library(reshape2)
library(tidyverse)

# Read csv files in the directory and store as a list
filenames <- list.files()

# Function to read data
readdata <- function(filename) {
  df <- read.csv(filename, sep="\t")
  vec <- df[,3]           # Read column with percentage values
  names(vec) <- df[,1]    # Read column with class codes
  return(vec)
}

# Combine as class codes and percentage values in a matrix
temp <- do.call(rbind, lapply(filenames, readdata))
colnames(temp) <- c("1","2","3","4","5","6","7","8")

# Add years as another column
row.names(temp) <- c("1988","2000","2010","2015")

# Convert wide format data frame into long format data frame
data <- melt(temp, id.vars="years", variable.name="class", value.name="percentage")
colnames(data) <- c("Years","Class","Percentage")

# Create Stacked Area Graphs ------------

plot <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                   labels=c("Forest",
                            "Mangrove",
                            "Grassland",
                            "Rice Paddy / Bare Soil",
                            "Exposed Rock",
                            "Shrub / Other Vegetation",
                            "Built-up Area",
                            "Water Body"))), 
                   data=data)
plot <- plot + labs(x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot <- plot + scale_fill_manual(values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"))
plot <- plot + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot <- plot + theme_bw()
plot <- plot + theme(legend.position="bottom", legend.box="horizontal", legend.title = element_blank())
plot <- plot + theme(legend.text=element_text(size=13))
plot <- plot + theme(axis.title=element_text(size=13), axis.text=element_text(size=11))
plot <- plot + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

ggsave(plot, file="StackedArea-00-Mindoro-Island.pdf", width=16, height=15, units="cm", dpi=300)

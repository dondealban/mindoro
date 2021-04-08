# Script Description --------------------
# This script creates a stacked area graph of multi-temporal land cover data, particularly the land cover maps produced
# using Landsat data at four time-points: 1988, 2000, 2010, and 2015 for Mindoro Island, Philippines. The land cover maps
# consist of 8 categories including: forest, mangrove, grasland, rice paddy/bare soil, exposed rock, shrubs/other
# vegetation, and water.
#
# Script By:      Jose Don T De Alban
# Date Created:   20 Nov 2017
# Last Modified:  08 Apr 2021


# Set Working Directories ---------------
Dir1 <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/mindoro_island"
Dir2 <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/pa_mcws"
Dir3 <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/kba_siburan"
Dir4 <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/pa_mibnp"

# Load Libraries and Data ---------------
library(reshape2)
library(tidyverse)

# Function to Read Data Files -----------
readdata <- function(filename) {
  df <- read.csv(filename, sep="\t")
  vec <- df[,3]           # Read column with percentage values
  names(vec) <- df[,1]    # Read column with class codes
  return(vec)
}

# MINDORO ISLAND

# Read csv files in the directory and store as a list
setwd(Dir1)
filenames1 <- list.files()

# Combine as class codes and percentage values in a matrix
temp1 <- do.call(rbind, lapply(filenames1, readdata))
colnames(temp1) <- c("1","2","3","4","5","6","7","8")
row.names(temp1) <- c("1988","2000","2010","2015") # Add years as another column

# Convert wide format data frame into long format data frame
data1 <- melt(temp1, id.vars="years", variable.name="class", value.name="percentage")
colnames(data1) <- c("Years","Class","Percentage")

# Create stacked area plot
plot1 <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                    labels=c("Forest",
                             "Mangrove",
                             "Grassland",
                             "Rice Paddy / Bare Soil",
                             "Exposed Rock",
                             "Shrub / Other Vegetation",
                             "Built-up Area",
                             "Water Body"))), 
                    data=data1)
plot1 <- plot1 + labs(title="(a) Mindoro Island", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot1 <- plot1 + scale_fill_manual(values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"))
plot1 <- plot1 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot1 <- plot1 + theme_bw()
plot1 <- plot1 + theme(legend.position="bottom", legend.box="horizontal", legend.title = element_blank())
plot1 <- plot1 + theme(legend.text=element_text(size=13))
plot1 <- plot1 + theme(axis.title=element_text(size=13), axis.text=element_text(size=11))
plot1 <- plot1 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# MT CALAVITE WILDLIFE SANCTUARY

# Read csv files in the directory and store as a list
setwd(Dir2)
filenames2 <- list.files()

# Combine as class codes and percentage values in a matrix
temp2 <- do.call(rbind, lapply(filenames2, readdata))
colnames(temp2) <- c("1","2","3","4","5","6","7","8")
row.names(temp2) <- c("1988","2000","2010","2015") # Add years as another column

# Convert wide format data frame into long format data frame
data2 <- melt(temp2, id.vars="years", variable.name="class", value.name="percentage")
colnames(data2) <- c("Years","Class","Percentage")

# Create stacked area plot
plot2 <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                    labels=c("Forest",
                             "Mangrove",
                             "Grassland",
                             "Rice Paddy / Bare Soil",
                             "Exposed Rock",
                             "Shrub / Other Vegetation",
                             "Built-up Area",
                             "Water Body"))), 
                             data=data2)
plot2 <- plot2 + labs(title="(a) Mt. Calavite Wildlife Sanctuary", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot2 <- plot2 + scale_fill_manual(values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"))
plot2 <- plot2 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot2 <- plot2 + theme_bw()
plot2 <- plot2 + theme(legend.position="bottom", legend.box="horizontal", legend.title = element_blank())
plot2 <- plot2 + theme(legend.text=element_text(size=13))
plot2 <- plot2 + theme(axis.title=element_text(size=13), axis.text=element_text(size=11))
plot2 <- plot2 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())


ggsave(plot, file="StackedArea_Mindoro-Island_v2.pdf", width=16, height=15, units="cm", dpi=300)

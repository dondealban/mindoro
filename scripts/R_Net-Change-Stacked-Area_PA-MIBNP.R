# Script Description --------------------
# This script creates a stacked area graph of multi-temporal land cover data, particularly
# the land cover maps produced using Landsat data at four time-points: 1988, 2000, 2010,
# and 2015 for Mindoro Island, Philippines. The land cover maps consist of 8 categories 
# including: forest, mangrove, grasland, rice paddy/bare soil, exposed rock, shrubs/other
# vegetation, and water. The plot shows net change for Mt Iglit-Baco National Park.
#
# Script By:      Jose Don T De Alban
# Date Created:   20 Nov 2017
# Last Modified:  30 Jul 2019


# Set Working Directory -----------------
setwd("/Users/dondealban/Mindoro/pa_mibnp")

# Load Libraries and Data ---------------
library(reshape2)
library(tidyverse)

# Read csv files in the directory and store as a list
filenames <- list.files()

# Store files into four separate temporary dataframes 
temp1 <- filenames[1] # 1988
temp2 <- filenames[2] # 2000
temp3 <- filenames[3] # 2010
temp4 <- filenames[4] # 2015

# Function to read data
readdata <- function(filename) {
  df <- read.csv(filename, sep="\t")
  vec <- df[, 3]           # Read column with percentage values
  names(vec) <- df[, 1]    # Read column with class codes
  return(vec)
}

# Combine as class codes and percentage values in a matrix
mat1 <- do.call(rbind, lapply(temp1, readdata))
mat2 <- do.call(rbind, lapply(temp2, readdata))
mat3 <- do.call(rbind, lapply(temp3, readdata))
mat4 <- do.call(rbind, lapply(temp4, readdata))

# Remove unnecessary columns
mat1a <- t(as.matrix(mat1[,-c(2,7)]))
mat2a <- t(as.matrix(mat2[,-c(2,7)]))
mat3a <- t(as.matrix(mat3[,-c(6)]))
mat4a <- t(as.matrix(mat4[,-c(6:7)]))

# Combine as class codes and percentage values in a matrix
temp <- rbind(mat1a, mat2a, mat3a, mat4a)
colnames(temp) <- c("1","2","3","4","5")

# Add years as another column
row.names(temp) <- c("1988","2000","2010","2015")

# Convert wide format data frame into long format data frame
data <- melt(temp, id.vars="years", variable.name="class", value.name="percentage")
colnames(data) <- c("Years","Class","Percentage")

# Create Stacked Area Graphs ------------

plot <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                   labels=c("Forest",
                            "Grassland",
                            "Rice Paddy / Bare Soil",
                            "Exposed Rock",
                            "Shrub / Other Vegetation"))), 
                   data=data)
plot <- plot + labs(x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot <- plot + scale_fill_manual(values=c("#246a24","#c6f800","#ffff66","#bcbdbc","#07d316"))
plot <- plot + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot <- plot + theme_bw()
plot <- plot + theme(legend.position="bottom", legend.box="horizontal", legend.title = element_blank())

ggsave(plot, file="StackedArea-00-Mindoro-PA-MIBNP.pdf", width=16, height=15, units="cm", dpi=300)

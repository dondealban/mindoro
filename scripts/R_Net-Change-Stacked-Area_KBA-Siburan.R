# Script Description --------------------
# This script creates a stacked area graph of multi-temporal land cover data, particularly
# the land cover maps produced using Landsat data at four time-points: 1988, 2000, 2010,
# and 2015 for Mindoro Island, Philippines. The land cover maps consist of 8 categories 
# including: forest, mangrove, grasland, rice paddy/bare soil, exposed rock, shrubs/other
# vegetation, and water.
#
# Script By:      Jose Don T De Alban
# Date Created:   20 Nov 2017
# Last Modified:  25 Aug 2019


# Set Working Directory -----------------
setwd("/Users/dondealban/Mindoro/kba_siburan")

# Load Libraries and Data ---------------
library(reshape2)
library(tidyverse)

# Read csv files in the directory and store as a list
filenames <- list.files()

# Store files into four separate temporary dataframes 
period1 <- filenames[1] # 1988
period2 <- filenames[2] # 2000
period3 <- filenames[3] # 2010
period4 <- filenames[4] # 2015

# Function to read data
readdata <- function(filename) {
  df <- read.csv(filename, sep="\t")
  vec <- df[, 3]           # Read column with percentage values
  names(vec) <- df[, 1]    # Read column with class codes
  return(vec)
}

# Combine as class codes and percentage values in a matrix
mat1 <- do.call(rbind, lapply(period1, readdata))
mat2 <- do.call(rbind, lapply(period2, readdata))
mat3 <- do.call(rbind, lapply(period3, readdata))
mat4 <- do.call(rbind, lapply(period4, readdata))

# Create new column with zeroes for Class 6 in 2nd and 3rd period and insert in matrix
mat1a <- t(as.matrix(c(mat1[,1:ncol(mat1)]))) # transposed 1x7 matrix
mat2a <- t(as.matrix(c(mat2[,1:5], 0, mat2[,6]))) # transposed 1x7 matrix
mat3a <- t(as.matrix(c(mat3[,1:5], 0, mat3[,6]))) # transposed 1x7 matrix
mat4a <- t(as.matrix(c(mat4[,1:ncol(mat4)]))) # transposed 1x7 matrix

# Combine matrices from two periods and change column names
temp <- rbind(mat1a, mat2a, mat3a, mat4a)
colnames(temp) <- c("1","2","3","4","5","6","7")

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
                            "Shrub / Other Vegetation",
                            "Built-up Area",
                            "Water Body"))), 
                   data=data)
plot <- plot + labs(x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot <- plot + scale_fill_manual(values=c("#246a24","#c6f800","#ffff66",
                                          "#bcbdbc","#07d316","#ff0000","#66ccff"))
plot <- plot + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot <- plot + theme_bw()
plot <- plot + theme(legend.position="bottom", legend.box="horizontal", legend.title = element_blank())

ggsave(plot, file="StackedArea-00-Mindoro-KBA-Siburan.pdf", width=16, height=15, units="cm", dpi=300)

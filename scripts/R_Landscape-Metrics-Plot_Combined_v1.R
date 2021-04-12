# Script Description ----------------------
# This R script creates a visualisation plot using ggplot2 package of the changes in landscape metrics calculated for 
# various land cover types in Mindoro Island, Philippines at four time-points: 1988, 2000, 2010, 2015. The land cover
# maps were derived from land cover classification of Landsat data for each time-point. The landscape metrics were 
# calculated separately by Johanness Jamaludin using the landscapemetrics R package. 
#
# Script By:      Jose Don T De Alban
# Date Created:   12 Apr 2021
# Last Modified:  


# Set Working Directory -------------------
setwd("/Users/dondealban/Dropbox/Research/Mindoro/landscape patterns/")

# Load Libraries --------------------------
library(reshape2)
library(tidyverse)

# Read Data Files ------------------------
csvLM <- read.csv(file="mindoro-pattern-metrics-wide-final-allregions_rev.csv", header=TRUE, sep=",")
colnames(csvLM) <- c("Code.Region.Year","Site","Year","Class.Code","Land.Cover","Percent.Landscape",
                     "Number.Patches","Mean.Patch.Area","Edge.Density","Mean.Shape.Index","Mean.NN.Distance")

# Reorganise Input Data Files ------------
# Convert dataframe from wide-format to long-format
dfLMall <- melt(csvLM, id.vars=c("Code.Region.Year","Site","Year","Class.Code","Land.Cover"))
colnames(dfLMall) <- c("Code.Region.Year","Site","Year","Class.Code","Land.Cover","Landscape.Metrics","Value")
# Create subset dataframe by extracting forest/grassland land cover types
dfLMsub <- dfLMall %>% filter(dfLMall$Land.Cover %in% "Forest" | dfLMall$Land.Cover %in% "Grassland")

# Generate Plots ------------------------

# Plot #1: Changes in Landscape Metrics for All Land Cover Types
plot1 <- ggplot() + geom_line(data=dfLMall, aes(x=Year, y=Value, colour=as.factor(Class.Code)))
plot1 <- plot1 + facet_wrap(Site ~ Landscape.Metrics, ncol=6, scales="free_y")
plot1 <- plot1 + scale_colour_manual(name="Land Cover Type",
                                     values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"),
                                     labels=c("Forest","Mangrove","Grassland","Rice Paddy / Bare Soil",
                                              "Exposed Rock","Shrub / Other Vegetation","Built-up Area","Water Body"))


# Plot #2: Changes in Landscape Metrics for a Subset of Land Cover Types
plot2 <- ggplot() + geom_line(data=dfLMsub, aes(x=Year, y=Value, colour=as.factor(Class.Code)))
plot2 <- plot2 + facet_wrap(Site ~ Landscape.Metrics, ncol=6, scales="free_y")
plot2 <- plot2 + scale_colour_manual(name="Land Cover Type", values=c("#246a24","#c6f800"), labels=c("Forest","Grassland"))
plot2 <- plot2 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot2 <- plot2 + theme_bw()
plot2 <- plot2 + theme(legend.title=element_blank(), legend.position="bottom", legend.box="horizontal")

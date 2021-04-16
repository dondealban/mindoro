# Script Description ----------------------
# This R script creates a visualisation plot using ggplot2 package of the changes in landscape metrics calculated for 
# various land cover types in Mindoro Island, Philippines at four time-points: 1988, 2000, 2010, 2015. The land cover
# maps were derived from land cover classification of Landsat data for each time-point. The landscape metrics were 
# calculated separately by Johanness Jamaludin using the landscapemetrics R package. 
#
# Script By:      Jose Don T De Alban
# Date Created:   12 Apr 2021
# Last Modified:  16 Apr 2021


# Set Working Directory -------------------
setwd("/Users/dondealban/Dropbox/Research/Mindoro/landscape patterns/")

# Load Libraries --------------------------
library(reshape2)
library(tidyverse)

# Read Data Files ------------------------
csvLM <- read.csv(file="mindoro-pattern-metrics-wide-final-allregions_rev.csv", header=TRUE, sep=",")
colnames(csvLM) <- c("Code.Region.Year","Site","Year","Class.Code","Land.Cover","Percent.Landscape",
                     "Number.Patches","Mean.Patch.Area","Edge.Density","Mean.Shape.Index","Mean.NN.Distance")

# Organise Input Data Files --------------
# Convert dataframe from wide-format to long-format
dfLMall <- melt(csvLM, id.vars=c("Code.Region.Year","Site","Year","Class.Code","Land.Cover"))
colnames(dfLMall) <- c("Code.Region.Year","Site","Year","Class.Code","Land.Cover","Landscape.Metrics","Value")

# Replace content of input cells
dfLMall$Landscape.Metrics <- gsub('Percent.Landscape', 'percent of landscape (%)', dfLMall$Landscape.Metrics)
dfLMall$Landscape.Metrics <- gsub('Number.Patches', 'number of patches', dfLMall$Landscape.Metrics)
dfLMall$Landscape.Metrics <- gsub('Mean.Patch.Area', 'mean patch area (ha)', dfLMall$Landscape.Metrics)
dfLMall$Landscape.Metrics <- gsub('Edge.Density', 'edge density (m/ha)', dfLMall$Landscape.Metrics)
dfLMall$Landscape.Metrics <- gsub('Mean.Shape.Index', 'mean shape index', dfLMall$Landscape.Metrics)
dfLMall$Landscape.Metrics <- gsub('Mean.NN.Distance', 'mean nn distance (m)', dfLMall$Landscape.Metrics)

# Create subset dataframe by extracting forest/grassland land cover types
dfLMsub <- dfLMall %>% filter(dfLMall$Land.Cover %in% "Forest" | dfLMall$Land.Cover %in% "Grassland")

# Generate Plots ------------------------

# Plot #2: Changes in Landscape Metrics for a Subset of Land Cover Types
plot2 <- ggplot() + geom_line(data=dfLMsub, aes(x=Year, y=Value, colour=as.factor(Class.Code)), size=0.8)
plot2 <- plot2 + facet_wrap(Site ~ Landscape.Metrics, ncol=6, scales="free_y")
plot2 <- plot2 + scale_colour_manual(name="Land Cover Type", values=c("#246a24","#07d316"), labels=c("Forest","Grassland"))
plot2 <- plot2 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot2 <- plot2 + theme_bw()
plot2 <- plot2 + theme(legend.title=element_blank(), legend.position="bottom", legend.box="horizontal")
plot2 <- plot2 + theme(legend.text=element_text(size=14), strip.text.x=element_text(size=13))
plot2 <- plot2 + theme(axis.title=element_text(size=14), axis.text=element_text(size=12))
plot2 <- plot2 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Save Output Plots -----------------------

# Output boxplots to a PDF file
ggsave(plot2, file="Landscape-Metrics_Forest-Grassland_Mindoro_v2.pdf", width=50, height=25, units="cm", dpi=300)


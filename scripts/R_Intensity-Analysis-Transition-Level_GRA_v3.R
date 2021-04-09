# Script Description --------------------
# This R script generates barplots using ggplot2 package to visualise the transition-level intensity analysis of
# changes in Mindoro Island, Philippines derived from land cover classification of Landsat data for three time-
# intervals: 1988-2000, 2000-2010, and 2010-2015. Intensity analysis was calculated using an Excel spreadsheet with a VBA macro
# (see https://sites.google.com/site/intensityanalysis/). The domain of analysis is Mindoro Island.
#
# Script By:      Jose Don T De Alban
# Date Created:   13 Mar 2018
# Last Modified:  09 Apr 2021

# Set Working Directory -------------------
setwd("/Users/dondealban/Dropbox/Research/Mindoro/intensity analysis/")

# Load Libraries --------------------------
library(tidyverse)
library(readxl)
library(egg)

# Read Input Data -------------------------

# Read transition level XLSX data file, convert to data frame, and store into variable
rawG <- as.data.frame(read_excel("Transition_Level_Intensity_Analysis.xlsx", sheet="GRA_Gain"))
rawL <- as.data.frame(read_excel("Transition_Level_Intensity_Analysis.xlsx", sheet="GRA_Loss"))

# Clean and Subset Data -------------------

# 1. Add Change Type column
type1 <- rep("Gain", nrow(rawG))
type2 <- rep("Loss", nrow(rawL))
dfG <- cbind(rawG, type1)
dfL <- cbind(rawL, type2)

# 2. Reorder columns before renaming
dfGain <- dfG[,c(1:2,12,3:11)]
dfLoss <- dfL[,c(1:2,12,3:11)]

# 3. Change column names for easier reference
# Note the following description of category level column names
# ColA - Years of Time Interval
# ColB - Study Area/Site
# ColC - Change Type
# ColD - Category Name
# ColE - Observed Annual Loss/Gain [number of elements]
# ColF - Loss/Gain Intensity [percent of t1/t2 category]
# ColG - Uniform Intensity [percent of t1/t2 to/from category]
# ColH - Uniform Annual Loss/Gain [number of elements]
# ColI - Hypothesized Annual Error [number of elements]
# ColJ - Commission Intensity [percent of t1/t2 transition]
# ColK - Omission Intensity [percent of t1/t2 transition]
# ColL - Hypothesized t1/t2 Error [percent of interval domain]

list <- c("ColA","ColB","ColC","ColD","ColE","ColF","ColG","ColH","ColI","ColJ","ColK","ColL")
colnames(dfGain) <- c(list)
colnames(dfLoss) <- c(list)

# Generate Plots ------------------------

# Plot 1: To N (Gain Transition)
plotG <- ggplot() + geom_bar(data=dfGain, aes(x=ColD, y=ColF, fill=ColC), stat="identity", position=position_dodge())
plotG <- plotG + geom_hline(data=dfGain, aes(yintercept=ColG, colour="#000000"), linetype="dashed") # Uniform line
plotG <- plotG + facet_grid(ColB ~ ColA, scales="free_y")
plotG <- plotG + labs(x="Losing Category", y="Annual Transition Intensity (% of Category at Initial Time)")
plotG <- plotG + labs(title="(b) Gross grassland gain transitions")
plotG <- plotG + scale_fill_manual(values=c("#5b9bd5"), labels=c("Gain Intensity"))
plotG <- plotG + scale_colour_manual(values=c("#000000"), labels=c("Uniform Intensity"))
plotG <- plotG + theme_bw()
plotG <- plotG + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank())
plotG <- plotG + theme(legend.text=element_text(size=13), strip.text=element_text(size=13))
plotG <- plotG + theme(axis.title=element_text(size=13), axis.text=element_text(size=12))
plotG <- plotG + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Plot 2: From M (Loss Transition)
plotL <- ggplot() + geom_bar(data=dfLoss, aes(x=ColD, y=ColF, fill=ColC), stat="identity", position=position_dodge())
plotL <- plotL + geom_hline(data=dfLoss, aes(yintercept=ColG, colour="#000000"), linetype="dashed") # Uniform line
plotL <- plotL + facet_grid(ColB ~ ColA, scales="free_y")
plotL <- plotL + labs(x="Gaining Category", y="Annual Transition Intensity (% of Category at Final Time)")
plotL <- plotL + labs(title="(a) Gross grassland loss transitions")
plotL <- plotL + scale_fill_manual(values=c("#ed7d31"), labels=c("Loss Intensity"))
plotL <- plotL + scale_colour_manual(values=c("#000000"), labels=c("Uniform Intensity"))
plotL <- plotL + theme_bw()
plotL <- plotL + theme(legend.position="bottom", legend.box="horizontal", legend.title=element_blank())
plotL <- plotL + theme(legend.text=element_text(size=13), strip.text=element_text(size=13))
plotL <- plotL + theme(axis.title=element_text(size=13), axis.text=element_text(size=12))
plotL <- plotL + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Expose ggplot2 Layouts ----------------
plotlayout <- lapply(list(plotL, plotG), expose_layout, FALSE, FALSE)
grid.arrange(
  grobs = plotlayout,
  widths = c(1,1),
  layout_matrix = rbind(c(1,2))
)
mergeplot <- ggarrange(plotL, plotG, widths=c(1,1), heights=c(1))

# Save Outputs --------------------------

# Output boxplots to a PDF file
ggsave(plotG, file="Transition-Level-Intensity-Analysis_Grassland-Gain_v3.pdf", width=30, height=35, units="cm", dpi=300)
ggsave(plotL, file="Transition-Level-Intensity-Analysis_Grassland-Loss_v3.pdf", width=30, height=35, units="cm", dpi=300)
ggsave(mergeplot, file="Transition-Level-Intensity-Analysis_Combined-Grassland-GainLoss_v1.pdf", width=60, height=35, units="cm", dpi=300)


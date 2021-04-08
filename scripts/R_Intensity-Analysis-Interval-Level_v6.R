# Script Description --------------------
# This R script generates barplots using ggplot2 package to visualise the interval-level intensity analysis of changes in
# Mindoro Island, Philippines derived from land cover classification of Landsat data for three time-intervals: 1988-2000,
# 2000-2010, and 2010-2015. Intensity analysis was calculated using an Excel spreadsheet with a VBA macro
# (see https://sites.google.com/site/intensityanalysis/).
#
# Script By:      Jose Don T De Alban
# Date Created:   25 Apr 2017
# Last Modified:  08 Apr 2021

# Set Working Directory -------------------
setwd("/Users/dondealban/Dropbox/Research/Mindoro/intensity analysis/")

# Load Libraries --------------------------
library(tidyverse)
library(readxl)

# Read Input Data -------------------------

# Read interval level XLSX data file
xlsxMINDR <- read_excel("Intensity_Analysis_Mindoro Island.xlsx", sheet="Interval_Level")
xlsxMCWS  <- read_excel("Intensity_Analysis_PA_MCWS.xlsx", sheet="Interval_Level")
xlsxMIBNP <- read_excel("Intensity_Analysis_PA_MIBNP.xlsx", sheet="Interval_Level")
xlsxSBRN  <- read_excel("Intensity_Analysis_KBA_Siburan.xlsx", sheet="Interval_Level")

# Clean and Subset Data -------------------

# Remove first row with unnecessary header name 
tempMINDR <- xlsxMINDR[-c(1),] %>% write_csv("Interval_Level_Mindoro_Island.csv")
tempMCWS  <- xlsxMCWS[-c(1),] %>% write_csv("Interval_Level_PA_MCWS.csv")
tempMIBNP <- xlsxMIBNP[-c(1),] %>% write_csv("Interval_Level_PA_MIBNP.csv")
tempSBRN  <- xlsxSBRN[-c(1),] %>% write_csv("Interval_Level_KBA_SBRN.csv")

# And save and then read CSV file
csvMINDR <- read.csv(file="Interval_Level_Mindoro_Island.csv", header=TRUE, sep=",")
csvMCWS  <- read.csv(file="Interval_Level_PA_MCWS.csv", header=TRUE, sep=",")
csvMIBNP <- read.csv(file="Interval_Level_PA_MIBNP.csv", header=TRUE, sep=",")
csvSBRN  <- read.csv(file="Interval_Level_KBA_SBRN.csv", header=TRUE, sep=",")

# Add new column with site name
csvMINDR$Site <- c("Mindoro Island")
csvMCWS$Site  <- c("Mt Calavite WS")
csvMIBNP$Site <- c("Mts Iglit-Baco NP")
csvSBRN$Site  <- c("Mt Siburan KBA")

# Combine data frames
csvINT <- rbind(csvMINDR, csvMCWS, csvMIBNP, csvSBRN)

# Rename column names
colnames(csvINT) <- c("Time.Interval","Obs.Change","Ann.Change","Uni.Ann.Change",
                     "Uni.Change","Hypo.Error","Comm.Intensity","Om.Intensity","Site") 

# Select columns: time interval, observed change rate, uniform change rate, site
dfINT <- subset(csvINT, select=c(1,3:4,9))
dfINT$Time.Interval <- gsub('_', '-', dfINT$Time.Interval) # Replace character in column

# Generate Plots ------------------------

# Interval-level intensity analysis plot for all domains for both intervals
plotINT <- ggplot() + geom_bar(data=dfINT, aes(x=Time.Interval, y=Ann.Change, fill="#c6c3bf"), stat="identity", position=position_dodge())
plotINT <- plotINT  + geom_hline(data=dfINT, aes(yintercept=Uni.Ann.Change, colour="#000000"), linetype="dashed", size=0.8) # Uniform line
plotINT <- plotINT  + facet_grid(~Site)
plotINT <- plotINT  + labs(x="Time Interval", y="Annual Change (% of Map)")
plotINT <- plotINT  + scale_fill_manual(values=c("#808080"), name="", labels = c("Observed Change"))
plotINT <- plotINT  + scale_colour_manual(values=c("#000000"), labels=c("Uniform Intensity"))
plotINT <- plotINT  + theme_bw()
plotINT <- plotINT  + theme(legend.title=element_blank(), legend.position="bottom", legend.box="horizontal")
plotINT <- plotINT  + theme(legend.text=element_text(size=11), axis.text=element_text(size=11), strip.text.x=element_text(size=11))
plotINT <- plotINT  + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Save Output Plots -----------------------

# Output boxplots to a PDF file
ggsave(plotINT, file="Interval-Level-Intensity-Analysis_Mindoro_v6.pdf", width=30, height=15, units="cm", dpi=300)


# Script Description --------------------
# This script creates visualisation plots of net and gross land cover changes from multi-temporal land cover data,
# particularly the land cover maps produced using Landsat data at four time-points: 1988, 2000, 2010, and 2015 for
# Mindoro Island, Philippines. The land cover maps consist of 8 categories including: forest, mangrove, grasland,
# rice paddy/bare soil, exposed rock, shrubs/other vegetation, and waterbody. The plots were generated using the
# tools in OpenLand R package (Exavier & Zeilhofer 2020).
#
# Script By:      Jose Don T De Alban
# Date Created:   05 Apr 2021
# Last Modified:  


# Load Libraries -------------------------
library(raster)
library(sp)
library(OpenLand)
library(ggplot2)

# Set Working Directories ----------------
DirDATA  <- "/Users/dondealban/Desktop/mindoro/"
DirMAIN  <- "/Users/dondealban/Dropbox/Research/mindoro/"
DirPLOT  <- "/Users/dondealban/Dropbox/Research/mindoro/net gross barplot/"

# Load Raster Files ----------------------
setwd(DirDATA)
r1988 <- raster('Mindoro_1988.tif') # Note: raster files in EPSG:32651; 0s treated as NAs
r2000 <- raster('Mindoro_2000.tif')
r2010 <- raster('Mindoro_2010.tif')
r2015 <- raster('Mindoro_2015.tif')
# Create rasterstacks from all raster files
imagestack <- stack(r1988,r2000,r2010,r2015) # Main stack
stack_1988_2000 <- stack(r1988,r2000) # Split stacks
stack_2000_2010 <- stack(r2000,r2010)
stack_2010_2015 <- stack(r2010,r2015)

# MAIN STACK -----------------------------

# Create Contingency Table ---------------
min_1988_2015 <- contingencyTable(input_raster=imagestack, pixelresolution=30)
min_1988_2015 # Checking tibble contents

# Modify Category Names and Legend -------
# Edit category names
min_1988_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
# Add colors for categories in the same order as the legend
min_1988_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
min_1988_2015$tb_legend # Checking legend contents

# Generate Plots -------------------------
# Barplot of net and gross land cover changes
setwd(DirPLOT)
main_multistep <- netgrossplot(dataset = min_1988_2015$lulc_Multistep,
                               legendtable = min_1988_2015$tb_legend,
                               xlab = "Land Cover Types",
                               ylab = bquote("Area ("~ km^2 ~")"),
                               changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                               color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))
main_onestep   <- netgrossplot(dataset = min_1988_2015$lulc_Onestep,
                               legendtable = min_1988_2015$tb_legend,
                               xlab = "Land Cover Types",
                               ylab = bquote("Area ("~ km^2 ~")"),
                               changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                               color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))

# SPLIT STACKS ---------------------------

# Create Contingency Table ---------------
min_1988_2000 <- contingencyTable(input_raster=stack_1988_2000, pixelresolution=30)
min_2000_2010 <- contingencyTable(input_raster=stack_2000_2010, pixelresolution=30)
min_2010_2015 <- contingencyTable(input_raster=stack_2010_2015, pixelresolution=30)

# Modify Category Names and Legend -------
# Edit category names
min_1988_2000$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2000_2010$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2010_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
# Add colors for categories in the same order as the legend
min_1988_2000$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
min_2000_2010$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
min_2010_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")

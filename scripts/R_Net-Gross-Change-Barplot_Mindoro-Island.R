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
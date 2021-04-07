# Script Description --------------------
# This script creates visualisation plots of net and gross land cover changes from multi-temporal land cover data,
# particularly the land cover maps produced using Landsat data at four time-points: 1988, 2000, 2010, and 2015 for
# Mindoro Island, Philippines. The land cover maps consist of 8 categories including: forest, mangrove, grasland,
# rice paddy/bare soil, exposed rock, shrubs/other vegetation, and waterbody. The plots were generated using the
# tools in OpenLand R package (Exavier & Zeilhofer 2020).
#
# Script By:      Jose Don T De Alban
# Date Created:   05 Apr 2021
# Last Modified:  07 Apr 2021


# Load Libraries -------------------------
library(raster)
library(sp)
library(OpenLand)
library(ggplot2)
library(egg)

# Set Working Directories ----------------
DirDATA  <- "/Users/dondealban/Desktop/mindoro/"
DirMAIN  <- "/Users/dondealban/Dropbox/Research/mindoro/"
DirPLOT  <- "/Users/dondealban/Dropbox/Research/mindoro/net gross barplot/"

# Load Raster Files ----------------------
setwd(DirDATA)

# Mindoro Island
r1988 <- raster('Mindoro_1988.tif') # Note: raster files in EPSG:32651; 0s treated as NAs
r2000 <- raster('Mindoro_2000.tif')
r2010 <- raster('Mindoro_2010.tif')
r2015 <- raster('Mindoro_2015.tif')
# Create rasterstacks from all raster files
imagestack <- stack(r1988,r2000,r2010,r2015) # Main stack
stack_1988_2000 <- stack(r1988,r2000) # Split stack I1
stack_2000_2010 <- stack(r2000,r2010) # Split stack I2
stack_2010_2015 <- stack(r2010,r2015) # Split stack I3

# Mt Calavite Wildlife Sanctuary
c1988 <- raster('MCWS_1988.tif') # Note: raster files in EPSG:32651; 0s treated as NAs
c2000 <- raster('MCWS_2000.tif')
c2010 <- raster('MCWS_2010.tif')
c2015 <- raster('MCWS_2015.tif')
# Create rasterstacks from all raster files
c_1988_2000 <- stack(c1988,c2000) # Split stack I1
c_2000_2010 <- stack(c2000,c2010) # Split stack I2
c_2010_2015 <- stack(c2010,c2015) # Split stack I3

# Mt Siburan Key Biodiversity Area
s1988 <- raster('Siburan_1988.tif') # Note: raster files in EPSG:32651; 0s treated as NAs
s2000 <- raster('Siburan_2000.tif')
s2010 <- raster('Siburan_2010.tif')
s2015 <- raster('Siburan_2015.tif')
# Create rasterstacks from all raster files
s_1988_2000 <- stack(s1988,s2000) # Split stack I1
s_2000_2010 <- stack(s2000,s2010) # Split stack I2
s_2010_2015 <- stack(s2010,s2015) # Split stack I3

# Mts Iglit-Baco National Park
b1988 <- raster('MIBNP_1988.tif') # Note: raster files in EPSG:32651; 0s treated as NAs
b2000 <- raster('MIBNP_2000.tif')
b2010 <- raster('MIBNP_2010.tif')
b2015 <- raster('MIBNP_2015.tif')
# Create rasterstacks from all raster files
b_1988_2000 <- stack(b1988,b2000) # Split stack I1
b_2000_2010 <- stack(b2000,b2010) # Split stack I2
b_2010_2015 <- stack(b2010,b2015) # Split stack I3

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
min_1988_2000 <- contingencyTable(input_raster=stack_1988_2000, pixelresolution=30) # Mindoro Island
min_2000_2010 <- contingencyTable(input_raster=stack_2000_2010, pixelresolution=30)
min_2010_2015 <- contingencyTable(input_raster=stack_2010_2015, pixelresolution=30)
c_1988_2000 <- contingencyTable(input_raster=c_1988_2000, pixelresolution=30)       # Mt Calavite Wildlife Sanctuary
c_2000_2010 <- contingencyTable(input_raster=c_2000_2010, pixelresolution=30)
c_2010_2015 <- contingencyTable(input_raster=c_2010_2015, pixelresolution=30)
s_1988_2000 <- contingencyTable(input_raster=s_1988_2000, pixelresolution=30)       # Mt Siburan Key Biodiversity Area
s_2000_2010 <- contingencyTable(input_raster=s_2000_2010, pixelresolution=30)
s_2010_2015 <- contingencyTable(input_raster=s_2010_2015, pixelresolution=30)
b_1988_2000 <- contingencyTable(input_raster=b_1988_2000, pixelresolution=30)       # Mts Iglit-Baco National Park
b_2000_2010 <- contingencyTable(input_raster=b_2000_2010, pixelresolution=30)
b_2010_2015 <- contingencyTable(input_raster=b_2010_2015, pixelresolution=30)

# Modify Category Names and Legend -------
# Edit category names
min_1988_2000$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2000_2010$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2010_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
c_1988_2000$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
c_2000_2010$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
c_2010_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
# Add colors for categories in the same order as the legend
min_1988_2000$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
min_2000_2010$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
min_2010_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
c_1988_2000$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
c_2000_2010$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")
c_2010_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")

# Generate Plots -------------------------
# Barplot of net and gross land cover changes
setwd(DirPLOT)

# Mindoro Island
# Plot I1 (1988-2000)
mplot_i1 <- netgrossplot(dataset = min_1988_2000$lulc_Multistep,
                        legendtable = min_1988_2000$tb_legend,
                        xlab = "Land Cover Types",
                        ylab = bquote("Area ("~ km^2 ~")"),
                        changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                        color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
mplot_i1 <- mplot_i1 + ylim(-1000,1000)
mplot_i1 <- mplot_i1 + labs(subtitle="1988-2000")
mplot_i1 <- mplot_i1 + theme(legend.position="none")
mplot_i1 <- mplot_i1 + theme(axis.title.x=element_blank())
# Plot I2 (2000-2010)
mplot_i2 <- netgrossplot(dataset = min_2000_2010$lulc_Multistep,
                        legendtable = min_2000_2010$tb_legend,
                        xlab = "Land Cover Types",
                        ylab = bquote("Area ("~ km^2 ~")"),
                        changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                        color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
mplot_i2 <- mplot_i2 + ylim(-1000,1000)
mplot_i2 <- mplot_i2 + labs(title="Mindoro Island", subtitle="2000-2010")
mplot_i2 <- mplot_i2 + theme(legend.position="none")
mplot_i2 <- mplot_i2 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank())
# Plot I3 (2010-2015)
mplot_i3 <- netgrossplot(dataset = min_2010_2015$lulc_Multistep,
                        legendtable = min_2010_2015$tb_legend,
                        xlab = "Land Cover Types",
                        ylab = bquote("Area ("~ km^2 ~")"),
                        changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                        color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
mplot_i3 <- mplot_i3 + ylim(-1000,1000)
mplot_i3 <- mplot_i3 + labs(subtitle="2010-2015")
mplot_i3 <- mplot_i3 + theme(axis.title.x=element_blank())
mplot_i3 <- mplot_i3 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank())

# Mt Calavite Wildlife Sanctuary
# Plot I1 (1988-2000)
cplot_i1 <- netgrossplot(dataset = c_1988_2000$lulc_Multistep,
                         legendtable = c_1988_2000$tb_legend,
                         xlab = "Land Cover Types",
                         ylab = bquote("Area ("~ km^2 ~")"),
                         changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                         color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
cplot_i1 <- cplot_i1 + ylim(-25,25)
cplot_i1 <- cplot_i1 + labs(subtitle="1988-2000")
cplot_i1 <- cplot_i1 + theme(legend.position="none")
cplot_i1 <- cplot_i1 + theme(axis.title.x=element_blank())
# Plot I2 (2000-2010)
cplot_i2 <- netgrossplot(dataset = c_2000_2010$lulc_Multistep,
                         legendtable = c_2000_2010$tb_legend,
                         xlab = "Land Cover Types",
                         ylab = bquote("Area ("~ km^2 ~")"),
                         changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                         color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
cplot_i2 <- cplot_i2 + ylim(-25,25)
cplot_i2 <- cplot_i2 + labs(title="Mt Calavite Wildlife Sanctuary", subtitle="2000-2010")
cplot_i2 <- cplot_i2 + theme(legend.position="none")
cplot_i2 <- cplot_i2 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank())
# Plot I3 (2010-2015)
cplot_i3 <- netgrossplot(dataset = c_2010_2015$lulc_Multistep,
                         legendtable = c_2010_2015$tb_legend,
                         xlab = "Land Cover Types",
                         ylab = bquote("Area ("~ km^2 ~")"),
                         changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
                         color = c(GC = "gray70", NG = "#1e88e5", NL = "#d81b60"))
cplot_i3 <- cplot_i3 + ylim(-25,25)
cplot_i3 <- cplot_i3 + labs(subtitle="2010-2015")
cplot_i3 <- cplot_i3 + theme(axis.title.x=element_blank())
cplot_i3 <- cplot_i3 + theme(axis.text.y=element_blank(), axis.ticks.y=element_blank(), axis.title.y = element_blank())

# Expose ggplot2 Layouts -----------------
plotlayout_m <- lapply(list(mplot_i1, mplot_i2, mplot_i3), expose_layout, FALSE, FALSE) # Mindoro Island
grid.arrange(
  grobs = plotlayout_m,
  widths = c(1,1,1),
  layout_matrix = rbind(c(1,2,3))
)
mergeplot_m <- ggarrange(mplot_i1, mplot_i2, mplot_i3, widths=c(1,1,1), heights=c(1))

plotlayout_c <- lapply(list(cplot_i1, cplot_i2, cplot_i3), expose_layout, FALSE, FALSE) # Mt Calavite Wildlife Sanctuary
grid.arrange(
  grobs = plotlayout_c,
  widths = c(1,1,1),
  layout_matrix = rbind(c(1,2,3))
)
mergeplot_c <- ggarrange(cplot_i1, cplot_i2, cplot_i3, widths=c(1,1,1), heights=c(1))

# Save Output Plots ----------------------
setwd(DirPLOT)
ggsave(mergeplot, file="NetGrossBarplot_Mindoro-Island.pdf", width=30, height=10, units="cm", dpi=300)

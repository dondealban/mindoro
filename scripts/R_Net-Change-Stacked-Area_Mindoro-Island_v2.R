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
Dir1    <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/mindoro_island/"
Dir2    <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/pa_mcws/"
Dir3    <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/kba_siburan/"
Dir4    <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/pa_mibnp/"
DirMAIN <- "/Users/dondealban/Dropbox/Research/mindoro/stacked area/"

# Load Libraries and Data ---------------
library(egg)
library(ggplot2)
library(grid)
library(gtable)
library(reshape2)
library(tidyverse)

# Function to Read Data Files -----------
readdata <- function(filename) {
  df <- read.csv(filename, sep="\t")
  vec <- df[,3]           # Read column with percentage values
  names(vec) <- df[,1]    # Read column with class codes
  return(vec)
}

# Generate Study Area Plots -------------

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
plot1 <- plot1 + labs(title="Mindoro Island", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot1 <- plot1 + scale_fill_manual(values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"))
plot1 <- plot1 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot1 <- plot1 + theme_bw()
plot1 <- plot1 + theme(legend.position="none")
plot1 <- plot1 + theme(legend.title=element_text(size=13), legend.text=element_text(size=13))
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
plot2 <- plot2 + labs(title="Mt. Calavite WS", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot2 <- plot2 + scale_fill_manual(values=c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff"))
plot2 <- plot2 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot2 <- plot2 + theme_bw()
plot2 <- plot2 + theme(legend.position="none")
plot2 <- plot2 + theme(legend.title=element_text(size=13), legend.text=element_text(size=13))
plot2 <- plot2 + theme(axis.title=element_text(size=13), axis.text=element_text(size=11), axis.title.y=element_blank())
plot2 <- plot2 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# MT SIBURAN KEY BIODIVERSITY AREA
# Read csv files in the directory and store as a list
setwd(Dir3)
filenames3 <- list.files()

# Store files into four separate temporary dataframes 
period1s <- filenames3[1] # 1988
period2s <- filenames3[2] # 2000
period3s <- filenames3[3] # 2010
period4s <- filenames3[4] # 2015

# Combine as class codes and percentage values in a matrix
vec1s <- do.call(rbind, lapply(period1s, readdata))
vec2s <- do.call(rbind, lapply(period2s, readdata))
vec3s <- do.call(rbind, lapply(period3s, readdata))
vec4s <- do.call(rbind, lapply(period4s, readdata))

# Create new column with zeroes for Class 6 in 2nd and 3rd period and insert in matrix
mat1s <- t(as.matrix(c(vec1s[,1:ncol(vec1s)]))) # transposed 1x7 matrix
mat2s <- t(as.matrix(c(vec2s[,1:5], 0, vec2s[,6]))) # transposed 1x7 matrix
mat3s <- t(as.matrix(c(vec3s[,1:5], 0, vec3s[,6]))) # transposed 1x7 matrix
mat4s <- t(as.matrix(c(vec4s[,1:ncol(vec4s)]))) # transposed 1x7 matrix

# Combine matrices from two periods and change column names
temp3 <- rbind(mat1s, mat2s, mat3s, mat4s)
colnames(temp3) <- c("1","2","3","4","5","6","7")
row.names(temp3) <- c("1988","2000","2010","2015") # Add years as another column

# Convert wide format data frame into long format data frame
data3 <- melt(temp3, id.vars="years", variable.name="class", value.name="percentage")
colnames(data3) <- c("Years","Class","Percentage")

# Create stacked area plot
plot3 <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                    labels=c("Forest",
                             "Grassland",
                             "Rice Paddy / Bare Soil",
                             "Exposed Rock",
                             "Shrub / Other Vegetation",
                             "Built-up Area",
                             "Water Body"))), 
                    data=data3)
plot3 <- plot3 + labs(title="Mt. Siburan KBA", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot3 <- plot3 + scale_fill_manual(values=c("#246a24","#c6f800","#ffff66",
                                            "#bcbdbc","#07d316","#ff0000","#66ccff"))
plot3 <- plot3 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot3 <- plot3 + theme_bw()
plot3 <- plot3 + theme(legend.position="none")
plot3 <- plot3 + theme(legend.title=element_text(size=13), legend.text=element_text(size=13))
plot3 <- plot3 + theme(axis.title=element_text(size=13), axis.text=element_text(size=11), axis.title.y=element_blank())
plot3 <- plot3 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# MTS IGLIT-BACO NATIONAL PARK
# Read csv files in the directory and store as a list
setwd(Dir4)
filenames4 <- list.files()

# Store files into four separate temporary dataframes 
period1b <- filenames4[1] # 1988
period2b <- filenames4[2] # 2000
period3b <- filenames4[3] # 2010
period4b <- filenames4[4] # 2015

# Combine as class codes and percentage values in a matrix
vec1b <- do.call(rbind, lapply(period1b, readdata))
vec2b <- do.call(rbind, lapply(period2b, readdata))
vec3b <- do.call(rbind, lapply(period3b, readdata))
vec4b <- do.call(rbind, lapply(period4b, readdata))

# Remove unnecessary columns
mat1b <- t(as.matrix(vec1b[,-c(2,7)]))
mat2b <- t(as.matrix(vec2b[,-c(2,7)]))
mat3b <- t(as.matrix(vec3b[,-c(6)]))
mat4b <- t(as.matrix(vec4b[,-c(6:7)]))

# Combine matrices from two periods and change column names
temp4 <- rbind(mat1b, mat2b, mat3b, mat4b)
colnames(temp4) <- c("1","2","3","4","5")
row.names(temp4) <- c("1988","2000","2010","2015") # Add years as another column

# Convert wide format data frame into long format data frame
data4 <- melt(temp4, id.vars="years", variable.name="class", value.name="percentage")
colnames(data4) <- c("Years","Class","Percentage")

# Create stacked area plot
plot4 <- ggplot() + geom_area(aes(x=Years, y=Percentage, fill=factor(Class,
                    labels=c("Forest",
                             "Grassland",
                             "Rice Paddy / Bare Soil",
                             "Exposed Rock",
                             "Shrub / Other Vegetation"))), 
                    data=data4)
plot4 <- plot4 + labs(title="Mts. Iglit-Baco NP", x="Year", y="Percentage of Landscape", fill="Land Cover Category")
plot4 <- plot4 + scale_fill_manual(values=c("#246a24","#c6f800","#ffff66","#bcbdbc","#07d316"))
plot4 <- plot4 + scale_x_continuous(breaks=c(1988,2000,2010,2015))
plot4 <- plot4 + theme_bw()
plot4 <- plot4 + theme(legend.position="none")
plot4 <- plot4 + theme(legend.title=element_text(size=13), legend.text=element_text(size=13))
plot4 <- plot4 + theme(axis.title=element_text(size=13), axis.text=element_text(size=11), axis.title.y=element_blank())
plot4 <- plot4 + theme(panel.grid.major=element_blank(), panel.grid.minor=element_blank())

# Expose ggplot2 Layouts -----------------
plotlayout <- lapply(list(plot1, plot2, plot3, plot4), expose_layout, FALSE, FALSE)
grid.arrange(
  grobs = plotlayout,
  widths = c(2,2),
  layout_matrix = rbind(c(1,2),
                        c(3,4))
)
mergeplot <- ggarrange(plot1, plot2, plot3, plot4, widths=c(1,1), heights=c(1,1))

# Function to Combine Legend -------------
grid_arrange_shared_legend <-
  function(...,
           ncol = length(list(...)),
           nrow = 1,
           position = c("bottom", "right"))
    {
    plots <- list(...)
    position <- match.arg(position)
    g <-
      ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
    legend <- g[[which(sapply(g, function(x)
      x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    lwidth <- sum(legend$width)
    gl <- lapply(plots, function(x)
      x + theme(legend.position = "none"))
    gl <- c(gl, ncol = ncol, nrow = nrow)
    
    combined <- switch(
      position,
      "bottom" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 1,
        heights = unit.c(unit(1, "npc") - lheight, lheight)
      ),
      "right" = arrangeGrob(
        do.call(arrangeGrob, gl),
        legend,
        ncol = 2,
        widths = unit.c(unit(1, "npc") - lwidth, lwidth)
      )
    )
    grid.newpage()
    grid.draw(combined)
    
    # return gtable invisibly
    invisible(combined)
  }

# Combine legend of merged plot
grid_arrange_shared_legend(plot1, plot2, plot3, plot4)

# Save Plots -----------------------------
setwd(DirMAIN)
ggsave(grid_arrange_shared_legend(plot1, plot2, plot3, plot4), file="StackedArea_Combined_v1.pdf", width=30, height=15, units="cm", dpi=300)

setwd("/Users/dondealban/Desktop/")

library(raster)
library(sp)
library(OpenLand)
library(ggplot2)

r1988 <- raster('Mindoro_1988.tif')
r2000 <- raster('Mindoro_2000.tif')
r2010 <- raster('Mindoro_2010.tif')
r2015 <- raster('Mindoro_2015.tif')

#r1988[r1988 <= 0] <- NA
#r2000[r2000 <= 0] <- NA
#r2010[r2010 <= 0] <- NA
#r2015[r2015 <= 0] <- NA

imagestack <- stack(r1988,r2000,r2010,r2015)
imagestack
min_1988_2015 <- contingencyTable(input_raster = imagestack, pixelresolution = 30)
min_1988_2015
## editing the category name
min_1988_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))

## add the color by the same order of the legend,
## it can be the color name (eg. "black") or the HEX value (eg. #000000)
min_1988_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")

min_1988_2015$tb_legend

# Sankey diagram
sankeyLand(dataset = min_1988_2015$lulc_Multistep,legendtable = min_1988_2015$tb_legend)

# Barplot
barplotLand(dataset = min_1988_2015$lulc_Multistep, 
            legendtable = min_1988_2015$tb_legend,
            xlab = "Year",
            ylab = bquote("Area ("~ km^2~")"),
            area_km2 = TRUE)

# Net and gross land cover changes
netgrossplot(dataset = min_1988_2015$lulc_Multistep,
             legendtable = min_1988_2015$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))
netgrossplot(dataset = min_1988_2015$lulc_Onestep,
             legendtable = min_1988_2015$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))

# Intensity Analysis
testMin <- intensityAnalysis(dataset = min_1988_2015, category_n = "GRA", category_m = "GRA", area_km2=TRUE)
names(testMin)   # Inspect list of objects
testMin          # Show objects

## Interval-Level Intensity Analysis
plot_intervalIA <- plot(testMin$interval_lvl,
                        labels = c(leftlabel = "Interval Change Area (%)",
                                   rightlabel = "Annual Change Area (%)"),
                        marginplot = c(-8, 0), labs = c("Changes", "Uniform Rate"), 
                        leg_curv = c(x = 2/10, y = 3/10))

## Category-Level Intensity Analysis
## Gain Category
plot_category_g_IA <- plot(testMin$category_lvlGain, labels = c(leftlabel = bquote("Gain Area (" ~ km^2 ~ ")"),
                                                               rightlabel = "Intensity Gain (%)"),
                           marginplot = c(-8,8), labs = c("Categories", "Uniform Rate"), 
                           leg_curv = c(x = 5/10, y = 5/10))
## Loss Category
plot_category_l_IA <- plot(testMin$category_lvlLoss, labels = c(leftlabel = bquote("Loss Area (" ~ km^2 ~ ")"),
                                                               rightlabel = "Loss Intensity (%)"),
                           marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
                           leg_curv = c(x = 5/10, y = 5/10))

## Transition-Level Intensity Analysis
## Gain of the `n` Category 'FOR'
plot_transition_g_IA <- plot(testMin$transition_lvlGain_n,
                             labels = c(leftlabel = bquote("Gain of FOR (" ~ km^2 ~ ")"),
                                        rightlabel = "Intensity Gain of FOR (%)"),
                             marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
                             leg_curv = c(x = 5/10, y = 5/10))
## Gain of the `n` Category 'GRA'
plot_transition_g_IA <- plot(testMin$transition_lvlGain_n,
                             labels = c(leftlabel = bquote("Gain of GRA (" ~ km^2 ~ ")"),
                                        rightlabel = "Intensity Gain of GRA (%)"),
                             marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
                             leg_curv = c(x = 5/10, y = 5/10))

## Loss of the `m` Category 'FOR'
plot_transition_l_IA <- plot(testMin$transition_lvlLoss_m, 
                             labels = c(leftlabel = bquote("Loss of FOR (" ~ km^2 ~ ")"),
                                        rightlabel = "Intensity Loss of FOR (%)"),
                             marginplot = c(.3, .3), labs = c("Categories", "Uniform Rate"), 
                             leg_curv = c(x = 1/10, y = 5/10))
## Loss of the `m` Category 'GRA'
plot_transition_l_IA <- plot(testMin$transition_lvlLoss_m, 
                             labels = c(leftlabel = bquote("Loss of GRA (" ~ km^2 ~ ")"),
                                        rightlabel = "Intensity Loss of GRA (%)"),
                                        labs = c("Categories", "Uniform Rate")) 

plot(testMin$transition_lvlLoss_m)
testMin$transition_lvlLoss_m

losschart <- plot(testMin$transition_lvlLoss_m, 
                  labels = c(leftlabel = bquote("Annual Transition Area ("~km^2~")"),
                             rightlabel = "Annual Transition Intensity (%)"),
                  title = "Transition-Level Intensity Analysis: Grassland Loss",
                  labs = c("Categories","Uniform Intensity"))


### Alternative net gross plots
stack_1988_2000 <- stack(r1988,r2000)
stack_2000_2010 <- stack(r2000,r2010)
stack_2010_2015 <- stack(r2010,r2015)
stack_1988_2015 <- stack(r1988,r2015)

min_1988_2000 <- contingencyTable(input_raster = stack_1988_2000, pixelresolution = 30)
min_2000_2010 <- contingencyTable(input_raster = stack_2000_2010, pixelresolution = 30)
min_2010_2015 <- contingencyTable(input_raster = stack_2010_2015, pixelresolution = 30)

min_1988_2000$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_1988_2000$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")

min_2000_2010$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2000_2010$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")

min_2010_2015$tb_legend$categoryName <- factor(c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"),
                                               levels=c("FOR","MNG","GRA","RBS","ERK","SOV","BUA","WTR"))
min_2010_2015$tb_legend$color <- c("#246a24","#6666ff","#c6f800","#ffff66","#bcbdbc","#07d316","#ff0000","#66ccff")


netgrossplot(dataset = min_1988_2000$lulc_Onestep,
             legendtable = min_1988_2000$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))
netgrossplot(dataset = min_2000_2010$lulc_Onestep,
             legendtable = min_2000_2010$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))
netgrossplot(dataset = min_2010_2015$lulc_Onestep,
             legendtable = min_2010_2015$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))

chart <- netgrossplot(dataset = min_2010_2015$lulc_Onestep,
             legendtable = min_2010_2015$tb_legend,
             xlab = "Land Cover Types",
             ylab = bquote("Area ("~ km^2 ~")"),
             changesLabel = c(GC = "Gross change", NG = "Net gain", NL = "Net loss"),
             color = c(GC = "gray70", NG = "#006400", NL = "#EE2C2C"))
chart <- chart  + theme_bw()
chart <- chart  + theme(panel.background = element_blank())




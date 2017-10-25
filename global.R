## Title:   Global variables for shiny dashboard for collection analysis
## Date:    19-oct-2017
## Author:  Guus Martijn Teunisse (gmteunisse@gmail.com)
##########################################################################

##### Session basics #####

#Load functions
source("Dependencies/functions.R")

#Load packages
awesome_package_loader(c("shiny", 
                         "shinydashboard",
                         "readxl", 
                         "dplyr", 
                         "ggplot2", 
                         "plotly", 
                         "reshape2",
                         "scales"))

#Define plot styling
#Set colors
color_blue <- c("#3c598e", "#5077bd", "#6495ed", "#83aaf0", "#a2bff4")
color_orange <- c("#994433", "#cc5b44", "#ff7256", "#ff8e77", "#ffaa99")
color_tan <- c("#8e703c", "#bd9650", "#edbc64", "#f0c983", "#f4d6a2")
color_grey <- c("#737373", "#9a9a9a", "#c1c1c1", "#cdcdcd", "#d9d9d9")

#Set plotting aesthetics
plot_theme <- theme(panel.background = element_rect(fill = 'transparent', colour = NA),
                    plot.background = element_rect(fill = 'transparent', colour = NA),
                    legend.background = element_rect(fill = 'transparent', colour = NA),
                    panel.grid.minor = element_line(colour = NA),
                    panel.grid.major = element_line(colour = adjustcolor('grey', 0.2)),
                    legend.key = element_rect(fill = "white"),
                    legend.key.size = unit(0.4, "cm"),
                    text = element_text(family = "Helvetica", size = 12),
                    panel.grid.major.x = element_blank(),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(colour = 'grey'),
                    axis.line.y = element_line(colour = 'grey'),
                    axis.ticks = element_line(colour = 'grey'))
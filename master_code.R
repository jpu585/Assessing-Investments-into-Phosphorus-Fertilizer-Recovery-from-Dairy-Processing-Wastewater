# master file

# This code runs all the analysis on a current R version without any of the packages. It also creates all the figures. In case the packages are already, this step can be skipped.

# Set the folder with the replication files as working directory
#setwd()
#
#install.packages(c("pacman", "eurostat", "rsdmx", "lubridate", "sf",
#"scales", "ggrepel",
#                 "ggthemes", "conflicted", "gghighlight", "wesanderson",
#"gfonts",
#                 "plotly", "xts", "Benchmarking", "viridis", "patchwork", "RColorBrewer", "ggpattern"))

#set directory  
source("code.R")
#source("figures.R")
source("code_plots.R")
# Figure 5
c.plot
# Figure 6
input.plot
# Figure 7
te.plot
# Figure 8
plot
# Figure 
app.plot


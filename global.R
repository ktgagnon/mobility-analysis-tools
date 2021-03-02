library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(shinycustomloader)
library(ggplot2)
library(plotly)
library(data.table)
library(scales)
library(feather)
library(broom)
library(reactlog)

#reactlog_enable()

setwd("C:/Users/KG050542/Exec_Dash_Shiny/")

source("multigroup_dot_plot.R")
source("time_series_plot.R")
source("ridgeplot.R")
source("stackBar_plot.R")
source("cumulativePercentPlot.R")
source("time_series_plot_smoother_subplots.R")
source("multigroup_dot_plot_vert.R")
source("barplot.R")

# Sud population data
prev_long <- setDT(read_feather("sud_prevalence_long_final.feather"))
cost_wide <- setDT(read_feather("sud_cost_wide_final.feather"))

# Reference data for SUD
enr_cnts <- setDT(read_feather("enr_cnts_all.feather"))
non_sud_cost <-setDT(read_feather("non_sud_cost.feather"))

dates <- prev_long[,.("Dates"=unique(Date), "date_id"=unique(date_id))]
dates <- setorderv(dates, "Dates", 1)

memberids <- unique(prev_long[,.("member_id"=member_id)])

# Capitation, Enrollment, Spend Tab
cost_cap_mlr <- setDT(read_feather("cost_cap_enr_mlr_megs.feather"))
enr_cap_mlr <- setDT(read_feather("enr_cnts_cap_mlr_megs.feather"))
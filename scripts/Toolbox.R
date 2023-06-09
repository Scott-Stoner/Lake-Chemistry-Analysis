library(dplyr)
library(ggplot2)
library(MASS)
library(randomForest)

### TOOLBOX

# This file contains functions associated with EPA NLS 2012 Lake Data
# Scott Stoner
# 2023

## Import Function

#   This function takes arguments: string(a csv file name), a list of strings(column
# names). It imports the desired file and returns the selected columns in a dataframe.

tb_import <- function(me_columns, me_filename){
  stopifnot(is.vector(me_columns))
  stopifnot(is.character(me_filename))
  
  read.csv(me_filename) %>%
  dplyr::select(all_of(me_columns)) %>%
  invisible()
}

# test code

# filename <- "nla2012_wide_siteinfo_08232016.csv"
# column_list <- c("UID", "SITE_ID", "VISIT_NO", "DATE_COL",
#                  "SITETYPE", "SITESAMP", "DSGN12", "EVAL_NAME",
#                  "STATE", "ELEVATION", "AREA_HA", "PERIM_KM",
#                  "SIZE_CLASS", "FS_EW", "NA_L2CODE", "NA_L3CODE",
#                  "URBAN", "LAKE_ORIGIN")
# 
# import_result <- tb_import(column_list, filename)

## Summary Stats

# This function takes a dataframe and returns summary() data for each numeric col.

tb_summary <- function(me_data){
  me_data %>%
    select_if(is.numeric) %>%
    summary() %>%
    return()
}

# this function returns the r-squared value for two columns
# NOT WORKING: won't recognize input columns 

# tb_r_squared <- function(me_data, me_x, me_y) {
#   y <- {{me_y}}
#   x <- {{me_x}}
#   summary(lm(y~x,me_data))$r.squared %>%
#     return()
# }

## Scatter Plot

# These functions create scatter plots with specific alterations.

# Basic Scatter Plot
# takes a dataframe, x, and y column. 

tb_scatter <- function(me_data, me_x, me_y) {
  ggplot(me_data, aes(x = {{me_x}}, y = {{me_y}})) +
    geom_point()
}

# Basic Scatter with Linear Regression line

tb_scatter_linreg <- function(me_data, me_x, me_y) {
  ggplot(me_data, aes(x = {{me_x}}, y = {{me_y}})) +
    geom_point() +
    geom_smooth(method='lm', se=FALSE)
}

# Scatter Plot with additional input for color column.

tb_scatter_color <- function(me_data, me_x, me_y, me_color) {
  ggplot(me_data, aes(x = {{me_x}}, y = {{me_y}}, color = {{me_color}})) +
    geom_point()
}

# test code

# tb_scatter(watershed_filter_result, NLCD2006_FORESTPCT_BSN, PTL_RESULT)
# tb_scatter_linreg(watershed_filter_result, NLCD2006_FORESTPCT_BSN, PTL_RESULT)
# tb_scatter_color(watershed_filter_result, NLCD2006_FORESTPCT_BSN, PTL_RESULT, URBAN)

## Box Plot

# Creates a box plot for a single column

tb_boxplot_single <- function(me_data, me_x) {
  ggplot(me_data, aes({{me_x}})) +
    geom_boxplot()
}

# creates multiple box plots based on (me_y) data on the y axis,
# grouped by (me_x) on the x-axis

tb_boxplot_multi <- function(me_data, me_x, me_y) {
  ggplot(me_data, aes({{me_x}}, {{me_y}})) +
    geom_boxplot()
}

# test code

# tb_boxplot_single(watershed_filter_result, PTL_RESULT)
# tb_boxplot_multi(watershed_filter_result, URBAN, PTL_RESULT)

## Histogram

tb_histogram <- function(me_data, me_x) {
  me_data %>%
  ggplot() +
    geom_bar(aes({{me_x}})) +
    scale_x_binned()
}

tb_histogram_panel <- function(me_data, me_x, me_y) {
  me_data %>%
    ggplot() +
    geom_bar(aes({{me_x}})) +
    scale_x_binned() +
    facet_wrap(vars({{me_y}}))
}

# test code
# 
# tb_histogram(watershed_filter_result, PTL_RESULT)
# tb_histogram_panel(watershed_filter_result, PTL_RESULT, URBAN)

## PCA

tb_SVD <- function(me_data, me_columns) {
  me_data %>%
    dplyr::select(all_of(me_columns)) %>%
    svd() %>%
    return()
}

## Random Forest

tb_randomforest <- function(){}

# test code
print(getwd())  
test_data <- tb_import(c("UID", "NTL_RESULT", "PTL_RESULT", "PH_RESULT"), "./scripts/data/nla2012_waterchem_wide.csv")
set.seed(710)
test_forest <- randomForest(PTL_RESULT~.,
                            data = test_data,
                            ntree = 100,
                            mtry = 2)
test_forest  

dplyr::select(phyto_filtered, "TAXA_ID") %>%
  + group_by(TAXA_ID) %>%
  + summarise(id = TAXA_ID,
              count = count(TAXA_ID))

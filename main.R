######################################################################

### Start with a clean environment by removing objects in workspace
rm(list=ls())

### Setting work directory
working_directory <- base::setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
#working_directory <- base::setwd(".")

### Load Rdata
Rdata_files <- list.files(path = working_directory, pattern = "*.RData", full.names = T) #No subdirectories

if ( length(Rdata_files) >0) {
  invisible(lapply(Rdata_files,load,.GlobalEnv))
} else {
  paste(c(".RData files", "do not exist"), collapse = " ")
}

### Install required packages
source("requirements.R")

### helper/customized functions
source("helperfuns_1.R")
source("helperfuns_2.R")
source("helperfuns_plots_3.R")
source("helperfuns_plots_4.R")

### process flowchart
source("flowchart.R")

#df_loyalty_analysis <- readRDS("df_loyalty_analysis.rds")

#df_loyalty_analysis_nova <- readRDS("df_loyalty_analysis_nova.rds")

#df_loyalty_analysis_class_name <- readRDS("df_loyalty_analysis_class_name.rds")

#df_loyalty_basket_classname <- readRDS("df_loyalty_basket_classname.rds")

### Load recode file
source("load_recode_file.R")

######################################################################
## Loading and Cleaning Data

### Load supermarketA Rdata file
source("load_data_local.R")

### Data cleaning
source("cleaning.R")

### Select variables for analysis
source("analysis_data.R")
source("analysis_data_wide_nova.R")
source("analysis_data_wide_classname.R")

### Save analysis clean data set - spss file
source("save_analysis_clean_data.R")

######################################################################
## Data Profiling - Loyalty customers

### Descriptive and Inferential stats
source("customer_stats.R")
source("nova_items_stats.R")
#source("nova_items_stats_gt.R")
source("items_prop_differences_stats.R")
#source("items_prop_differences_stats_gt.R")

### Saving descriptive and inferential output
source("save_customer_stats_output.R")
source("save_nova_items_stats_output.R")
#source("save_nova_items_stats_gt_output.R")
source("save_items_prop_differences_stats_output.R")
#source("save_items_prop_differences_stats_gt_output.R")

######################################################################
## Plots- Loyalty customers

### Descriptive plots - simple and stacked bar graphs
source("customer_descriptive_plots.R")
source("nova_items_descriptive_plots.R")

### Time series plots - proportion
source("customer_month_trend_prop_plots.R")
source("customer_quarter_trend_prop_plots.R")
source("nova_month_trend_prop_plots.R")
source("nova_quarter_trend_prop_plots.R")
source("items_quantity_month_trend_prop_plots.R")
source("items_quantity_quarter_trend_prop_plots.R")

### Age-Sex Time series plots - proportion
source("customer_age_sex_month_trend_prop_plots.R")
source("customer_age_sex_quarter_trend_prop_plots.R")
source("nova_age_sex_month_trend_prop_plots.R")
source("nova_age_sex_quarter_trend_prop_plots.R")
source("items_quantity_count_age_sex_month_trend_prop_plots.R")
source("items_quantity_count_age_sex_quarter_trend_prop_plots.R")

### Age-County Time series plots - proportion
source("customer_age_county_month_trend_prop_plots.R")
source("customer_age_county_quarter_trend_prop_plots.R")
source("nova_age_county_month_trend_prop_plots.R")
source("nova_age_county_quarter_trend_prop_plots.R")
source("items_quantity_count_age_county_month_trend_prop_plots.R")
source("items_quantity_count_age_county_quarter_trend_prop_plots.R")

### Sex-County Time series plots - proportion
source("customer_sex_county_month_trend_prop_plots.R")
source("customer_sex_county_quarter_trend_prop_plots.R")
source("nova_sex_county_month_trend_prop_plots.R")
source("nova_sex_county_quarter_trend_prop_plots.R")
source("items_quantity_count_sex_county_month_trend_prop_plots.R")
source("items_quantity_count_sex_county_quarter_trend_prop_plots.R")

######################################################################
## Market Basket Analysis - Loyalty customers

source("transactions_stats.R")
source("save_transactions_stats_output.R")

### Transform data to dummy
source("basket_analysis_data.R")

### Class name rules
source("basket_classname_thresholds.R")

source("basket_classname_overall.R")
source("basket_classname_gender.R")
source("basket_classname_age.R")
source("basket_classname_year.R")
source("basket_classname_county.R")

source("basket_classname_age_gender.R")
source("basket_classname_age_county.R")
source("basket_classname_gender_county.R")

source("basket_classname_year_gender.R")
source("basket_classname_year_age.R")
source("basket_classname_year_county.R")

### Saving class name Market Basket Analysis output
source("save_classname_basket_output.R")

### Visualize class name Rules - Heat maps
source("rules_classname_heatmap.R")
source("rules_socio_by_classname_heatmap.R")
source("rules_year_by_classname_heatmap.R")

######################################################################
## Save workspace at the end without working directory path

save(list = ls(all.names = TRUE)[ls(all.names = TRUE) %in% c("df_loyalty_analysis", "df_loyalty_analysis_class_name",
                                                             "df_loyalty_basket_classname", "df_loyalty_analysis_nova")],
     file = "df_loyalty_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_prop_differences_nova"),
     file = "nova_prop_differences_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_prop_differences_items"),
     file = "items_prop_differences_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

save(list = ls(all.names = TRUE, pattern = "*_classname_rules"),
     file = "basket_classname_analysis.RData",
     envir = .GlobalEnv #parent.frame()
     )

#saveRDS(df_loyalty, file = "df_loyalty.rds")
#saveRDS(df_loyalty_analysis, file = "df_loyalty_analysis.rds")
#saveRDS(df_loyalty_analysis_nova, file = "df_loyalty_analysis_nova.rds")
#saveRDS(df_loyalty_analysis_class_name, file = "df_loyalty_analysis_class_name.rds")
#saveRDS(df_loyalty_basket_classname, file = "df_loyalty_basket_classname.rds")

######################################################################


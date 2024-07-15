# Supermarket-Project-Insights_Grocery_Purchase_Patterns
Utilizing grocery data to examine individuals' grocery purchasing patterns, their demographics and the combined effects of these factors on health-related issues.

## Background

Use of innovative ways with big data is on the rise due to advances in software development, storage capacity and computational power which have made it possible to analyze large datasets. One such industry that can utilize big data is the retail industry, specifically supermarkets.

Supermarkets on a daily basis collect sales data at the store level and grocery data at the household level through loyalty cards. Grocery data can provide an accurate measure of household or individual level dietary-related behaviors in a real-life setting. Despite the rapid growth of supermarkets in low and middle income countries (LMIC’s), the full potential of grocery data has been underutilized.

## Summary

The **"Analysis of supermarket grocery data for prediction of nutritional and health outcomes at the population level in Kenya"** project aims to create an open-access centralized database for harmonized supermarket purchase data obtained from several supermarkets located in various counties in Kenya and examine the purchasing patterns of consumers and its effect on population nutritional health outcomes. Information from this study can inform policy for effective interventions and changes to the physical environment that promote health in Kenya. 


## Setup

We are assuming you have `R Software` and `Rstudio IDE` installed. If not you can download and install [**R software**](https://www.r-project.org/) then followed by [**RStudio/Posit IDE**](https://posit.co/download/rstudio-desktop/).

## Data

The data used for analysis is available on reasonable request from the [**Study PI - Agnes Kiragga**](mailto:akiragga@aphrc.org?subject=[GitHub]%20Source%20Han%20Sans).

- **Data used for analysis:** `clean_supermarket_a.RData`

- Data anonymized at supermarket and client level

-  Transactional data with 47,748,603 records from January 2022 to December 2023 of individuals who have made purchases in a tier 2 supermarket with 10 branches spread across 5 Kenyan counties (Nakuru, Kajiado, Nairobi, Kirinyaga and Machakos).

- Transactional data further filtered by removing non-food items and non-loyalty card shoppers. The final dataset for analysis had 15,210,101 records.

- Demographic characteristics of loyalty individuals includes gender, age and location (Counties).

- Food items categorized into 103 food categories which were further classified into 4 groups according to the Nova food classification.

## Tools/Materials

1. Nova food classification guides are in the _data_ sub-folder of this repository.

2. The `supermarket_a_recode_file.xlsx` file contains: 
    
    1. MetaData about supermarkets in the _branches_ sheet.
    
    2. Guide on search terms that were used to convert the quantities to standard unit of measures in _uom_ sheet.
   
    3. Guide to how food items were categorized in the _unique_items_ sheet.
    
    4. Data dictionary of the data _rename_vars_ sheet.
   
## Run

After cloning the repository or downloading the ZIP, you also need the data files (**Data used for analysis**) in the _data_ sub-folder of _Supermarket-Project-Insights_Grocery_Purchase_Patterns_ folder.

Open `Rstudio` then set your working directory to the _Supermarket-Project-Insights_Grocery_Purchase_Patterns_ folder. 

- If you get data for this project, it is advisable to work on a `Rstudio Server` setup and run individual files.

- To run individual files, open the `main.R` script, and run from the beginning.



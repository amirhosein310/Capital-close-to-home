# load the required libraries
library(readr)
library(readxl) 
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)
library(sf)
library(foreign)

#load the data
setwd("H:/Thesis/Data")

bank_data <-  read_csv("bank_fin_9261030.csv")

#sanity checks
table(bank_data$LEGALFRM)
# drop any other accounting practices beside local GAAP
bank_data <- bank_data[bank_data$ACCPRACTICE == "Local GAAP", ]
bank_data <- bank_data[bank_data$`_40025` == "Bank", ]

#check for duplicates
bank_data <- bank_data[!duplicated(bank_data[, c("bvdid", "CLOSDATE")]), ]


# change the column order
bank_data <- bank_data[, c("NAME_INTERNAT", setdiff(names(bank_data), "NAME_INTERNAT"))]
bank_data <- bank_data[, c("LEGALFRM", setdiff(names(bank_data), "LEGALFRM"))]


# merge with kreis data to get match the zipcode to kreis code
plz_mapping <- read_excel("georef-germany-postleitzahl.xlsx")
# Drop the Geometry column
plz_mapping$Geometry <- NULL

# Rename and clean up Kreis code (district code) column
plz_mapping$ao_kreis <- plz_mapping$`Kreis code`
plz_mapping$`Kreis code` <- NULL
plz_mapping$ao_kreis <- sub("^0+", "", plz_mapping$ao_kreis)  # Remove leading zeros
plz_mapping$ao_kreis <- as.numeric(plz_mapping$ao_kreis)      # Convert to numeric

# Rename postal code column
plz_mapping$plz <- plz_mapping$`Postleitzahl / Post code`
# add former east Germany state dummy (1 forformer east german county 0 for west)
plz_mapping <- plz_mapping %>%
  mutate(former_east = ifelse(`Land code` %in% c(12, 13, 14, 15, 16), 1, 0))

#merge the mapping with the dataset to add the geolocation and also the kreis code and name
bank_data <- merge(bank_data, plz_mapping, by.x="POSTCODE", by.y="plz", all.x = TRUE, all.y = FALSE)


# subset the sparkassee from the dataset
sparkasse_data <- bank_data[grepl("sparkasse", bank_data$NAME_INTERNAT, ignore.case = TRUE), ]
# filter for the banks that have a HQ postcode
sparkasse_data <- sparkasse_data[!is.na(sparkasse_data$POSTCODE), ]
sparkasse_data <- sparkasse_data[sparkasse_data$`_40025` == "Bank", ]
# calculate share of non interest rev.
sparkasse_data <- sparkasse_data %>%
  mutate(share_non_interest_inc = `_193600` * 100 / `_194400`)

#read gdp per kreis data
total_gdp <- read.csv("total_gdp.csv")
total_gdp <- total_gdp[!is.na(total_gdp$NUTS.3), ]
total_gdp$Regional.schlüssel[total_gdp$Regional.schlüssel == 2] <- 2000
total_gdp$Regional.schlüssel[total_gdp$Regional.schlüssel == 11] <- 11000
colnames(total_gdp)[colnames(total_gdp) == "Regional.schlüssel"] <- "ao_kreis"
#convert cols to numerical
cols_to_convert <- grep("^X[0-9]{4}$", colnames(total_gdp), value = TRUE)
total_gdp[cols_to_convert] <- lapply(total_gdp[cols_to_convert], function(x) as.numeric(gsub(" ", "", x)))

# calculate the growth rate of the gdp per capita for each kreis
year_columns <- grep("^X[0-9]{4}$", colnames(total_gdp), value = TRUE)


#read gdp per capita data
gdp_data <- read.csv("GDP_per_capita.csv")
#delete aggregated cells
gdp_data <- gdp_data[!is.na(gdp_data$NUTS.3), ]
#adjust Berlin and Hamburg codes 
gdp_data$Regional.schlüssel[gdp_data$Regional.schlüssel == 2] <- 2000
gdp_data$Regional.schlüssel[gdp_data$Regional.schlüssel == 11] <- 11000
colnames(gdp_data)[colnames(gdp_data) == "Regional.schlüssel"] <- "ao_kreis"
#convert cols to numerical
cols_to_convert <- grep("^X[0-9]{4}$", colnames(gdp_data), value = TRUE)
gdp_data[cols_to_convert] <- lapply(gdp_data[cols_to_convert], function(x) as.numeric(gsub(" ", "", x)))

# calculate the growth rate of the gdp per capita for each kreis
year_columns <- grep("^X[0-9]{4}$", colnames(gdp_data), value = TRUE)

# Create an empty dataframe for growth rates
growth_rate_data <- gdp_data[, year_columns]

# Calculate the growth rates for each year
for (i in 2:length(year_columns)) {
  growth_rate_data[, i] <- (gdp_data[, year_columns[i]] - gdp_data[, year_columns[i - 1]]) / 
    gdp_data[, year_columns[i - 1]] * 100
}

# Rename columns to indicate growth rate
colnames(growth_rate_data) <- paste0("Growth_", year_columns)
growth_rate_data <- cbind(ao_kreis = gdp_data$ao_kreis, growth_rate_data)
# Remove the Growth_X1992 column
growth_rate_data <- growth_rate_data[, !colnames(growth_rate_data) %in% "Growth_X1992"]


# load sparkasee branches data scraped from their website
sp_branches <- read.csv("sparkasse_branches.csv")
sp_branches <- sp_branches[sp_branches$Address != "N/A", ]
# extract the zipcodes
sp_branches$ZIP_Code <- sub(".*\\b(\\d{5})\\b.*", "\\1", sp_branches$Address)

# join to get the kreis code
sp_branches <- sp_branches %>%
  inner_join(plz_mapping, by = c("ZIP_Code" = "plz"))

# delete duplicates
sp_branches <- sp_branches[!duplicated(sp_branches[, c("Name.x", "Address")]), ]
#delete unrelated
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB50917", ] #Verband
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB59028", ] #Verband
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB40607", ] #Verband
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB40185", ] #Verband
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB97746", ] #Hochschule sparkasee
sparkasse_data <- sparkasse_data[sparkasse_data$bvdid != "DEFEB97411", ] #1822direkt Gesellschaft der Frankfurter Sparkasse mbH
# delete Bausparkassen
sparkasse_data <- sparkasse_data[!grepl("bausparkasse", sparkasse_data$NAME_INTERNAT, ignore.case = TRUE), ]


# Split geo_point_2d into latitude and longitude
sp_branches <- sp_branches %>%
  separate(geo_point_2d, into = c("Latitude", "Longitude"), sep = ", ", convert = TRUE)

#delete SB Centers, immobilien centers and other non branches
sp_branches <- sp_branches[!grepl("SB-Center|SB-Filiale|SB Center|Immobilien|SB-Stelle|Video-Service|Sparkassemobile|SB-Standort|Sparkassenmobil|Mobile|SB-Angebot|SB-Service ", sp_branches$Name.x), ]


# plot sparkasse branches locations
ggplot(data = sp_branches, aes(x = Longitude, y = Latitude)) +
  geom_point() +
  labs(title = "Sparkasee Branches Geographic Points", x = "Longitude", y = "Latitude") +
  theme_minimal()


#read population data
population_data <- read.csv("population_DE.csv")
population_data <- population_data[!is.na(population_data$NUTS.3), ]
#adjust Berlin and Hamburg codes 
population_data$Regional.schlüssel[population_data$Regional.schlüssel == 2] <- 2000
population_data$Regional.schlüssel[population_data$Regional.schlüssel == 11] <- 11000
colnames(population_data)[colnames(population_data) == "Regional.schlüssel"] <- "ao_kreis"


# Define all possible values of 'ao_kreis'
all_ao_kreis <- unique(sp_branches$ao_kreis)

# Count occurrences using table
sp_branches_summary <- as.data.frame(table(factor(sp_branches$ao_kreis, levels = all_ao_kreis)))

# Rename columns for clarity
colnames(sp_branches_summary) <- c("ao_kreis", "SP_branch_count")

#merge with population data of 2022
sp_branches_summary <- merge(sp_branches_summary, population_data[, c("ao_kreis", "X2022")], by = "ao_kreis", all.x = TRUE)
# branches per 1000 inhabitants in each kreis
# Ensure SP_branch_count and X2022 are numeric
sp_branches_summary$SP_branch_count <- as.numeric(sp_branches_summary$SP_branch_count)
sp_branches_summary$X2022 <- as.numeric(sp_branches_summary$X2022)

# Add the new column 'branch_per_population'
sp_branches_summary$branch_per_population <- sp_branches_summary$SP_branch_count / sp_branches_summary$X2022 #to do > there are NA here should fix

# temp plots
#### Plots for Landkreis #### PLOT #######

# Read the GeoJSON file into an sf object
kreis_map <- st_read("georef-germany-kreis@public.geojson")
# Set a new column for color, defaulting all to white
kreis_map$color <- "#FFFFFF"

wdt <- averaged_dataframe       #change HERE for the plot
wdt$SCHL_ID <- wdt$ID        #change HERE for the plot
wdt$Slope_Coefficient <- wdt$avg_equity_ratio #change HERE for the plot

# Convert ao_kreis from factor to integer
wdt$ID <- as.integer(as.character(wdt$ID))



kreis_map$krs_code <- as.integer(sapply(kreis_map$krs_code, `[`, 1))
kreis_map_2 <- left_join(kreis_map, wdt, by = c("krs_code" = "ID"))


ggplot() +
  geom_sf(data = kreis_map_2, aes(fill = Slope_Coefficient)) +
  scale_fill_viridis(option = "magma", begin = 0, end = 1, direction = 1, limits = c(quantile(kreis_map_2$Slope_Coefficient, probs = 0.01, na.rm = TRUE), quantile(kreis_map_2$Slope_Coefficient, probs = 0.99, na.rm = TRUE))) +
  ggtitle("Average loan per GDP") +
  theme_linedraw() +
  theme(legend.position = "right",
        legend.title = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm")) +
  guides(fill = guide_colorbar(barwidth = 0.5, barheight = 10)) +
  labs(fill = "")

## end of plot ################3 


# subsetting the big inter regional banks and build
big_banks <- bank_data[bank_data$CATEGORY_OF_COMPANY == "VERY LARGE COMPANY", ]
# delete sparkasee banks 
big_banks <- big_banks[!big_banks$bvdid %in% sparkasse_data$bvdid, ]


# create a new df for yearly sparkasse data

#1. get the total Tier1 capital per each year
yearly_totals_sp <- sparkasse_data %>%
  group_by(CLOSDATE_year) %>%
  summarise(Total_194900 = sum(`_194900`, na.rm = TRUE)) %>%
  arrange(CLOSDATE_year)

#2. share of Tier1 capital for each year
sparkasse_data <- sparkasse_data %>%
  group_by(CLOSDATE_year) %>%
  mutate(Year_Total_194900 = sum(`_194900`, na.rm = TRUE),  # Calculate year total
         Share_Tier1= `_194900` / Year_Total_194900) %>%  # Calculate the share
  ungroup()

# average Tier 1 ccapital for each kreis
average_share_tier1_sp <- sparkasse_data %>%
  group_by(ao_kreis) %>%
  summarise(Average_Share_Tier1 = mean(Share_Tier1, na.rm = TRUE)) %>%
  arrange(ao_kreis)


######## PANEL ESTIMATION #########


# constructing the main dataframe to conduct the panel estimation
# Create the main dataframe
main_dataframe <- sparkasse_data %>%
  select(ID = ao_kreis, Year = CLOSDATE_year, ROA, ROE, total_assets = `_191100`,  
         tier1_ratio = `_194700`, equity_ratio = `_195100`,bvdid =  bvdid, former_east, share_non_interest_inc, loans =  `_190000`) %>%  # Rename and select relevant columns
  arrange(ID, Year)  # Sort by ID and Year

#delete duplicates
main_dataframe <- main_dataframe[!duplicated(main_dataframe), ]

#merge the gdp growth data (Yit)
# Step 1: Reshape the growth_rate_data into long format
gdp_growth_long <- growth_rate_data %>%
  pivot_longer(
    cols = starts_with("Growth_"),
    names_to = "Year",
    names_prefix = "Growth_X",
    values_to = "GDP_Growth"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert year to numeric for matching


# Step 2: Merge GDP growth data with the main dataframe
main_dataframe <- main_dataframe %>%
  left_join(gdp_growth_long, by = c("ID" = "ao_kreis", "Year" = "Year"))

# Merge GDP per capita to the main df
# make the df long fomrat
gdp_long <- gdp_data %>%
  pivot_longer(
    cols = starts_with("X"),          # Select year columns (e.g., X1992, X1994, ...)
    names_to = "Year",                # Create a "Year" column
    names_prefix = "X",               # Remove the "X" prefix from year names
    values_to = "GDP_per_capita"      # Name of the new value column
  ) %>%
  mutate(Year = as.numeric(Year))     # Convert year to numeric for merging
#merge
main_dataframe <- main_dataframe %>%
  mutate(ID = as.numeric(ID))
gdp_long <- gdp_long %>%
  select(ao_kreis, Year, GDP_per_capita)
main_dataframe <- main_dataframe %>%
  left_join(gdp_long, by = c("ID" = "ao_kreis", "Year" = "Year"))




#merge populatin data
# Step 1: Select only relevant columns from population_data (ao_kreis and population years)
population_filtered <- population_data %>%
  select(ao_kreis, starts_with("X"))  # Keep only ID and year columns

# Step 2: Reshape the population_data into long format
population_long <- population_filtered %>%
  pivot_longer(
    cols = starts_with("X"),          # Select year columns (e.g., X1992, X1993, ...)
    names_to = "Year",
    names_prefix = "X",               # Remove "X" prefix from year names
    values_to = "Population"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric for merging

# Step 3: Merge the filtered population data with the main dataframe
main_dataframe <- main_dataframe %>%
  left_join(population_long, by = c("ID" = "ao_kreis", "Year" = "Year"))
main_dataframe$Population <- as.numeric(gsub(" ", "", main_dataframe$Population))

#merge the total GDP data
# Step 1: Select only relevant columns from population_data (ao_kreis and population years)
gdp_filtered <-total_gdp %>%
  select(ao_kreis, starts_with("X"))  # Keep only ID and year columns

# Step 2: Reshape the population_data into long format
gdp_long <- gdp_filtered %>%
  pivot_longer(
    cols = starts_with("X"),          # Select year columns (e.g., X1992, X1993, ...)
    names_to = "Year",
    names_prefix = "X",               # Remove "X" prefix from year names
    values_to = "gdp"
  ) %>%
  mutate(Year = as.numeric(Year))  # Convert Year to numeric for merging

# Step 3: Merge the filtered population data with the main dataframe
main_dataframe <- main_dataframe %>%
  left_join(gdp_long, by = c("ID" = "ao_kreis", "Year" = "Year"))
main_dataframe$gdp <- as.numeric(gsub(" ", "", main_dataframe$gdp))

# take care of multiple banks working in one kreis
# Resolve duplicates by grouping and aggregating
main_dataframe <- main_dataframe %>%
  group_by(ID, Year) %>%
  summarise(
    across(where(is.numeric) & !total_assets, mean, na.rm = TRUE),  # Mean for numeric columns except total_assets
    total_assets = sum(total_assets, na.rm = TRUE),                # Sum for total_assets
    bvdid = paste(unique(bvdid), collapse = ";"),                  # Concatenate unique bvdid values
    .groups = "drop"                                               # Ungroup after summarising
  )





# add länder CPI to the main df
CPI_data <- read.csv("CPI.csv")
# Correct the incorrect state names in CPI_data
CPI_data <- CPI_data %>%
  mutate(`ï..state` = case_when(
    `ï..state` == "ThÃ¼ringen" ~ "Thüringen",
    `ï..state` == "Baden-WÃ¼rttemberg" ~ "Baden-Württemberg",
    TRUE ~ `ï..state`  # Keep other values unchanged
  ))




# add ao_kreis code
CPI_data <- CPI_data %>%
  left_join(
    plz_mapping %>% select(Land_name = `Land name`, ao_kreis),
    by = c("ï..state" = "Land_name")
  )
# make it long format
# Convert all year columns to numeric
CPI_data <- CPI_data %>%
  mutate(across(starts_with("X"), ~ as.numeric(as.character(.))))

# Convert CPI_data to long format
CPI_data <- CPI_data %>%
  pivot_longer(
    cols = starts_with("X"),          # Select year columns (e.g., X1995, X1996, ...)
    names_to = "Year",                # Name of the new column for years
    names_prefix = "X",               # Remove "X" prefix from year names
    values_to = "CPI"                 # Name of the new column for CPI values
  ) %>%
  mutate(Year = as.numeric(Year))     # Convert Year to numeric for merging

CPI_data$`ï..state` <- NULL
CPI_data <- CPI_data[!duplicated(CPI_data), ]

#Merge CPI_long with main_dataframe, keeping only the CPI column
main_dataframe <- main_dataframe %>%
  left_join(CPI_data %>% select(ao_kreis, Year, CPI), 
            by = c("ID" = "ao_kreis", "Year" = "Year"))
#calculate the cpi change
main_dataframe <- main_dataframe %>%
  group_by(ID) %>%
  arrange(Year) %>%  # Ensure data is ordered by Year within each ID
  mutate(
    CPI_pct_change = ifelse(is.na(lag(CPI)), NA, (CPI - lag(CPI)) / lag(CPI) * 100)
  ) %>%
  ungroup()


# add loan per gdp column
main_dataframe$loan_per_gdp <- main_dataframe$loans / main_dataframe$gdp





annual_cpi <- read_csv("61111-0001_en.csv")


#output for STATA 
write.dta(main_dataframe, "main_data.dta")

# add missing points into the main data frame

main_dataframe <- read.csv("main_data.csv")
missing_kreis <- read.csv("missing_dp_kreis.csv")

# Iterate through each row in missing_kreis to replicate data
for (i in 1:nrow(missing_kreis)) {
  # Get the missing ao_kreis and corresponding hq_kreis
  missing_ao_kreis <- missing_kreis$ao_kreis.x[i]
  hq_kreis <- missing_kreis$hq_kreis[i]
  
  # Filter rows in main_dataframe for the hq_kreis
  hq_data <- main_dataframe[main_dataframe$ID == hq_kreis, ]
  
  # If hq_data is not empty, proceed
  if (nrow(hq_data) > 0) {
    # Replace the ao_kreis value with the missing ao_kreis
    hq_data$ID <- missing_ao_kreis
    
    # Ensure column order and names match before appending
    hq_data <- hq_data[, colnames(main_dataframe)]
    
    # Append the updated rows to main_dataframe
    main_dataframe <- rbind(main_dataframe, hq_data)
  }
}
main_dataframe <- main_dataframe[rowSums(is.na(main_dataframe)) < ncol(main_dataframe), ]
main_dataframe <- main_dataframe[!is.na(main_dataframe$X), ]


averaged_dataframe <- main_dataframe %>%
  group_by(ID) %>%
  summarise(across(everything(), ~ mean(.x, na.rm = TRUE), .names = "avg_{.col}"))


# add branch count to the main dataframe
main_dataframe <- main_dataframe %>%
  mutate(ID = as.character(ID))

sp_branches_summary <- sp_branches_summary %>%
  mutate(ao_kreis = as.character(ao_kreis))

# Merge the datasets
main_dataframe_with_branches <- main_dataframe %>%
  left_join(sp_branches_summary, by = c("ID" = "ao_kreis"))

#subset the data based on the access to fiance, based on the number of branches per location
# Calculate the median of branch_per_population
median_branch_per_population <- median(main_dataframe_with_branches$branch_per_population, na.rm = TRUE)

# Subset the data into two groups
above_median <- main_dataframe_with_branches %>%
  filter(branch_per_population > median_branch_per_population)

below_median <- main_dataframe_with_branches %>%
  filter(branch_per_population <= median_branch_per_population)

# export the data for the stata 
write.dta(above_median, "above_median.dta")
write.dta(below_median, "below_median.dta")



# add interaction term
# Calculate the initial GDP_per_Capita for each ID
initial_gdp_per_capita <- main_dataframe %>%
  group_by(ID) %>%
  filter(!is.na(GDP_per_capita)) %>%    # Ensure we only consider non-NA values
  slice_min(Year) %>%                  # Select the row with the earliest year
  select(ID, initial_GDP_per_capita = GDP_per_capita)
# Add initial GDP_per_Capita to the main dataframe
main_dataframe <- main_dataframe %>%
  left_join(initial_gdp_per_capita, by = "ID")
# Add the interaction term to the dataframe
main_dataframe <- main_dataframe %>%
  mutate(interaction_GDP_ROA = initial_GDP_per_capita * ROA)
main_dataframe <- main_dataframe %>%
  mutate(interaction_GDP_ROE = initial_GDP_per_capita * ROE)

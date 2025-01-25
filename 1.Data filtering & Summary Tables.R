install.packages('haven')
install.packages('knitr')
install.packages('officer')
install.packages('flextable')
library(haven)
library(dplyr)
library(knitr)
library(officer)
library(flextable)


individual_data <- read_sas ("C:\\Users\\viraj\\Downloads\\BDIR7RFL.SAS7BDAT")
br_data <- read_sas ("C:\\Users\\viraj\\Downloads\\BDBR7RFL.SAS7BDAT")

dim(individual_data)  
dim(br_data)

# Subset individual data
individual_subset <- individual_data[, c('CASEID', 'V130', 'V212', 'V119', 'V106', 'V139', 'V140', 'V190', 'V113', 'V116', 'V136', 'V127', 'V128', 'V129', 'V137','V501')]

# Check if these variables exist in the dataset
c('V130', 'V212', 'V119', 'V106', 'V139', 'V140', 'V190', 'V113', 'V116', 'V136', 'V127', 'V128', 'V129', 'V137', 'V501') %in% colnames(individual_data)

br_subset <- br_data[, c('CASEID', 'M19', 'B4', 'B5', 'B6', 'BORD', 'M15', 'M4')]

c('M19', 'B4', 'B5', 'B6', 'BORD', 'M15', 'M4') %in% colnames(br_data)

dim(individual_subset)  
dim(br_subset)

head(individual_subset)
head(br_subset)

# Summary of missing values for Individual Recode dataset
colSums(is.na(individual_subset))

# Summary of missing values for Birth Recode dataset
colSums(is.na(br_subset))

# Proportion of missing values for Individual Recode dataset
colMeans(is.na(individual_subset)) * 100  # Percentage

# Proportion of missing values for Birth Recode dataset
colMeans(is.na(br_subset)) * 100

#filtering rows with missing values for M19, B6, M4
br_filtered <- br_subset %>%
  filter(
    !is.na(M19) | !is.na(B6),                     # Keep rows where at least one is non-missing
    !(M19 %in% c(9996, 9998)),                    # Exclude invalid birth weight codes
    !(B6 %in% c(199, 299, 399, 997, 998))         # Exclude invalid age at death codes
  )

br_filtered$M19[is.na(br_filtered$M19)] <- median(br_filtered$M19, na.rm = TRUE)
br_filtered$B6[is.na(br_filtered$B6)] <- median(br_filtered$B6, na.rm = TRUE)

mode_impute <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}
br_filtered$M4[is.na(br_filtered$M4)] <- mode_impute(br_filtered$M4)

# Calculate the mode of M15
mode_impute_M15 <- as.numeric(names(sort(table(br_filtered$M15), decreasing = TRUE))[1])

# Define the relevant condition (assuming B5 == 1 means "child is alive")
relevant_condition <- br_filtered$B5 == 1

# For relevant cases (where M15 is missing), impute mode
br_filtered$M15[is.na(br_filtered$M15) & relevant_condition] <- mode_impute_M15

# For non-relevant cases (non-birthing individuals), assign 'Not Applicable'
br_filtered$M15[is.na(br_filtered$M15) & !relevant_condition] <- 99

# Remove rows where V212 is missing
individual_subset <- individual_subset[!is.na(individual_subset$V212), ]

colSums(is.na(br_filtered))
colSums(is.na(individual_subset))

dim(individual_subset)
dim(br_filtered)

# Perform an inner join on the CASEID column
final_dataset <- br_filtered %>%
  inner_join(individual_subset, by = "CASEID")

# Check the dimensions and ensure the merge was successful
dim(final_dataset)

# Check the first few rows of the final dataset to ensure it's merged correctly
head(final_dataset)

# Ensure no missing data in the merged dataset
colSums(is.na(final_dataset))

#Recode for the Religion variable
final_dataset <- final_dataset %>%
  mutate(V130_recode = case_when(
    V130 == 1 ~ "Muslim",
    V130 %in% c(2, 3, 4, 96) ~ "Non-Muslim",
    TRUE ~ "Unknown"
  ))

# Create summary table for the recoded variable
summary_V130 <- final_dataset %>%
  count(V130_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))

# Print the summary table
print(summary_V130)

# Recode V119 (Electricity)
final_dataset <- final_dataset %>%
  mutate(V119_recode = case_when(
    V119 == 1 ~ "Yes",
    V119 == 0 ~ "No",
    V119 == 7 ~ "No",
    TRUE ~ "Unknown"  # Catch any unexpected values
  ))

summary_V119 <- final_dataset %>%
  count(V119_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V119)

# Recode V139 (De jure region of usual residence)
final_dataset <- final_dataset %>%
  mutate(V139_recode = case_when(
    V139 == 0 ~ "Abroad",
    V139 == 1 ~ "Barisal",
    V139 == 2 ~ "Chittagong",
    V139 == 3 ~ "Dhaka",
    V139 == 4 ~ "Khulna",
    V139 == 5 ~ "Mymensingh",
    V139 == 6 ~ "Rajshahi",
    V139 == 7 ~ "Rangpur",
    V139 == 8 ~ "Sylhet",
    V139 == 97 ~ "Not dejure resident",
    TRUE ~ "Unknown"
  ))

summary_V139 <- final_dataset %>%
  count(V139_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
 
print(summary_V139)

# Recode V140 (De jure type of place of usual residence)
final_dataset <- final_dataset %>%
  mutate(V140_recode = case_when(
    V140 == 0 ~ "Abroad",
    V140 == 1 ~ "Urban",
    V140 == 2 ~ "Rural",
    V140 == 7 ~ "Rural",
    TRUE ~ "Unknown"
  ))

summary_V140 <- final_dataset %>%
  count(V140_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
 
print(summary_V140)
                
# Recode V190 (Wealth index)
final_dataset <- final_dataset %>%
  mutate(V190_recode = case_when(
    V190 == 1 ~ "Poorest",
    V190 == 2 ~ "Poorer",
    V190 == 3 ~ "Middle",
    V190 == 4 ~ "Richer",
    V190 == 5 ~ "Richest",
    TRUE ~ "Unknown"

  ))

summary_V190 <- final_dataset %>%
  count(V190_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V190)

# Recode V136 (Total number of household members)
final_dataset <- final_dataset %>%
  mutate(V136_recode = case_when(
    V136 %in% 1:3 ~ "Small (1-3 people)",
    V136 %in% 4:6 ~ "Medium (4-6 people)",
    V136 >= 7 ~ "Large (7+ people)",
    TRUE ~ "Unknown"
  ))

summary_V136 <- final_dataset %>%
  count(V136_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V136)

# Recode V113 (Main source of drinking water ) into 'Piped Water' and 'Non Piped Water'
final_dataset  <- final_dataset  %>%
  mutate(V113_recode = case_when(
    V113 %in% c(10, 11, 12, 13, 14) ~ "Piped Water",
    V113 %in% c(20, 21, 30) ~ "Tubewell/Borehole",
    V113 %in% c(31, 32, 40, 41, 42, 43, 51, 61, 62, 71, 96, 97) ~ "Non Piped Water",
    TRUE ~ as.character(V113)  # Preserve existing categories for other values
  ))

summary_V113 <- final_dataset %>%
  count(V113_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V113)

# Recode V116 (Type of toilet facility ) into 'Flush Toilet', 'Pit Latrine', and 'Others'
final_dataset <- final_dataset %>%
  mutate(V116_recode = case_when(
    V116 %in% c(10, 11, 12, 13, 14, 15) ~ "Flush Toilet",
    V116 %in% c(20, 21, 22, 23) ~ "Pit Latrine",
    V116 %in% c(30, 31, 41, 42, 43, 97) ~ "Others",
    TRUE ~ "Others"
  ))

summary_V116 <- final_dataset %>%
  count(V116_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V116)

# Recode V127 (Main Floor Material) into 'Earth/Mud', 'Wood', and 'Others'
final_dataset <- final_dataset %>%
  mutate(V127_recode = case_when(
    V127 %in% c(10, 11, 12, 20) ~ "Earth/Mud",
    V127 %in% c(21, 22, 30, 31, 32, 35) ~ "Wood",
    TRUE ~ "Cement/Ceramic"
  ))

summary_V127 <- final_dataset %>%
  count(V127_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V127)

# Recode V128 (Main Wall Material) into 'Mud/Stone' and 'Others'
final_dataset <- final_dataset %>%
  mutate(V128_recode = case_when(
    V128 %in% c(10, 11, 12, 13, 20, 21, 22) ~ "Mud/Stone", 
    V128 %in% c(31, 32, 33, 34, 35) ~ "Cement",
    TRUE ~ "Others"
  ))

summary_V128 <- final_dataset %>%
  count(V128_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V128)

# Recode V129 (Main Roof Material) into 'Natural Materials', 'Finished', and 'Others'
final_dataset <- final_dataset %>%
  mutate(V129_recode = case_when(
    V129 %in% c(10, 11, 12, 13, 20, 21, 22, 23, 24) ~ "Natural Materials",
    V129 %in% c(30, 32, 33, 34, 36, 96) ~ "Finished",
    V129 %in% c(31) ~ "Metal",
    V129 %in% c(35) ~ "Cement",
    TRUE ~ "Others"
  ))

summary_V129 <- final_dataset %>%
  count(V129_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V129)

# Recode V137 (Number of children 5 and under in the household)
final_dataset <- final_dataset %>%
  mutate(V137_recode = case_when(
    V137 == 0 ~ "0",               # Keep 0 as is
    V137 == 1 ~ "1",               # Keep 1 as is
    V137 == 2 ~ "2",               # Keep 2 as is
    V137 == 3 ~ "3",               # Keep 3 as is
    V137 > 3 ~ ">3",               # Combine all counts greater than 3 into ">3"
    is.na(V137) ~ "NA"             # Preserve NA values
  ))

summary_V137 <- final_dataset %>%
  count(V137_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
 
print(summary_V137)

# Recode V106 (Mother's Education)
final_dataset <- final_dataset %>%
  mutate(V106_recode = case_when(
    V106 == 0 ~ "No education",
    V106 == 1 ~ "Primary",
    V106 == 2 ~ "Secondary",
    V106 == 3 ~ "Higher",
    TRUE ~ NA_character_  # Preserve any NA values
  ))

summary_V106 <- final_dataset %>%
  count(V106_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
 
print(summary_V106)

# Recode V212 (Age of respondent at 1st birth) into age groups
final_dataset <- final_dataset %>%
  mutate(V212_recode = case_when(
    V212 < 16 ~ "<16",                  # Ages less than 16
    V212 >= 16 & V212 <= 18 ~ "16-18",  # Ages 16-18
    V212 >= 19 & V212 <= 21 ~ "19-21",  # Ages 19-21
    V212 >= 22 & V212 <= 25 ~ "22-25",  # Ages 22-25
    V212 > 25 ~ "26+",                  # Ages above 25
    is.na(V212) ~ "NA"                  # Preserve missing values
  ))

summary_V212 <- final_dataset %>%
  count(V212_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V212)

# Recode V501 (Current Marital Status)
final_dataset <- final_dataset %>%
  mutate(V501_recode = case_when(
    V501 == 0 ~ "Never in union",
    V501 == 1 ~ "Married",
    V501 == 2 ~ "Living with partner",
    V501 == 3 ~ "Widowed",
    V501 == 4 ~ "Divorced",
    V501 == 5 ~ "No longer living together/separated",
    TRUE ~ "Not Available"  # To handle NAs or other values
  ))

summary_V501 <- final_dataset %>%
  count(V501_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_V501)

#Recode M19 Child birth weight
final_dataset <- final_dataset %>%
  mutate(M19_category = case_when(
    M19 < 2500 ~ "Low Birth Weight",
    M19 >= 2500 & M19 < 4000 ~ "Normal Birth Weight",
    M19 >= 4000 ~ "High Birth Weight",
    TRUE ~ "Unknown" # In case of unexpected values
  ))

summary_M19 <- final_dataset %>%
  count(M19_category) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_M19)

#Recode B4 (Child sex)
final_dataset <- final_dataset %>%
  mutate(B4_recode = case_when(
    B4 == 1 ~ "Male",    
    B4 == 2 ~ "Female",   
    TRUE ~ NA_character_  
  ))

summary_B4 <- final_dataset %>%
  count(B4_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_B4)

# Recode B5 so that 0 = Alive and 1 = Dead
final_dataset <- final_dataset %>%
  mutate(B5 = case_when(
    B5 == 0 ~ 1,  # Dead becomes 1
    B5 == 1 ~ 0   # Alive becomes 0
  ))

# Create a new labeled variable for B5
final_dataset <- final_dataset %>%
  mutate(B5_Labeled = factor(B5, levels = c(0, 1), labels = c("Alive", "Dead")))

summary_B5 <- final_dataset %>%
  count(B5) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))

summary_B5_Labeled <- final_dataset %>%
  count(B5_Labeled) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))

print(summary_B5)
print(summary_B5_Labeled)

# Recode B6 into meaningful categories
final_dataset <- final_dataset %>%
  mutate(
    B6_recode = case_when(
      B6 == 100 ~ "Day of birth",
      B6 >= 101 & B6 <= 199 ~ "Died within the first month",
      B6 >= 201 & B6 <= 299 ~ "Died between 1 and 11 months",
      B6 >= 301 & B6 <= 399 ~ "Died at 1 year or older",
      TRUE ~ NA_character_  # Catch-all for any unexpected values
    )
  )

summary_B6 <- final_dataset %>%
  count(B6_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))

print(summary_B6)

# Recode BORD in merged_data
final_dataset$BORD_recode <- with(final_dataset, 
                                ifelse(BORD == 1, '1',                      # Recode BORD 1 as '1'
                                ifelse(BORD %in% 2:4, '2,3,4',              # Recode BORD 2, 3, 4 as '2,3,4'
                                ifelse(BORD >= 5, '5 and more', NA))))      # Recode BORD 5 and above as '5 and more'

summary_BORD <- final_dataset %>%
  group_by(BORD_recode) %>%
  summarize(Count = n(), 
            Percentage = round((n() / nrow(merged_data)) * 100, 2))
print(summary_BORD)

# Recode the M15 Place of Delivery variable
final_dataset <- final_dataset %>%
  mutate(M15_recode = case_when(
    M15 %in% c(10, 11) ~ 'Home',
    M15 %in% c(20, 21, 22, 23, 24, 25, 26, 27, 28) ~ 'Public Sector',
    M15 %in% c(30, 31, 32, 33, 36) ~ 'Private Sector',
    M15 %in% c(40, 41, 42) ~ 'NGO/Delivery Hut',
    TRUE ~ 'Other'  # Handle any other values (including 96) as 'Other'
  ))

summary_M15 <- final_dataset %>%
  count(M15_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_M15)

# Recode M4 (Duration of breastfeeding)
final_dataset <- final_dataset %>%
  mutate(M4_recode = case_when(
    M4 == 93 ~ "Ever breastfed, not currently breastfeeding",
    M4 == 94 ~ "Never breastfed",
    M4 == 95 ~ "Still breastfeeding",
    M4 == 96 ~ "Breastfed until died",
    M4 == 97 ~ "Inconsistent",
    M4 == 98 ~ "Don't know",
    TRUE ~ "Not Available"  # To handle NAs or other values
  ))

summary_M4 <- final_dataset %>%
  count(M4_recode) %>%
  mutate(Percentage = round((n / sum(n)) * 100, 2))
print(summary_M4)

# Define a function to create a Word table
save_to_word <- function(doc, data, title) {
  # Add a title
  doc <- doc %>%
    body_add_par(title, style = "heading 1")
  
  # Convert the data frame to a table and add to the Word document
  doc <- doc %>%
    body_add_table(value = data, style = "table_template")
  
  return(doc)
}

# Initialize a new Word document
doc <- read_docx()

# Add the summary tables
doc <- save_to_word(doc, summary_V130, "Religion (V130)")
doc <- save_to_word(doc, summary_V119, "Household has: electricity (V119)")
doc <- save_to_word(doc, summary_V139, "Region (V139)")
doc <- save_to_word(doc, summary_V140, "Type of Place of residence (V140)")
doc <- save_to_word(doc, summary_V190, "Wealth index (V190)")
doc <- save_to_word(doc, summary_V136, "Number of household members (V136)")
doc <- save_to_word(doc, summary_V113, "Source of drinking water (V113)")
doc <- save_to_word(doc, summary_V116, "Type of toilet facility (V116)")
doc <- save_to_word(doc, summary_V127, "Main floor material (V127)")
doc <- save_to_word(doc, summary_V128, "Main wall material (V128)")
doc <- save_to_word(doc, summary_V129, "Main roof material (V129)")
doc <- save_to_word(doc, summary_V137, "Number of children 5 and under (V137)")
doc <- save_to_word(doc, summary_V106, "Highest educational level (V106)")
doc <- save_to_word(doc, summary_V212, "Age of respondent at 1st birth (V212)")
doc <- save_to_word(doc, summary_V501, "Current Marital Status (V501)")
doc <- save_to_word(doc, summary_M19, "Birth Weight (M19)")
doc <- save_to_word(doc, summary_B4, "Child Sex (B4)")
doc <- save_to_word(doc, summary_B5, "Child is alive (B5)")
doc <- save_to_word(doc, summary_B6, "Age at Death (B6)")
doc <- save_to_word(doc, summary_BORD, "Birth Order (BORD)")
doc <- save_to_word(doc, summary_M15, "Place of Delivery (M15)")
doc <- save_to_word(doc, summary_M4, "Duration of Breastfeeding (M4)")

# Save the document
print(doc, target = "Summary_Tables.docx")














# Load the necessary library
library(readr)

# Specify the file path
file_path <- 'D:/UOM/Statistics/SI Project/Final Datset/229415U_data_file_1.csv'

# Load the CSV file into a data frame
df <- read_csv(file_path)

# Display the first 5 rows of the data frame
print(head(df, 5))

# Display the shape of the data frame
print(dim(df))

# Display the structure of the data frame
print(str(df))


# Load the necessary libraries
library(readr)
library(ggplot2)
library(dplyr)

#Survey data - Gender distribution

# Count the number of each gender
gender_counts <- df %>%
  group_by(Gender) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a pie chart
ggplot(gender_counts, aes(x = "", y = Count, fill = Gender)) +
  geom_bar(width = 1, stat = "identity") +
  coord_polar("y", start = 0) +
  theme_void() +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), position = position_stack(vjust = 0.5)) +
  labs(fill = "Gender") +
  ggtitle("Gender Distribution") +
  theme(plot.title = element_text(hjust = 0.5, color = "blue", size = 20))


#Gender Distribution with Relationship to Household

# Count the number of each gender for each relationship to household
gender_relationship_counts <- df %>%
  group_by(Gender, `Relationship to Household`) %>%
  summarise(Count = n())

# Create a stacked bar chart
ggplot(gender_relationship_counts, aes(x = `Relationship to Household`, y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Relationship to Household", y = "Count", fill = "Gender", title = "Gender Distribution by Relationship to Household") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 16, color = "black"))


# Survey data - Respondent location distribution

# Convert the district names to title case
df <- df %>%
  mutate(District = tools::toTitleCase(District))

# Now you can proceed with the previous code to create the bar chart
district_counts <- df %>%
  group_by(District) %>%
  summarise(Count = n()) %>%
  arrange(Count)

# Create a horizontal bar chart
ggplot(district_counts, aes(x = reorder(District, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.1) +
  labs(x = "District", y = "Count", title = "Electric consumption data - District distribution") +
  theme_minimal() +
  theme(plot.title = element_text(color = "black", face = "bold"))



#Survey Data - Current Educational Activities of the Respondent

# Count the number of each educational activity
activity_counts <- df %>%
  group_by(`Current Educational Activity`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a column bar chart
ggplot(activity_counts, aes(x = reorder(`Current Educational Activity`,-Count) , y = Count, fill = `Current Educational Activity`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Current Educational Activity", y = "Count", title = "Educational Activity of the Respondent") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))


#Survey Data - Main occupation of the Respondent

# Count the number of each Main occupation
occupation_counts <- df %>%
  group_by(`Main occupation`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a column bar chart
ggplot(occupation_counts, aes(x = reorder(`Main occupation`, -Count), y = Count, fill = `Main occupation`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Main occupation", y = "Count", title = "Main occupation of the Respondent") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))



# Survey Data - Monthly Food Spend of the Respondent

# Count the number of each Monthly Food Spend
foodspend_counts <- df %>%
  group_by(`Monthly Avg Food Spend`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a column bar chart
ggplot(foodspend_counts, aes(x = reorder(`Monthly Avg Food Spend`, -Count), y = Count, fill = `Monthly Avg Food Spend`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Monthly Avg Food Spend", y = "Count", title = "Monthly Avg Food Spend of the Respondent") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

#Survey data - Average non food Monthly spend

# Load the necessary library
library(ggplot2)

# Create a box plot
ggplot(df, aes(x = "", y = `Monthly Avg Non - Food Spend`)) +
  geom_boxplot(outlier.shape = NA) +
  labs(x = "", y = "Monthly Avg Non - Food Spend", title = "Monthly Non Food Spend among Respondent with outliers") +
  theme(plot.title = element_text(color = "black", face = "bold"))


# Identify the outlier
outliers <- boxplot.stats(df$`Monthly Avg Non - Food Spend`)$out

# Filter out the outlier
df_no_outliers <- df %>%
  filter(!(`Monthly Avg Non - Food Spend` %in% outliers))

# Create a box plot
ggplot(df_no_outliers, aes(x = "", y = `Monthly Avg Non - Food Spend`)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.shape = NA) +
  labs(x = "", y = "Monthly Avg Non - Food Spend", title = "Monthly Non Food Spend among Respondent without outliers") +
  theme(plot.title = element_text(color = "black", face = "bold"))

#Monthly Non Food spend among the Occupation

# Create a box plot
ggplot(df_no_outliers, aes(x = `Main occupation`, y = `Monthly Avg Non - Food Spend`)) +
  geom_boxplot(fill = "steelblue", color = "black", outlier.shape = NA) +
  labs(x = "Main Occupation Category", y = "Monthly Avg Non - Food Spend", title = "Monthly Non Food Spend among Respondent by Occupation") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))


#Survey data - Bill calculation Acknowledgment

# Count the number of each bill calculation acknowledgment
acknowledgment_counts <- df %>%
  group_by(`Bill calculation Acknowledgment`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a column chart
ggplot(acknowledgment_counts, aes(x = reorder(`Bill calculation Acknowledgment`, -Count), y = Count, fill = `Bill calculation Acknowledgment`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Bill Calculation Acknowledgment", y = "Count", title = "Bill Calculation Acknowledgment among Respondent") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

#-----------------------------------------------------------------------------------------------------------------------------------------#

#Electric consumption - Related factors Analysis#

#-----1. Bill Calculation Acknowledgement-----#

# Load the necessary library
library(gridExtra)

# Modify the "Bill calculation Acknowledgment" column
df <- df %>%
  mutate(`Bill calculation Acknowledgment` = ifelse(`Bill calculation Acknowledgment` == "Don't know", "Don't know", "Have an Acknowledgment"))

# Count the number of each bill calculation acknowledgment for each main occupation
acknowledgment_counts <- df %>%
  group_by(`Bill calculation Acknowledgment`, `Main occupation`) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create a column chart for each bill calculation acknowledgment
p1 <- ggplot(acknowledgment_counts %>% filter(`Bill calculation Acknowledgment` == "Don't know"), aes(x = reorder(`Main occupation`,-Count), y = Count, fill = `Main occupation`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Main Occupation Category", y = "Count", title = "Don't know") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

p2 <- ggplot(acknowledgment_counts %>% filter(`Bill calculation Acknowledgment` == "Have an Acknowledgment"), aes(x = reorder(`Main occupation`,-Count), y = Count, fill = `Main occupation`)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste(round(Percentage, 1), "%")), vjust = -0.5) +
  labs(x = "Main Occupation Category", y = "Count", title = "Have an Acknowledgment") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

# Display the charts side by side
grid.arrange(p1, p2, ncol = 2)


#-----2.People who doesn't have an Acknowledgment about Bill calculation by Gender -----#


# Count the number of each bill calculation acknowledgment for each main occupation and gender
acknowledgment_counts <- df %>%
  filter(`Bill calculation Acknowledgment` == "Don't know") %>%
  group_by(`Main occupation`, Gender) %>%
  summarise(Count = n())

# Create a stacked horizontal bar chart
ggplot(acknowledgment_counts, aes(x = `Main occupation`, y = Count, fill = Gender)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  labs(x = "Main Occupation", y = "Count", title = "People who dosen't have an Acknowledgment about Bill calculation by Gender") +
  theme(plot.title = element_text(color = "black", face = "bold"))

#-----3.Bill Payment Behavior by Main occupation -----#


# Count the number of each bill payment behaviour for each main occupation
behaviour_counts <- df %>%
  group_by(`Main occupation`, `Bill Payment behaviour`) %>%
  summarise(Count = n())

# Create a stacked horizontal bar chart
ggplot(behaviour_counts, aes(x = `Main occupation`, y = Count, fill = `Bill Payment behaviour`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  labs(x = "Main Occupation", y = "Count", title = "Bill Payment Behaviour by Occupation") +
  theme(plot.title = element_text(color = "black", face = "bold"))


#-----4. Red Notice count by Bill payment behavior and Main Occupation-----#


# Load the necessary library
library(gridExtra)

# Calculate the total Red Notice count for each bill payment behavior
behaviour_counts <- df %>%
  group_by(`Bill Payment behaviour`) %>%
  summarise(`Total Red Notice Count` = sum(`Red Notice count`))

# Create a column chart for Red Notice count by bill payment behavior
p1 <- ggplot(behaviour_counts, aes(x = reorder(`Bill Payment behaviour`, -`Total Red Notice Count`), y = `Total Red Notice Count`, fill = `Bill Payment behaviour`)) +
  geom_bar(stat = "identity") +
  labs(x = "Bill Payment Behaviour", y = "Total Red Notice Count", title = "Red Notice Count by Bill Payment Behaviour") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

# Calculate the total Red Notice count for each main occupation
occupation_counts <- df %>%
  group_by(`Main occupation`) %>%
  summarise(`Total Red Notice Count` = sum(`Red Notice count`))

# Create a column chart for Red Notice count by main occupation
p2 <- ggplot(occupation_counts, aes(x = reorder(`Main occupation`, -`Total Red Notice Count`), y = `Total Red Notice Count`, fill = `Main occupation`)) +
  geom_bar(stat = "identity") +
  labs(x = "Main Occupation", y = "Total Red Notice Count", title = "Red Notice Count by Main Occupation") +
  theme(plot.title = element_text(color = "black", face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1))

# Display the charts side by side
grid.arrange(p1, p2, ncol = 2)


#-----5. Night lighting usage behavior pattern for each district-----#


# Load the necessary library
library(treemap)

# Count the number of each night lighting usage behaviour for each district
usage_counts <- df %>%
  group_by(`Night lighting usage behaviour`, District) %>%
  summarise(Count = n())

# Append the count to the category name
usage_counts$Label <- paste0(usage_counts$`Night lighting usage behaviour`, " (", usage_counts$Count, ")")

# Create a treemap
treemap(usage_counts,
        index = c("District", "Label"),
        vSize = "Count",
        title = "Night lighting usage pattern by District",
        title.legend = "Count",
        fontsize.labels = c(15, 12),
        fontcolor.labels = c("white", "black"),
        align.labels = list(c("center", "center"), c("left", "top")),
        fontface.labels = 2,
        bg.labels = 0)



#-----6. Ironing clothes pattern for each district-----#

# Count the number of ironing clothes count for each district
ironing_counts <- df %>%
  group_by(`Ironing clothes behaviour`, District) %>%
  summarise(Count = n())

# Append the count to the category name
ironing_counts$Label <- paste0(ironing_counts$`Ironing clothes behaviour`, " (", ironing_counts$Count, ")")

# Create a treemap
treemap(ironing_counts,
        index = c("District", "Label"),
        vSize = "Count",
        title = "Ironing clothes pattern by District",
        title.legend = "Count",
        fontsize.labels = c(15, 12),
        fontcolor.labels = c("white", "black"),
        align.labels = list(c("center", "center"), c("left", "top")),
        fontface.labels = 2,
        bg.labels = 0)

#-----7. Electric Meter checking pattern for each district-----#

# Count the number of Electric meter checking count for each district
meter_counts <- df %>%
  group_by(`is check _ Electricity meter`, District) %>%
  summarise(Count = n())

# Append the count to the category name
meter_counts$Label <- paste0(meter_counts$`is check _ Electricity meter`, " (", meter_counts$Count, ")")

# Create a treemap
treemap(meter_counts,
        index = c("District", "Label"),
        vSize = "Count",
        title = "Elecricity Meter checking pattern by District",
        title.legend = "Count",
        fontsize.labels = c(15, 12),
        fontcolor.labels = c("white", "black"),
        align.labels = list(c("center", "center"), c("left", "top")),
        fontface.labels = 2,
        bg.labels = 0)


#-----8. New Electric appliance buying pattern for each district-----#

# Count the number of Electric appliance buying pattern for each district
buying_counts <- df %>%
  group_by(`is check _ Energy rating new buying Applicance`, District) %>%
  summarise(Count = n())

# Append the count to the category name
buying_counts$Label <- paste0(buying_counts$`is check _ Energy rating new buying Applicance`, " (", buying_counts$Count, ")")

# Create a treemap
treemap(buying_counts,
        index = c("District", "Label"),
        vSize = "Count",
        title = "Electric appliance buying - Energy rating checking pattern by district",
        title.legend = "Count",
        fontsize.labels = c(15, 12),
        fontcolor.labels = c("white", "black"),
        align.labels = list(c("center", "center"), c("left", "top")),
        fontface.labels = 2,
        bg.labels = 0)


#-----9. Electricity Generation methods by district-----#


# Load the necessary libraries
library(tidyverse)

# Separate the multiple inputs into different rows
df_separated <- df %>%
  separate_rows(`Electricity Generation Methods`, sep = ", ")

# Count the number of each electricity generation method for each district
method_counts <- df_separated %>%
  group_by(District, `Electricity Generation Methods`) %>%
  summarise(Count = n(), .groups = "drop")

# Create a column chart
ggplot(method_counts, aes(x = District, y = Count, fill = `Electricity Generation Methods`)) +
  geom_col() +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(x = "District", y = "Count", fill = "Electricity Generation Methods") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~`Electricity Generation Methods`, scales = "free_y")


#-----10. When people use Solar Energy how they core about Energy Saving-----#


# Load the necessary library
library(gridExtra)

# Filter the data for 'Solar Energy'
solar_energy <- df %>%
  filter(`Electricity Generation Methods` == "Solar Energy")

# Count the number of each 'Ironing clothes behavior' for 'Solar Energy'
behaviour_counts <- solar_energy %>%
  group_by(`Ironing clothes behaviour`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Create a column chart
p1 <- ggplot(behaviour_counts, aes(x = reorder(`Ironing clothes behaviour`, -Count), y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(x = "Ironing Clothes Behaviour", y = "Count", title = "Ironing Clothes Behaviour when use Solar Energy") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Count the number of each 'Night lighting usage behavior' for 'Solar Energy'
night_counts <- solar_energy %>%
  group_by(`Night lighting usage behaviour`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Create a column chart
p2 <- ggplot(night_counts, aes(x = reorder(`Night lighting usage behaviour`, -Count), y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(x = "Night Lighting Usage Behaviour", y = "Count", title = "Night Lighting Usage Behaviour when use Solar Energy") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Count the number of each 'is check _ Electricity meter' for 'Solar Energy'
meter_counts <- solar_energy %>%
  group_by(`is check _ Electricity meter`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  arrange(desc(Count))

# Create a column chart
p3 <- ggplot(meter_counts, aes(x = reorder(`is check _ Electricity meter`, -Count), y = Count)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = Count), vjust = -0.3) +
  labs(x = "Is Check _ Electricity Meter", y = "Count", title = "Electricity meter checking behaviour when use Solar Energy'") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none")

# Arrange the charts vertically
grid.arrange(p1, p2, p3, nrow = 3)


#-----11. Solar Energy use cases-----#


# Separate the multiple inputs into different rows
df_separated <- df %>%
  separate_rows(`Solar Energy use cases`, sep = ", ")

# Filter out 'Don't know' rows and replace 'All the above' with specific use cases
df_filtered <- df_separated %>%
  filter(`Solar Energy use cases` != "Solar Energy does not use") %>%
  mutate(`Solar Energy use cases` = ifelse(`Solar Energy use cases` == "All the above", 
                                           c("Water heating", "Agriculture equipment and systems (irrigation systems, etc)", "Outdoor lighting", "Car charging", "Cooking"), 
                                           `Solar Energy use cases`))

# Count the number of each solar energy use case
use_case_counts <- df_filtered %>%
  group_by(`Solar Energy use cases`) %>%
  summarise(Count = n(), .groups = "drop")

# Create a horizontal bar chart
ggplot(use_case_counts, aes(x = reorder(`Solar Energy use cases`,-Count), y = Count, fill = `Solar Energy use cases`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Count), hjust = -0.3) +
  labs(x = "Solar Energy Use Cases", y = "Count", title = "Solar Energy Use Cases", fill = "Solar Energy Use Cases") +
  theme(plot.title = element_text(color = "black", face = "bold"), legend.position = "none") +
  scale_fill_brewer(palette = "Set3")



#----- Inference Statistical Analysis -----#



#___________________________________________________________________________________________________________#

# Hypothesis testing

# Hypothesis 1: There is a significant relationship between the time period a house was built and the household's electricity consumption. 


# Count the number of each ironing clothes behavior by Build Year Period
ironing_clothes_counts <- df %>%
  group_by(`Built Year Period`, `Ironing clothes behaviour`) %>%
  summarise(Count = n())

# Create a stacked horizontal bar chart
ggplot(ironing_clothes_counts, aes(x = `Built Year Period`, y = Count, fill = `Ironing clothes behaviour`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  labs(x = "Built Year Period", y = "Count", title = "Ironing clothes behaviour by Build Year Period") +
  theme(plot.title = element_text(color = "black", face = "bold"))


# Load the necessary library
library(dplyr)

# Hypothesis i: There is a significant relationship between Built Year Period and Ironing clothes behavior
result <- chisq.test(table(df$`Built Year Period`, df$`Ironing clothes behaviour`))

# Print the p-value

print(result)
print(paste("p-value:", result$p.value))

# Conclusion
if (result$p.value < 0.05) {
  print("Reject the null hypothesis. There is a significant relationship between Built Year Period and Ironing clothes behaviour.")
} else {
  print("Do not reject the null hypothesis. There is not enough evidence to suggest a significant relationship between Built Year Period and Ironing clothes behaviour.")
}


# Count the number of each Night lighting usage behaviour by Build Year Period
ironing_clothes_counts <- df %>%
  group_by(`Built Year Period`, `Night lighting usage behaviour`) %>%
  summarise(Count = n())

# Create a stacked horizontal bar chart
ggplot(ironing_clothes_counts, aes(x = `Built Year Period`, y = Count, fill = `Night lighting usage behaviour`)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  geom_text(aes(label = Count), position = position_stack(vjust = 0.5)) +
  labs(x = "Built Year Period", y = "Count", title = "Night lighting usage behaviour by Build Year Period") +
  theme(plot.title = element_text(color = "black", face = "bold"))



# Hypothesis ii: There is a significant relationship between Built Year Period and Night lighting usage behavior
result <- chisq.test(table(df$`Built Year Period`, df$`Night lighting usage behaviour`))

# Print the p-value

print(result)
print(paste("p-value:", result$p.value))

# Conclusion
if (result$p.value < 0.05) {
  print("Reject the null hypothesis. There is a significant relationship between Built Year Period and Night lighting usage behavior.")
} else {
  print("Do not reject the null hypothesis. There is not enough evidence to suggest a significant relationship between Built Year Period and Night lighting usage behavior.")
}



# Hypothesis iii: Relationship between Built Year Period and is check _ Electricity meter
result3 <- chisq.test(table(df$`Built Year Period`, df$`is check _ Electricity meter`))
print(result3)


# Hypothesis 2: Households who have knowledge about the bill payment have less electricity consumption.



# Load the necessary library
library(dplyr)

# Create a contingency table of Bill calculation Acknowledgment and Ironing clothes behaviour
contingency_table <- table(df$`Bill calculation Acknowledgment` != "Don't know", df$`Ironing clothes behaviour` != "don’t iron")

# Perform a Chi-Square test of independence
result <- chisq.test(contingency_table)

# Print the p-value
print(paste("p-value:", result$p.value))

# Conclusion
if (result$p.value < 0.05) {
  print("Reject the null hypothesis. There is a significant association between having knowledge about the bill payment and ironing clothes behavior.")
} else {
  print("Do not reject the null hypothesis. There is not enough evidence to suggest a significant association between having knowledge about the bill payment and ironing clothes behavior.")
}


# Association Of Electric consumption in terms of Location

# Load the necessary libraries
library(dplyr)
library(vcd)
library(reshape2)
library(ggplot2)

# Select the relevant columns
df_selected <- df %>% select(District, `Ironing clothes behaviour`, `Night lighting usage behaviour`, `is check _ Electricity meter`, `is check _ Energy rating new buying Applicance`)

# Function to calculate Cramer's V
cramersV <- function(x, y) assocstats(table(x, y))$cramer

# Compute Cramer's V for each pair of variables
cramersV_matrix <- sapply(df_selected, function(x) sapply(df_selected, function(y) cramersV(x, y)))

# Convert the matrix to a data frame for visualization
cramersV_df <- melt(cramersV_matrix)

# Rename the columns
colnames(cramersV_df) <- c("Variable 1", "Variable 2", "CramersV")

# Create a heatmap of Cramer's V
ggplot(cramersV_df, aes(x = `Variable 1`, y = `Variable 2`, fill = CramersV)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5, limit = c(0,1), space = "Lab", name="Cramer's V") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Heatmap of Cramer's V", x = "Variable 1", y = "Variable 2")


# Association Of Electric consumption and Solar Energy use cases

# Select the relevant columns
df_selected <- df %>% select(`Solar Energy use cases`, `Ironing clothes behaviour`, `Night lighting usage behaviour`, `is check _ Electricity meter`, `is check _ Energy rating new buying Applicance`)

# Function to calculate Cramer's V
cramersV <- function(x, y) assocstats(table(x, y))$cramer

# Compute Cramer's V for each pair of variables
cramersV_matrix <- sapply(df_selected, function(x) sapply(df_selected, function(y) cramersV(x, y)))

# Convert the matrix to a data frame for visualization
cramersV_df <- melt(cramersV_matrix)

# Rename the columns
colnames(cramersV_df) <- c("Variable 1", "Variable 2", "CramersV")

# Create a heatmap of Cramer's V
ggplot(cramersV_df, aes(x = `Variable 1`, y = `Variable 2`, fill = CramersV)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0.5, limit = c(0,1), space = "Lab", name="Cramer's V") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
  labs(title = "Heatmap of Cramer's V", x = "Variable 1", y = "Variable 2")


# List of categorical variables with shorter names
categorical_vars <- c("BuiltYear",
                      "Designer",
                      "Stories",
                      "Relationship",
                      "Gender",
                      "Education",
                      "Occupation",
                      "FoodSpend",
                      "Material",
                      "Ventilation",
                      "RoofInstall",
                      "RedNotice",
                      "BillBehaviour",
                      "ProfessionalWiring",
                      "BillAcknowledge",
                      "NightLightBehaviour", 
                      "CheckMeter", 
                      "CheckRating", 
                      "District")

# Rename the columns in the dataframe
names(df)[names(df) %in% c("Built Year Period",
                           "House Designer",
                           "Stories count",
                           "Relationship to Household",
                           "Gender",
                           "Current Educational Activity",
                           "Main occupation",
                           "Monthly Avg Food Spend",
                           "Built Material",
                           "Indoor Ventilation",
                           "Highest floor roof installation",
                           "Red Notice count",
                           "Bill Payment behaviour",
                           "is wiring _ professional electrician?",
                           "Bill calculation Acknowledgment",
                           "Night lighting usage behaviour", 
                           "is check _ Electricity meter", 
                           "is check _ Energy rating new buying Applicance", 
                           "District")] <- categorical_vars

# Initialize an empty matrix to store the p-values
p_values <- matrix(nrow = length(categorical_vars), ncol = length(categorical_vars))
rownames(p_values) <- categorical_vars
colnames(p_values) <- categorical_vars

# Perform a Chi-square test for each pair of variables
for (i in 1:length(categorical_vars)) {
  for (j in 1:length(categorical_vars)) {
    # Skip if the variables are the same
    if (i == j) next
    
    # Perform the Chi-square test
    test_result <- chisq.test(df[[categorical_vars[i]]], df[[categorical_vars[j]]])
    
    # Store the p-value in the matrix
    p_values[i, j] <- test_result$p.value
  }
}

# Print the matrix of p-values
print(p_values)

# Load the necessary library
library(corrplot)

# Convert the p-values to correlations
correlations <- 1 - p_values

# Create a correlation matrix with correlation values and a title
corrplot(correlations, method = "color", type = "upper", diag = FALSE, 
         addCoef.col = "black", # Add correlation values
         title = "Correlation matrix for Electricity usage pattern", # Add title
         mar = c(0,0,1,0)) # Add space for title


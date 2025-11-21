
# Load relevant libraries
library(tidyverse)  #data manipulation and visualization
library(lubridate)  #to work with date and time data
library(readxl)     #to read the excel file
library(gridExtra)  # plot arrangement 
library(ggplot2)    # for ggplot and qplot 
library(rlang)      # is needed in tidyverse package
library(arules)     #for association rule

#Loading the dataset 
retail_dataset <- read_excel("C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Datamining/Online Retail.xlsx")

head(retail_dataset, 10)

#check the data types and information
glimpse(retail_dataset)

#Convert the data type of  CustomerID from double to integer
retail_dataset$CustomerID <- as.integer(retail_dataset$CustomerID)

#Check the missing values
colSums(is.na(retail_dataset))

# Check for "?" in columns
sapply(retail_dataset, function(x) if(is.character(x)) any(x == "?", na.rm = TRUE) else FALSE)

# Check for "??" in columns
sapply(retail_dataset, function(x) if(is.character(x)) any(x == "??", na.rm = TRUE) else FALSE)

# Count "?" values
sum(sapply(retail_dataset, function(x) if(is.character(x)) sum(x == "?", na.rm = TRUE) else 0))

# Count "??" values
sum(sapply(retail_dataset, function(x) if(is.character(x)) sum(x == "??", na.rm = TRUE) else 0))


#==============================Start cleaning process=======================================


#create  cleaned_dataset: replace "?" and "??" with NA
cleaned_dataset <- retail_dataset %>%
  mutate(across(where(is.character), ~replace(., . %in% c("?", "??"), NA)))

#check the missing values again to confirm  
colSums(is.na(cleaned_dataset))

#remove the null values from description column
cleaned_dataset <- cleaned_dataset %>%
  filter(!is.na(Description))

#remove null values from CustomerID column
cleaned_dataset <- cleaned_dataset %>%
  filter(!is.na(CustomerID))

#check for null values in Description column
sum(is.na(cleaned_dataset$Description))

#check for null values in CustomerID column
sum(is.na(cleaned_dataset$CustomerID))

#check the negative values in UnitPrice and Quantity
sum(retail_dataset$UnitPrice <= 0)
sum(retail_dataset$Quantity <= 0)

#Remove zero and negative values of UnitPrice and Quantity
cleaned_dataset <- cleaned_dataset %>%
  filter(Quantity > 0, UnitPrice > 0)

#check the negative values in UnitPrice and Quantity
sum(cleaned_dataset$UnitPrice <= 0)
sum(cleaned_dataset$Quantity <= 0)

# List service StockCodes starting with letters
service_stockcode_retail <- retail_dataset %>%
  filter(grepl("^[a-zA-Z]+", StockCode)) %>%
  select(StockCode, Description) %>%
  distinct()

print (service_stockcode_retail, n=Inf)
View(service_stockcode_retail)

# Define the list of service stock codes to be removed
service_stockcodes <- c('POST', 'C2', 'DOT', 'M', 'BANK CHARGES', 'm', 'AMAZONFEE', 'S', 
                         'gift_0001_10', 'gift_0001_20', 'gift_0001_30', 'gift_0001_40', 'gift_0001_50')

# Remove service_stockcodes
cleaned_dataset <- cleaned_dataset %>%
  filter(!(StockCode %in% service_stockcodes))

#Check invalid (zero) values in each selected column
cleaned_dataset %>%
  summarise(
    Zero_InvoiceNo = sum(InvoiceNo == "0", na.rm = TRUE),
    Zero_StockCode = sum(StockCode == "0", na.rm = TRUE),
    Zero_Description = sum(Description == "0", na.rm = TRUE),
    Zero_Country = sum(Country == "0", na.rm = TRUE)
  )

# check missing values column wise
colSums(is.na(cleaned_dataset))

# Save cleaned dataset into a CSV file
write.csv(cleaned_dataset, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Datamining/cleaned_online_retail_dataset.csv", row.names = FALSE)



#==========================Exploratory Data Analysis======================================


#Summary statistics of Unitprice and Quantity
summary(cleaned_dataset$UnitPrice)
summary(cleaned_dataset$Quantity)

# Histogram for UnitPrice 
ggplot(cleaned_dataset, aes(x = UnitPrice)) + 
  geom_histogram(binwidth = 1,fill = "skyblue", color = "black") + 
  theme_minimal() + 

  xlim(0, 50)  +
  ggtitle("Distribution of Unit Prices") + 
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for Unit Price
ggplot(cleaned_dataset, aes(y = UnitPrice)) +
  geom_boxplot(fill = "salmon") +
  labs(title = "Boxplot of Unit Price", y = "Unit Price") +
  theme_minimal()

#Histogram for Quantity
ggplot(cleaned_dataset, aes(x = Quantity)) + 
  geom_histogram(binwidth = 3, fill = "skyblue", color = "black") + 
  theme_minimal() + xlim(0,100) +
  ggtitle("Distribution of Quantities") +  
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot for Quantity
ggplot(cleaned_dataset, aes(y = Quantity)) +
  geom_boxplot(fill = "skyblue") +
  labs(title = "Boxplot of Quantity", y = "Quantity") +
  theme_minimal()

# Create TotalPrice column
cleaned_dataset$TotalPrice <- cleaned_dataset$Quantity * cleaned_dataset$UnitPrice

# Select relevant columns
correlation_data <- cleaned_dataset[, c("Quantity", "UnitPrice", "TotalPrice")]

# Compute and display the correlation matrix
cor_matrix <- cor(correlation_data, method = "pearson")  
print(cor_matrix)

# Top 10 products by quantity
top_10_products <- cleaned_dataset %>%
  group_by(StockCode, Description) %>%
  summarise(total_quantity = sum(Quantity, na.rm = TRUE)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)

# View the result
print(top_10_products)

# Bar plot for top 10 products by quantity
ggplot(top_10_products, aes(x = reorder(Description, total_quantity), y = total_quantity)) + 
  geom_bar(stat = "identity", fill = "steelblue") + 
  coord_flip() +  
  labs(title = "Top 10 Products by Quantity", x = "Product", y = "Total Quantity") + 
  theme_minimal() + 
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(size = 6)
  )

#===============================Top 3 countries based on transaction count===============


# Count number of records for each Country
country_record_counts <- cleaned_dataset %>%
  group_by(Country) %>%
  summarise(record_count = n(), .groups = 'drop') %>%
  arrange(desc(record_count))

# View the result
print(country_record_counts)


#Get top 3 countries 
top3_countries <- cleaned_dataset %>%
  group_by(Country) %>%
  summarise(transaction_count = n()) %>%
  arrange(desc(transaction_count)) %>%
  head(3) %>%
  pull(Country)  # Top 3 countries based on transaction count

# Filter the retail data to include only the top 3 countries
filtered_data <- cleaned_dataset %>%
  filter(Country %in% top3_countries)

print(top3_countries)


#============================Create baskets for each country===========================

#=================================01.United Kingdom====================================

# Filter for UK 
uk_data <- cleaned_dataset %>% filter(Country == "United Kingdom")

# Create transactions: group by InvoiceNo and get list of StockCodes or Descriptions
uk_transactions <- uk_data %>%
  group_by(InvoiceNo) %>%
  summarise(Items = list(Description)) %>%
  pull(Items)

# Convert to transactions format
uk_basket <- as(uk_transactions, "transactions")

inspect(uk_basket[1])

# Convert transactions to a logical sparse matrix
basket_matrix <- as(uk_basket, "matrix")

# Convert to a data frame
basket_df <- as.data.frame(basket_matrix)

# View the first 5 rows and first 10 columns
basket_df[1:5, 1:10]

# Apply the Apriori algorithm to find frequent itemsets
uk_rules <- apriori(uk_basket, parameter = list(supp = 0.01, conf = 0.5))

# View the rules
summary(uk_rules)

inspect(uk_rules)

# Convert the association rules to a data frame
uk_rules_df <- as(uk_rules, "data.frame")

inspect(head(uk_rules, 10))

# Save to CSV
#write.csv(uk_rules_df, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Datamining/uk_apriori_rules.csv", row.names = FALSE)

#===================================02.Germany==========================================

# Filter for Germany
germany_data <- cleaned_dataset %>% filter(Country == "Germany")

# Create transactions
germany_transactions <- germany_data %>%
  group_by(InvoiceNo) %>%
  summarise(Items = list(Description)) %>%
  pull(Items)

# Convert to transaction format
germany_basket <- as(germany_transactions, "transactions")

# Apply the Apriori algorithm to find frequent itemsets
germany_rules <- apriori(germany_basket, parameter = list(supp = 0.01, conf = 0.5))

inspect(germany_rules)

inspect(head(germany_rules, 10))

# Convert the association rules to a data frame
germany_rules_df <- as(germany_rules, "data.frame")

# Save to CSV
#write.csv(germany_rules_df, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Datamining/germany_apriori_rules.csv", row.names = FALSE)


#===================================03.France==================================

# Filter for France
france_data <- cleaned_dataset %>% filter(Country == "France")

# Create transactions
france_transactions <- france_data %>%
  group_by(InvoiceNo) %>%
  summarise(Items = list(Description)) %>%
  pull(Items)

# Convert to transaction format
france_basket <- as(france_transactions, "transactions")

# Apply the Apriori algorithm to find frequent itemsets
france_rules <- apriori(france_basket, parameter = list(supp = 0.01, conf = 0.5))

inspect(france_rules)

inspect(head(france_rules, 10))

# Convert the association rules to a data frame
france_rules_df <- as(france_rules, "data.frame")

# Save to CSV
#write.csv(france_rules_df, "C:/Users/User/Documents/My Documents/Datascience/Modules/Data mining/CW_Datamining/CW_Datamining/france_apriori_rules.csv", row.names = FALSE)




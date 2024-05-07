# Load the required library
library(dplyr)

# Load the dataset
big_basket <- read.csv("C:/Users/ravin/Downloads/archive (1)/BigBasket Products.csv")

# Drop the 'description' column
big_basket <- subset(big_basket, select = -c(description))

# Display the structure of the dataset
print(str(big_basket))

# Summary statistics of the dataset
print(summary(big_basket))

# Check for missing values
missing_values <- colSums(is.na(big_basket))
print(missing_values)

# Remove rows with missing values
big_basket <- na.omit(big_basket)
print(summary(big_basket))
print(str(big_basket))
print(ncol(big_basket))

# Explore categorical variables
unique(big_basket$product)
unique(big_basket$category)
unique(big_basket$sub_category)
unique(big_basket$brand)
unique(big_basket$type)

# Explore numerical variables
summary(big_basket$sale_price)
summary(big_basket$market_price)
summary(big_basket$rating)

# Perform any necessary transformations or conversions (e.g., encoding categorical variables)
## Header: Create a new feature 'discount_percentage'
big_basket$discount_percentage <- (big_basket$market_price - big_basket$sale_price) / big_basket$market_price


## Header: Check the first few rows of the updated dataset
print(big_basket)
# Save the cleaned dataset
write.csv(big_basket, "E:/moder_opt/clean_big_basket1.csv", row.names = FALSE)


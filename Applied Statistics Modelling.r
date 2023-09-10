library(IRdisplay)
display_png(file = "Ajio-logo.jpg", width = 700, height = 100)

file_path <- "Ajio Fasion Clothing.csv"
# Read the CSV file into a data frame
df <- read.csv(file_path, header = TRUE, sep = ",")
options(width = 10)
# Display the first few rows of the data to verify it was loaded correctly
head(df)

# View the structure of the dataset
str(df)

# Summary statistics
summary(df)

# Check for missing values
colSums(is.na(df))

# Ensure the column 'Discount.Price..in.Rs..' is numeric (convert if not already)
df$Discount.Price..in.Rs.. <- as.numeric(df$Discount.Price..in.Rs..)

# Create a histogram
hist(df$Discount.Price..in.Rs.., main = "Histogram of Discount Price (in Rs.)", xlab = "Discount Price (in Rs.)", col = "lightblue")

# Ensure both columns are numeric (convert if not already)
df$Discount.Price..in.Rs.. <- as.numeric(df$Discount.Price..in.Rs..)
df$Original.Price..in.Rs.. <- as.numeric(df$Original.Price..in.Rs..)

# Create a scatter plot
plot(df$Discount.Price..in.Rs.., df$Original.Price..in.Rs.., 
     main = "Scatter Plot: Discount Price vs. Original Price", 
     xlab = "Discount Price (in Rs.)", ylab = "Original Price (in Rs.)", 
     pch = 19, col = "blue")

install.packages("path/to/UpSetR_1.4.0.tar.gz", repos = NULL, type = "source")

# Visualize missing values
library(naniar)
miss_var_summary(df)

# Check for duplicate rows and remove them if needed
df <- unique(df)

# Drop the specified columns
df <- df[, !(names(df) %in% c("Product_URL", "URL_image", "Id_Product"))]

# Check the structure of the modified dataset
str(df)

# Remove or replace non-ASCII characters in the 'Description' column
df$Description <- iconv(df$Description, to = "ASCII", sub = " ")

# Now, convert 'Description' to lowercase
df$Description <- tolower(df$Description)

# Convert 'Discount.Price..in.Rs..' and 'Original.Price..in.Rs..' to numeric
df$Discount.Price..in.Rs.. <- as.numeric(gsub(",", "", df$Discount.Price..in.Rs..))
df$Original.Price..in.Rs.. <- as.numeric(gsub(",", "", df$Original.Price..in.Rs..))

# Check for missing values in 'Discount.Price..in.Rs..' and 'Original.Price..in.Rs..' columns
missing_discount <- sum(is.na(df$Discount.Price..in.Rs..))
missing_original <- sum(is.na(df$Original.Price..in.Rs..))

print("Before Imputation:")
print(paste("Missing values in 'Discount.Price..in.Rs..':", missing_discount))
print(paste("Missing values in 'Original.Price..in.Rs..':", missing_original))

# Impute missing values with the mean
df$Discount.Price..in.Rs..[is.na(df$Discount.Price..in.Rs..)] <- mean(df$Discount.Price..in.Rs.., na.rm = TRUE)
df$Original.Price..in.Rs..[is.na(df$Original.Price..in.Rs..)] <- mean(df$Original.Price..in.Rs.., na.rm = TRUE)

# Check for missing values again after imputation
missing_discount <- sum(is.na(df$Discount.Price..in.Rs..))
missing_original <- sum(is.na(df$Original.Price..in.Rs..))

print("After Imputation:")
print(paste("Missing values in 'Discount.Price..in.Rs..':", missing_discount))
print(paste("Missing values in 'Original.Price..in.Rs..':", missing_original))

# Example: Encode 'Category_by_gender' using one-hot encoding in the original 'df' dataset
encoded <- model.matrix(~Category_by_gender - 1, data = df)
colnames(encoded) <- gsub("Category_by_gender", "", colnames(encoded))
df <- cbind(df, encoded)

# Remove the original 'Category_by_gender' column if needed
df <- df[, -which(names(df) == "Category_by_gender")]

# Min-max scaling for 'Discount.Price..in.Rs..' and 'Original.Price..in.Rs..' after imputation
df$Discount.Price..in.Rs.. <- (df$Discount.Price..in.Rs.. - min(df$Discount.Price..in.Rs..)) / (max(df$Discount.Price..in.Rs..) - min(df$Discount.Price..in.Rs..))
df$Original.Price..in.Rs.. <- (df$Original.Price..in.Rs.. - min(df$Original.Price..in.Rs..)) / (max(df$Original.Price..in.Rs..) - min(df$Original.Price..in.Rs..))

df

# Sample data
sample_data <- data.frame(
  Discount.Price..in.Rs.. = c(500, 600, 700, 800, 900),
  Original.Price..in.Rs.. = c(700, 750, 780, 850, 950)
)

# Calculate the price difference
sample_data$Price_Difference <- sample_data$Original.Price..in.Rs.. - sample_data$Discount.Price..in.Rs..

# Create a line chart
library(ggplot2)

line_chart <- ggplot(sample_data, aes(x = 1:5)) +
  geom_line(aes(y = Original.Price..in.Rs.. - 100), color = "blue", size = 1) +
  geom_line(aes(y = Discount.Price..in.Rs..), color = "red", size = 1) +
  labs(
    title = "Original Price vs. Discount Price",
    x = "Product",
    y = "Price (in Rs.)"
  ) +
  scale_x_continuous(breaks = 1:5, labels = 1:5) +
  theme_minimal()

# Display the line chart
print(line_chart)

# Set the seed for reproducibility
set.seed(123)

# Create a random sample of 10% of the data
sample_size <- 0.10  # Adjust the sample size as needed
sampled_data <- df[sample(1:nrow(df), size = round(sample_size * nrow(df))), ]

# Perform ANOVA to test for brand impact on sales
brand_anova <- aov(Discount.Price..in.Rs.. ~ Brand, data = sampled_data)
summary(brand_anova)

# Assuming you have a data frame named 'sampled_data' containing your dataset
library(ggplot2)
library(dplyr)

# Create a summary data frame with mean discount price by brand
brand_summary <- sampled_data %>%
  group_by(Brand) %>%
  summarize(Avg_Discount_Price = mean(Discount.Price..in.Rs..)) %>%
  arrange(desc(Avg_Discount_Price)) %>%
  head(10)

# Create a bar plot for the top 10 brands
ggplot(brand_summary, aes(x = reorder(Brand, -Avg_Discount_Price), y = Avg_Discount_Price)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Average Discount Price by Top 10 Brands",
       x = "Brand", y = "Average Discount Price (in Rs.)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

# Perform ANOVA to test for color impact on sales
color_anova <- aov(Discount.Price..in.Rs.. ~ Color, data = sampled_data)
summary(color_anova)

library(forcats)

# Create a summary data frame with mean discount price by color
color_summary <- df %>%
  group_by(Color) %>%
  summarize(Avg_Discount_Price = mean(Discount.Price..in.Rs..)) %>%
  arrange(desc(Avg_Discount_Price)) %>%
  head(10)

# Create a bar chart for the top N colors
ggplot(color_summary, aes(x = fct_reorder(Color, -Avg_Discount_Price), y = Avg_Discount_Price)) +
  geom_bar(stat = "identity", fill = "violet") +
  labs(
    title = "Top 10 Colors by Average Discount Price",
    x = "Color",
    y = "Average Discount Price (in Rs.)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

# Perform a t-test or other suitable test to compare sales between men and women
t_test <- t.test(Discount.Price..in.Rs.. ~ Men, data = sampled_data)
t_test

# Load necessary libraries
library(ggplot2)

# Sample data (replace this with your actual dataset)
data <- data.frame(
  Gender = c("Men", "Women", "Men", "Women"),
  Average_Discount_Price = c(0.7, 0.6, 0.65, 0.55)
)

# Create a grouped bar chart
ggplot(data = data, aes(x = Gender, y = Average_Discount_Price, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8)) +
  
  # Customize the chart appearance
  labs(title = "Gender Impact on Average Discount Price",
       x = "Gender",
       y = "Average Discount Price") +
  
  theme_minimal() +
  
  # Adjust the axis labels
  scale_x_discrete(labels = c("Men", "Women"))

# Perform a t-test to compare prices between products for men and women
t_test_gender_price <- t.test(`Discount.Price..in.Rs..` ~ Men, data = sampled_data)
t_test_gender_price

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)  # Load the tidyr package

# Example data (replace with your actual dataset)
# Create a sample data frame
data <- data.frame(
  Brand = c("Brand1", "Brand2", "Brand3", "Brand4", "Brand5"),
  Discount.Price..in.Rs.. = c(50, 45, 60, 55, 48),
  Original.Price..in.Rs.. = c(60, 50, 70, 65, 55),
  Color = c("Red", "Blue", "Green", "Red", "Blue"),
  Men = c(30, 25, 35, 28, 30),
  Women = c(28, 24, 34, 27, 29)
)

# Reshape the data for plotting
data_long <- data %>%
  pivot_longer(cols = c("Men", "Women"), names_to = "Gender", values_to = "Price")

# Create the grouped bar chart
ggplot(data_long, aes(x = Brand, y = Price, fill = Gender)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.8), width = 0.6) +
  labs(
    title = "Average Prices by Brand for Men and Women",
    x = "Brand",
    y = "Average Price",
    fill = "Gender"
  ) +
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Perform a chi-squared test to assess the association between brand loyalty and gender
chi_squared_brand_loyalty <- chisq.test(table(sampled_data$Brand, sampled_data$Men))
chi_squared_brand_loyalty



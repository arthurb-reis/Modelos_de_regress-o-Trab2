column_names <- c("age","workclass","fnlwgt","education","education-num","marital-status","ocupation", "relationship", "race", "sex", "capital-gain","capital-loss","hours-per-week","native-country","income")
data <- read.csv("datasets/Adult/adult.csv", col.names = column_names)
string_columns <- sapply(data, is.character)

# Loop through and clean the string columns
#Convert categorical data into factors
for (col in names(data)[string_columns]) {
    data[, col] <- trimws(data[, col])
    data[,col] <- as.factor(data[,col])
}

# Check for missing values in the entire dataset
missing_values <- sum(is.na(data))

# Check for missing values in each column
missing_values_per_column <- colSums(is.na(data))

print(missing_values)
print(missing_values_per_column)
head(data)

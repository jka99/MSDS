setwd("C:/Users/jeffe/Documents/MSDS/GitHub/MSDS/DS740/lesson 11")

library(dplyr)
library(arules)

data("Groceries")

### question 1, 2, 3
a <- summary(Groceries)

### question 4
# Get item frequency
itemFrequency <- itemFrequency(Groceries)

# Filter items with frequency >= 0.05
itemFrequency <- itemFrequency[itemFrequency >= 0.05]

# Convert to data frame
itemFrequencyDF <- data.frame(Item = names(itemFrequency), Frequency = itemFrequency)

# Plot barplot using ggplot2
ggplot(itemFrequencyDF, aes(x = reorder(Item, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity") +
  ggtitle("Item Frequency") +
  xlab("Item") +
  ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### question 5
# Mine association rules
rules <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.5))

# Filter rules with size == 1
sub_rules <- subset(rules, size(rules) == 2)

# Inspect rules
inspect(sub_rules)


### question 6
rules6 <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.5))

# Sort rules by lift
rules6 <- sort(rules6, by = "lift", decreasing = TRUE)

# Inspect top 10 rules
inspect(head(rules6, n = 10))

### question 7
answer7 <- 18.996

### question 8
answer8 <- "instant food productsand soda...19 times AS likely...chosen randomly"

### question 9
answer9 <- 12

### question 10
answer10 <- 0.633

### question 11
# Mine association rules
rules11 <- apriori(Groceries, parameter = list(support = 0.001, confidence = 0.5))

# Filter out redundant rules
rules11 <- rules11[!is.redundant(rules11)]

# Count remaining rules
count <- length(rules11)
answer11 <- count

### question 12
pastryRules <- subset(rules11, rhs %in% "pastry")

# Inspect rules
inspect(pastryRules)

### question 13
answer13 <- "upload document"

### question 14
hd <- read.csv("HeartDisease.csv")

hd.data <- hd %>%
  mutate(hasCP = as.factor(if_else(ChestPain == 4, 0, 1))) %>%
  select(-ChestPain)

### question 15
hd$Age = discretize(hd$Age, breaks = 3, ordered = T, method = "interval")

### question 16
summary(hd$BloodPressure)
hd$BloodPressure = discretize(hd$BloodPressure, method = "fixed", 
                              breaks = c(94, 120, 140, 200), ordered = T)

### question 17
hd.data <- hd.data %>%
  mutate(Sex = as.factor(Sex),
         Chol.disc = as.factor(Chol.disc),
         HighBloodSugar = as.factor(HighBloodSugar),
         ECG = as.factor(ECG),
         MaxHeartRate.disc = as.factor(MaxHeartRate.disc),
         ExerAngina = as.factor(ExerAngina),
         STdepress.disc = as.factor(STdepress.disc),
         Slope = as.factor(Slope),
         Fluoroscopy = as.factor(Fluoroscopy),
         Thal = as.factor(Thal),
         hasHD = as.factor(hasHD))

hd.trans <- as(hd.data, "transactions")

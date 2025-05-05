caeserian <- read.csv("C:/Project/Dataset(Updated)_MIdterm_sectoin(B).csv",header=TRUE,sep=",")
caeserian

caeserian <- subset(caeserian, select = -c(X, X.1,X.2))
caeserian$Blood[caeserian$Blood == ""] <- NA
colSums(is.na(caeserian))
library(VIM)
aggr(caeserian, col=c('skyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(caeserian), cex.axis=.7, gap=3)




caeserian$Delivery_time <- factor(caeserian$Delivery_time,
                                  levels = c(0, 1, 2, 3),
                                  labels = c("Morning", "Afternoon", "Evening", "Night"))
mode_value <- names(sort(table(caeserian$Delivery_time), decreasing = TRUE))[1]
caeserian$Delivery_time[is.na(caeserian$Delivery_time)] <- mode_value
caeserian 

caeserian$Delivery_number <- factor(caeserian$Delivery_number,
                                    levels = c(1, 2, 3, 4),
                                    labels = c("First-time", "Second-time", "Third-time", "Fourth-time"))
mode_val <- names(sort(table(caeserian$Delivery_number), decreasing = TRUE))[1]
caeserian$Delivery_number[is.na(caeserian$Delivery_number)] <- mode_val
caeserian

caeserian$Heart <- factor(caeserian$Heart, 
                          levels = c(0, 1),
                          labels = c("No", "Yes"))
caeserian


mode_blood <- as.character(names(sort(table(caeserian$Blood), decreasing = TRUE)[1]))
caeserian$Blood[is.na(caeserian$Blood)] <- mode_blood
caeserian

caeserian$Caesarian <- factor(caeserian$Caesarian,
                              levels = c(0, 1),
                              labels = c("No_Caeser","Yes"))
caeserian
View(caeserian)





Q1 <- quantile(caeserian$Age, 0.25, na.rm = TRUE)
Q3 <- quantile(caeserian$Age, 0.75, na.rm = TRUE)
IQR_value <- Q3 - Q1

lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

caeserian$Age <- ifelse(caeserian$Age < lower_bound, floor(lower_bound),
                        ifelse(caeserian$Age > upper_bound, floor(upper_bound), caeserian$Age))
caeserian$Age

mean_age <- floor(mean(caeserian$Age, na.rm = TRUE))
caeserian$Age[is.na(caeserian$Age)] <- mean_age
caeserian$Age


write.csv(caeserian, file = "E:/10th sem/Data Science/Updated Mid Assignment/cleaned_caeserian.csv", row.names = FALSE)






duplicates <- caeserian[duplicated(caeserian), ]
print(duplicates)
caeserian <- caeserian[!duplicated(caeserian), ]
caeserian





library(dplyr)
filtered_age <- caeserian %>% filter(Age > 30)
head(filtered_age)

filtered_heart_caesarian <- caeserian %>%
  filter(Heart == "Yes", Caesarian == "Yes")
head(filtered_heart_caesarian)

filtered_medium_morning <- caeserian %>%
  filter(weight.kg. == "Medium", Delivery_time == "Morning")
head(filtered_medium_morning)

filtered_first_highbp <- caeserian %>%
  filter(Delivery_number == "First-time", Blood == "high")
head(filtered_first_highbp)




table(caeserian$Caesarian)
prop.table(table(caeserian$Caesarian)) * 100

minority <- caeserian %>% filter(Caesarian == "No_Caeser")  
majority <- caeserian %>% filter(Caesarian == "Yes")        
set.seed(123)
minority_oversampled <- minority[sample(1:nrow(minority), nrow(majority), replace = TRUE), ]
balanced_data <- rbind(majority, minority_oversampled)
balanced_data <- balanced_data[sample(1:nrow(balanced_data)), ]
table(balanced_data$Caesarian)
prop.table(table(balanced_data$Caesarian)) * 100





set.seed(123)
split_index <- sample(1:nrow(balanced_data), size = 0.7 * nrow(balanced_data))
train_set <- balanced_data[split_index, ]
test_set  <- balanced_data[-split_index, ]
cat("Training rows:", nrow(train_set), "\n")
cat("Testing rows :", nrow(test_set), "\n")




age_stats_blood <- balanced_data %>%
  group_by(Blood) %>%
  summarise(
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    Mode_Age = as.numeric(names(which.max(table(Age))))
  )
age_stats_blood




age_stats_weight <- balanced_data %>%
  group_by(weight.kg.) %>%
  summarise(
    Mean_Age = mean(Age),
    Median_Age = median(Age),
    Mode_Age = as.numeric(names(which.max(table(Age))))
  )
age_stats_weight



range_val <- function(x) max(x) - min(x)
age_spread_weight <- balanced_data %>%
  group_by(weight.kg.) %>%
  summarise(
    Range = range_val(Age),
    IQR = IQR(Age),
    Variance = var(Age),
    Std_Dev = sd(Age)
  )
age_spread_weight

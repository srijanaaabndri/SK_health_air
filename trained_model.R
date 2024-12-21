
library(caret)
library(randomForest)


dataset <- read.csv("nepal_respiratory_disease_symptoms_data.csv")

str(dataset)


dataset$Disease_Type <- as.factor(dataset$Disease_Type)

dataset <- na.omit(dataset)  

symptom_columns <- c("Cough", "Wheezing", "Shortness_of_breath", "Chest_tightness",
                     "Trouble_in_sleeping", "Fever", "Sweating_and_chills", "Chest_pain",
                     "Difficulty_breathing", "Chronic_cough", "Fatigue", "Bleeding_cough",
                     "Weight_loss", "Loss_of_appetite", "Weakness", "Night_sweats", 
                     "Eye_Change", "Hoarseness")

dataset[symptom_columns] <- lapply(dataset[symptom_columns], function(x) as.integer(x > 0))

dataset$symptoms_count <- rowSums(dataset[symptom_columns])

set.seed(123) 
train_index <- createDataPartition(dataset$Disease_Type, p = 0.8, list = FALSE)
train_data <- dataset[train_index, ]
test_data <- dataset[-train_index, ]

model <- randomForest(Disease_Type ~ AQI + Temperature + Humidity + symptoms_count,
                      data = train_data, ntree = 100)

saveRDS(model, "model.rds")

predictions <- predict(model, test_data)  
confusion_matrix <- confusionMatrix(predictions, test_data$Disease_Type)  

print(confusion_matrix)


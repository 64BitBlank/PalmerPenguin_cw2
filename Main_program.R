# install.packages("palmerpenguins")
# install.packages("readr")

# Load the palmerpenguins package
library(palmerpenguins)
library(readr)

# Load the penguins dataset
data("penguins_raw")

# View the first few rows of the dataset
head(penguins_raw)

# Remove rows with NA values
penguins_cleaned <- na.omit(penguins_raw)
# Remove the "year," "sex," and "island" columns
penguins_cleaned <- penguins_cleaned[, !(names(penguins_cleaned) %in% c("year", "sex", "island"))]
# Identify the index of the "species" column
species_col_index <- which(names(penguins_cleaned) == "species")
# Move the "species" column to the last position
penguins_cleaned <- penguins_cleaned[, c(1:(species_col_index-1), (species_col_index+1):ncol(penguins_cleaned), species_col_index)]
# Remove the original "species" column
penguins_cleaned <- penguins_cleaned[, -species_col_index]
# Remove the column headers
colnames(penguins_cleaned) <- NULL

#Randomly shuffle the dataset rows (repeatedly shuffled for 5 times)
rows_count <- nrow(penguins_cleaned)
for(k in 1:5){
  penguins_cleaned_random<-penguins_cleaned[sample(rows_count),]
}


#Hold out 1/3 rd validation dataset (111)
validation_instances <- sample(nrow(penguins_cleaned_random)/3)
penguins_validation<-penguins_cleaned_random[validation_instances,] #1/3 rd validation set - 111
penguins_train <- penguins_cleaned_random[-validation_instances,] #2/3 rd training set - 111

source("Perceptron.r")
source("Evaluation_Cross_Validation.r")
source("Evaluation_Validation.r")
source("Evaluation_Curves.r")


penguin_model <- Perceptron(0.01)


#Set number of epochs (iterations)
num_of_epochs <- 1000 #Ideally, run with 1000 number of epochs but 1000 takes considerable amount (>10 min) to train

#plot Learning Curve - Accuracy vs Training Sample size
plot_learning_curve(penguin_model, penguins_train, penguins_validation, number_of_iterations = num_of_epochs)

#plot Learning Curve - Accuracy vs Number of Epochs (Iterations)
plot_learning_curve_epochs(penguin_model, penguins_train, penguins_validation)

#plot Learning Curve - Accuracy vs Learning Rate values
plot_learning_curve_learning_Rates(penguins_train, penguins_validation, num_of_epochs = num_of_epochs)

#Train - Test - Cross Validate accross 10 folds
Cross_Validate(penguin_model, penguins_train, num_of_iterations = num_of_epochs, num_of_folds = 10)
#Cross_Validate(ml_model, dataset, num_of_iterations, num_of_folds)

#Validate results with held out validation dataset

Validate(penguin_model, penguins_train, penguins_validation, number_of_iterations = 10)

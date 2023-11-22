# Load the palmerpenguins package
library(palmerpenguins)

# Load the raw penguins dataset without column headers
penguins_raw <- palmerpenguins::penguins_raw

# Extract specific columns
selected_columns <- penguins_raw[, c("Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Species")]
# Remove rows with NA values
penguins_cleaned <- na.omit(selected_columns)

# Combine scaled numeric columns with the "Species" column
penguins_speciesReformat <- cbind(penguins_cleaned, Species_new = sapply(strsplit(as.character(penguins_cleaned$Species), " "), "[", 1))
# Remove the original "Species" column
penguins_speciesReformat <- penguins_speciesReformat[, !colnames(penguins_speciesReformat) %in% c("Species")]
# Remove the column headers
colnames(penguins_speciesReformat) <- NULL

#Randomly shuffle the dataset rows (repeatedly shuffled for 5 times)
rows_count <- nrow(penguins_speciesReformat)
for(k in 1:50){
  penguins_cleaned_random<-penguins_speciesReformat[sample(rows_count),]
}

#Hold out 1/3 rd validation dataset (114)
validation_instances <- sample(nrow(penguins_cleaned_random)/3)
penguins_validation<-penguins_cleaned_random[validation_instances,] #1/3 rd validation set - 114
penguins_train <- penguins_cleaned_random[-validation_instances,] #2/3 rd training set - 228

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

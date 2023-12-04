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

set.seed(69)
penguins_cleaned_random <- penguins_speciesReformat[sample(nrow(penguins_speciesReformat)), ]

train_size_list <- c(0.5, 0.7, 0.9)
learning_rate_list <- c(0.001, 0.01, 0.1, 0.5)
epochs_list <- c(10, 50, 100, 200, 500, 1000, 5000)

source("Perceptron.r")
source("Evaluation_Cross_Validation.r")
source("Evaluation_Validation.r")
source("Evaluation_Curves.r")

# Initialize an empty dataframe
result_df <- data.frame(Precision = numeric(),
                        Recall = numeric(),
                        F1_Score = numeric(),
                        Accuracy_Train = numeric(),
                        Accuracy_Val = numeric())

for (train_size in train_size_list) {
  
  # Split the data into training and testing sets
  validation_instances <- sample(nrow(penguins_cleaned_random) * train_size)
  penguins_validation <- penguins_cleaned_random[validation_instances, ]
  penguins_train <- penguins_cleaned_random[-validation_instances, ]  
  
  for (learning_rate in learning_rate_list) {
    
    penguin_model <- Perceptron(learning_rate)
    
    for (epochs in epochs_list) {
      num_of_epochs <- epochs
      
      cat(paste(
        "\n\n\nTrainSize =", train_size,
        " | LearningRate =", learning_rate,
        " | Epochs =", epochs, "\n"
      ))
      
      
      #plot Learning Curve - Accuracy vs Training Sample size
      plot_learning_curve(penguin_model, penguins_train, penguins_validation, number_of_iterations = num_of_epochs)
      
      #plot Learning Curve - Accuracy vs Number of Epochs (Iterations)
      plot_learning_curve_epochs(penguin_model, penguins_train, penguins_validation)
      
      #plot Learning Curve - Accuracy vs Learning Rate values
      plot_learning_curve_learning_Rates(penguins_train, penguins_validation, num_of_epochs = num_of_epochs)
      
      #Train - Test - Cross Validate accross 10 folds
      Cross_Validate(penguin_model, penguins_train, num_of_iterations = num_of_epochs, num_of_folds = 10)
      
      #Validate results with held out validation dataset
      iteration_result <- Validate(penguin_model, penguins_train, penguins_validation, number_of_iterations = 10)
      
      # Append the results to the existing dataframe
      result_df <- rbind(result_df, iteration_result)
    }
  }
}

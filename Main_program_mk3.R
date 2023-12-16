library(palmerpenguins)
library(ggplot2)
library(tidyr)

# Load and preprocess the penguin data
penguins_raw <- palmerpenguins::penguins_raw
selected_columns <- penguins_raw[, c("Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)", "Body Mass (g)", "Species")]
penguins_cleaned <- na.omit(selected_columns)

# Reformatting species names and converting them to numeric
penguins_random <- cbind(penguins_cleaned, Species_new = as.numeric(factor(penguins_cleaned$Species)))
penguins_random <- penguins_random[, !names(penguins_random) %in% "Species"]

set.seed(69)

# Define training parameters
train_size_list <- c(0.5, 0.3, 0.1)
learning_rate_list <- c(0.001, 0.01, 0.1, 0.5)
epochs_list <- c(10, 50, 100, 500, 1000, 5000)

penguins_random <- penguins_random[sample(nrow(penguins_random)), ]

scale_values <- function(x) {
  num_cols <- ncol(x)
  for (i in 1:(num_cols - 1)) {
    x[, i] <- (x[, i] - min(x[, i])) / (max(x[, i]) - min(x[, i]))
  }
  return(x)
}
penguins_random <- scale_values(penguins_random)

# Source the MLP and other necessary scripts
source("MLP.r")
source("Perceptron.r")
source("Evaluation_Cross_Validation.r")
source("Evaluation_Validation.r")
source("Evaluation_Curves.r")

# Define MLP parameters
input_size <- 4  # Number of features
hidden_layers <- c(4, 3)  # Number of neurons in the hidden layer
output_size <- 3  # Number of output classes

# Split the dataset into training and testing sets
validation_instances <- sample(nrow(penguins_random) * 0.5)
penguins_validation <- penguins_random[validation_instances, ]
penguins_train <- penguins_random[-validation_instances, ]


learning_rate <- 0.001
epochs <- 100  # Set the number of epochs

# Create the MLP network
network <- create_mlp_network(input_size, hidden_layers, output_size)

# Train the network
for (epoch in 1:epochs) {
  for (i in 1:nrow(penguins_train)) {
    # Extract the inputs and expected outputs from your training data
    inputs <- as.matrix(penguins_train[i, -ncol(penguins_train)])
    expected_output <- as.numeric(factor(penguins_train[i, ncol(penguins_train)]))
    
    # Perform forward propagation
    activations <- forward_propagation(network, inputs)
    print(activations)
    backward_propagation(network, activations, expected_output, learning_rate, inputs)
  }
}
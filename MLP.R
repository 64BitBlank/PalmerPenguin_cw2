# Define the structure of a neuron layer
create_layer <- function(n_input, n_neurons, activation_func, layerIdx) {
  layer <- list()
  layer$idx <- layerIdx
  if(activation_func ==  'relu'){
    std_dev <- sqrt(2 / n_input)
    layer$weights <- matrix(rnorm(n_input, mean = 0, sd = std_dev),n_input,n_neurons)
    layer$biases <- matrix(0,1,n_neurons)
  } else {
    std_dev <- sqrt(2 / (n_input + n_neurons))
    layer$weights <- matrix(rnorm(n_input * n_neurons, mean = 0, sd = std_dev),n_input, n_neurons)
    layer$biases <- matrix(0,1,n_neurons)
  }
  layer$activation_func <- activation_func
  return(layer)
}

# Activation Functions
sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

relu <- function(x) {
  pmax(0, x)
}

relu_derivative <- function(x) {
  ifelse(x > 0, 1, 0)
}

softmax <- function(x) {
  exp_x <- exp(x - max(x))
  exp_x / sum(exp_x)
}


# Forward Propagation
forward_propagation <- function(network, input) {
  activations <- list(input)
  current_input <- input

  for (layer in network) {
    if(layer$idx == 2){
      z <- current_input %*% layer$weights + layer$biases
    } else{
      z <- current_input %*% t(layer$weights) + layer$biases
    }
    a <- match.fun(layer$activation_func)(z)
    activations <- append(activations, list(a))
    current_input <- a
  }

  return(activations)
}

# Backpropagation
backward_propagation <- function(network, activations, expected_output, learning_rate, inputs) {
  gradients <- list()
  bias_gradients <- list()
  errors <- list()
  n_layers <- length(network)
  initial_error <- activations[[n_layers + 1]] - expected_output

  output_delta <- matrix(initial_error, nrow = 1)

  output_weights_gradient <- activations[[3]] %*% output_delta

  output_bias_gradient <- colSums(output_delta)

  hidden_layer_2_error <- (network[[2]]$weights %*% matrix(output_delta)) * as.numeric(relu_derivative(activations[[3]]))
  hidden_layer_2_weights_gradient <- t(activations[[2]]) %*% as.numeric(hidden_layer_2_error)
  hidden_layer_2_bias_gradient <- colSums(hidden_layer_2_error)

  hidden_layer_1_error <- (network[[1]]$weights %*% hidden_layer_2_error) * as.numeric(relu_derivative(activations[[2]]))
  input_layer_weights_gradient <- inputs %*% as.numeric(hidden_layer_1_error)
  input_layer_bias_gradient <- colSums(hidden_layer_1_error)

  network[[1]]$weights <- network[[1]]$weights - learning_rate * as.numeric(input_layer_weights_gradient)
  network[[1]]$biases <- network[[1]]$biases - learning_rate * input_layer_bias_gradient

  network[[2]]$weights <- network[[2]]$weights - learning_rate * as.numeric(hidden_layer_2_weights_gradient)
  network[[2]]$biases <- network[[2]]$biases - learning_rate * hidden_layer_2_bias_gradient

  network[[3]]$weights <- network[[3]]$weights - learning_rate * as.numeric(output_weights_gradient)
  network[[3]]$biases <- network[[3]]$biases - learning_rate * output_bias_gradient

}

# Define a function to create a multi-layer perceptron network
create_mlp_network <- function(input_size, hidden_layers, output_size) {
  network <- list()
  prev_layer_size <- input_size

  for (n_neurons in hidden_layers) {
    layerIdx <- length(network) + 1
    network <- append(network, list(create_layer(prev_layer_size, n_neurons, "relu", layerIdx)))
    prev_layer_size <- n_neurons
  }

  network <- append(network, list(create_layer(prev_layer_size, output_size, "softmax",3)))
  return(network)
}

# Ligistisc_regression
#
#
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# x_train = read.csv2("D:/DDox/ML/Project/ML_FinalProject_Team17/cardio_train.csv", sep = ";")
# y_train = x_train["cardio"]
# x_train <- subset(x_train, select = -c(1, 2, 3, 4,5, 6, 13))
# x_train = head(x_train, 100)
# y_train = head(y_train, 100)
#
# x_train = as.matrix(x_train)


# uploading mtcars data from tidyverse library

# df <- mtcars
# x_train <- df[, c(8,9)]
# y_train <- df[, 1]
# x_train = as.matrix(x_train)


#logistic_regression(x_train,y_train, 20)
#(beta_next = logistic_regression_trainer_helper(beta_next, data, targs, lr = .0001))
#y_pred = logistic_reg_predict_dataset(x_train, beta_next)
#loss(y_train, y_pred)

loss = function(y_pred, y_train) {
  sum((y_pred - y_train)^2)
}

#' Runs logistic regression on dataset. 
#' Returns a column of predictions. 
#' This WILL OVERFIT to data, so it is prefered to get beta first and use your own.
logistic_regression = function(x_train, y_train, num_epochs = 20) {
  beta_cur = logistic_regression_trainer(x_train, y_train, num_epochs = num_epochs)
  y_pred = logistic_reg_predict_dataset(x_train, beta_cur)
  return(make_ones_and_zeroes(y_pred))
}


#'Given a training dataset and a Beta will predict the target for each x_value
#'x_train is a Matrix or dataframe... need to double check
#'beta is a vector of length num_features. This will be found by logistic_regression_trainer
#'Returns it's prediction of the targets (a column of confidences)
#'
#' This gives a column of p_i
logistic_reg_predict_dataset = function(x_train, beta) {
  y_pred = rep(0,length(x_train[,1]))
  data = as.matrix(x_train)

  temp_func = function(row) {predict_row(beta, row)}

  for (i in 1:length(x_train[,1])) {
    row = x_train[i,]
    y_pred[i] = predict_row(beta, row)
  }

  # Wanted to vectorize it... Rstudio just crashed instead.
  #y_pred = apply(X = data, MARGIN = 1, FUN = temp_func)
  # Probably should have this be in a different function...
  #y_pred[y_pred >= .5] = 1
  #y_pred[y_pred < .5] = 0

  return(y_pred)
}

#' This function takes a column of values on U(0,1)
#' Makes the column become 1's and 0's based on cutoff
#' Default has cutoff of 0.5
make_ones_and_zeroes = function(y_pred, cutoff = .5) {
  y_pred[y_pred >= cutoff] = 1
  y_pred[y_pred < cutoff] = 0
  
  return(y_pred)
}


#' This function will take in a training dataset x_train and its targets y_train.
#' It will return the vector Beta that it determines using gradient descent on dataset
#' Can specify number of epochs, but defaults to 20
#' Returns Beta
logistic_regression_trainer <- function(x_train, y_train, num_epochs = 20) {
  # Takes in dataframe and returns beta for best fit.
  data = as.matrix(x_train) # Cast to matrix so things work
  targs = as.matrix(y_train)

  beta_init = solve(t(data) %*% data) %*% t(data) %*% targs

  beta_cur = beta_init



  for (i in 1:num_epochs){
    beta_next = logistic_regression_trainer_helper(beta_cur, data, targs)

    #if (is_close(beta_next, beta_cur)) {
    #  break
    #}
  }

  return(beta_cur)
}

#' I was trying something here, but I removed it because R wasn't happy with me
is_close = function(b1, b2){
  threshold = .01
  dist_sqred = sum((b1 - b2)**2)
  if (dist_sqred <= threshold){
    return(TRUE)
  }
  return(FALSE)
}

#' Helper function that does 1 step of gradient descent on our Beta vector
#' Takes in a beta vector, training set, targets for training set
#' Returns a new beta vector. 
#' If you find that the loss goes up instead of down as a result of this step,
#' try lowering the learning rate because it probably overstepped a local minimum.
logistic_regression_trainer_helper = function(beta_init, x_train, y_train, lr = 1) {
  beta_cur  = beta_init
  weight_changes = rep(0, length(beta_cur))

  for (i in 1:length(x_train[,1])){
    row = x_train[i,]
    y_i = y_train[i]

    s_i = predict_row(beta_cur, row)

    row_grad = (y_i -  s_i) * row

    weight_changes = weight_changes - row_grad
  }

  # Update beta_cur vector
  beta_cur = beta_cur - weight_changes*lr*(1/length(x_train[,1]))

  return(beta_cur)
}

#' Uses sigmoid function to predict row.
#' Takes a beta and row as input. Both vectors of the same length
#'     There is a dot product, so they must be same length
predict_row = function(beta, row) {
  sigmoid(beta, row)
}

#' Yeah, this feels a bit redundant
#' Why not just call this instead of predict_row?
#'     A: Because I could change predictor function if I really wanted by adding an optional 
#'        variable to predict_row and letting user specify function
#' Returns p_i from the loss function from Final_Project html
sigmoid = function(beta, x_vec) {
  1/(1 + exp(-as.vector(x_vec) %*% as.vector(beta)))
}

####Here is the code of making plot and boostrap
#Bootstrap
Inter<-function(B=20,alpha,data){
  n <- length(data)
  boot_mean <- rep(NA, B)
  for (i in 1:B){
    work_star <- data[sample(1:n, replace = TRUE)]
    boot_mean[i] <- mean(work_star)
  }
  return(quantile(boot_mean, c(alpha/2, 1 - alpha/2)))
}
#plot
plot(y ~ x, data=data, col="steelblue")
lines(y ~ x, newdata, lwd=2)

#### This is the IDEA of how we can find the Confusion Matrix.

confusion_matrix <- function(y_pred, y_train){
  y_pred = ifelse(y_pred>0.5, 1, 0)

  matrix_table = table(y_pred, y_train)


  prevalence = matrix_table[4]/sum(matrix_table[1:4])
  accuracy = sum(matrix_table[1], matrix_table[4])/sum(matrix_table[1:4])
  sensitivity = matrix_table[4] / sum(matrix_table[4], matrix_table[3])
  specificity = matrix_table[1] / matrix_table(cm[1], matrix_table[2])
  fscore = (2 * (sensitivity * Prevalence))/(sensitivity + Prevalence)
  DOR = (matrix_table[4]/matrix_table[3])/(matrix_table[2]/matrix_table[1])

  return(matrix_table, prevalence, accuracy, sensitivity, specificity, fscore, DOR)
}




































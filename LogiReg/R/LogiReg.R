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

logistic_regression = function(x_train, y_train) {
  beta = logistic_regression_trainer(x_train, y_train)
  
  return(logistic_reg_predict_dataset(x_train, beta))
}

logistic_reg_predict_dataset = function(x_train, beta) {
  y_pred = rep(0,length(x_train[,1]))
  
  for (i in 1:length(x_train[,1])) {
    row = x_train[i,]
    y_pred[i] = predict_row(beta, row)
  }
  
  return(y_pred)
}

logistic_regression_trainer <- function(x_train, y_train, num_epochs = 20) {
  # Takes in dataframe and returns beta for best fit. 
  beta_init = solve(t(x_train) %*% x_train) %*% x_train %*% y_train
  beta_cur = beta_init
  
  
  
  for (i in 1:num_epochs){
    beta_next = logistic_regression_trainer_helper(beta_cur, x_train, y_train)
    
    if (is_close(beta_next, beta_cur)) {
      break
    }
  }
  
  return(beta_cur)
}

is_close = function(b1, b2){
  threshold = .01
  dist_sqred = sum((b1 - b2)**2)
  if (dist_sqred <= threshold){
    return(TRUE)
  }
  return(FALSE)
}

logistic_regression_trainer_helper = function(beta, x_train, y_train) {
  beta_cur = beta
  weight_changes = rep(0, length(beta_cur))
  lr = 1
  
  for (i in 1:length(x_train[,1])){
    row = x_train[,i]
    y_i = y_train[i]
    
    s_i = predict_row(beta_cur, row)
    
    row_grad = (y_i -  s_i)*row
    Weight_changes = weight_changes - row_grad
  }
  
  # Update beta_cur vector
  beta_cur = beta_cur + weight_changes*lr*(1/length(x_train[,1]))
  
  return(beta_cur)
}

predict_row = function(beta, row) {
  sigmoid(beta, row)
}

sigmoid = function(beta, x_vec) {
  1/(1 + exp(-t(x_vec)%*%beta))
}


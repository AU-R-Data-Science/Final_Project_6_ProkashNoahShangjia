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

x_train = read.csv2("D:/DDox/ML/Project/ML_FinalProject_Team17/cardio_train.csv", sep = ";")
y_train = x_train["cardio"]
x_train <- subset(x_train, select = -c(1, 2, 3, 4,5, 6, 13))
x_train = head(x_train, 100)
y_train = head(y_train, 100)

x_train = as.matrix(x_train)


#logistic_regression(x_train,y_train, 20)
#(beta_next = logistic_regression_trainer_helper(beta_next, data, targs, lr = .0001))
#y_pred = logistic_reg_predict_dataset(x_train, beta_next)
#loss(y_train, y_pred)

loss = function(y_pred, y_train) {
  sum((y_pred - y_train)^2)
}

logistic_regression = function(x_train, y_train, num_epochs = 20) {
  beta_cur = logistic_regression_trainer(x_train, y_train, num_epochs = num_epochs)
  
  return(logistic_reg_predict_dataset(x_train, beta_cur))
}

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
  
  return(y_pred)
}

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

is_close = function(b1, b2){
  threshold = .01
  dist_sqred = sum((b1 - b2)**2)
  if (dist_sqred <= threshold){
    return(TRUE)
  }
  return(FALSE)
}

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

predict_row = function(beta, row) {
  sigmoid(beta, row)
}

sigmoid = function(beta, x_vec) {
  1/(1 + exp(-as.vector(x_vec) %*% as.vector(beta)))
}


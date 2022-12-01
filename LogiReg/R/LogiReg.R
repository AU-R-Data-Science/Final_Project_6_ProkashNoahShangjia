#'  @title Logistic_regression
#'
#'
#'
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

x_train = read.csv2("D:/DDox/ML/Project/ML_FinalProject_Team17/cardio_train.csv", sep = ";")
y_train = x_train["cardio"]
x_train <- subset(x_train, select = -c(1, 2, 3, 4, 5, 6, 13))
x_train = head(x_train, 100)
y_train = head(y_train, 100)

x_train = as.matrix(x_train)


# uploading personal dataset

# df <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Data/Fish_demand_DoubleHurdle/New_dataframe/Data/Main_consumption.xlsx")
# df <- df[1:500, c(9, 11, 13, 36)]
# write.csv(df, file = "df.csv", row.names = F)


#df <- df[1:500, c(9, 11, 13, 36)]

df = read.csv(file = "df.csv")


y_train <- df["urban"]
y_train <- as.vector(y_train)
y_train <- y_train[["urban"]]
y_train <- head(y_train, 500)
#
# a <- df[, c(9,11,13)]
x_train <- subset(df, select = c(1, 2, 3))
x_train <- head(x_train, 500)



#logistic_regression(x_train,y_train, 20)
#(beta_next = logistic_regression_trainer_helper(beta_next, data, targs, lr = .0001))
#y_pred = logistic_reg_predict_dataset(x_train, beta_next)
#loss(y_train, y_pred)


#' Our loss function
loss = function(y_pred, y_train) {
  sum((y_pred - y_train)^2)
}



#' This WILL OVERFIT to data, so it is prefered to get beta first and use your own.
#' @description Runs logistic regression on dataset.
#' @param  x_train \code{datafram} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @param  num_epochs \code{int} number of epochs to train for. Defaults to 20
#' @param  lr \code{double} learning rate of logistic regression. If you are not converging, try lowering it.
#' @param  cutoff A \code{double} on interval (0,1) that says to predict 1 if value >=cutoff. 0 otherwise
#' @return Returns a column of predictions (1's and 0's) with cutoff of .5 by default
#' \describe{
#'
#' }
#' @author Noah Heckenlively
#' @export
logistic_regression = function(x_train, y_train, num_epochs = 20, lr = 1, cutoff = .5) {
  beta_cur = logistic_regression_trainer(x_train, y_train, num_epochs = num_epochs, lr = lr)
  y_pred = logistic_reg_predict_dataset(x_train, beta_cur)
  return(make_ones_and_zeroes(y_pred, cutoff))
}


#' @description Given a Beta, makes predictions on training data x_train
#' @param  x_train \code{datafram} or matrix (gets cast to matrix) that is our set of features.
#' @param  beta_cur Numeric vector of length(number of features) that is found by logistic_regression_trainer
#' @return Returns a column of predictions (the p_i). Every element is on interval (0,1). Use make_ones_and_zeros to get y_hats
#' @author Noah Heckenlively
#' @export
logistic_reg_predict_dataset = function(x_train, beta_cur) {
  y_pred = rep(0,length(x_train[,1]))
  data = as.matrix(x_train)

  #temp_func = function(row) {predict_row(beta, row)}

  for (i in 1:length(x_train[,1])) {
    row = x_train[i,]
    y_pred[i] = predict_row(beta_cur, row)
  }

  # Wanted to vectorize it... Rstudio just crashed instead.
  #y_pred = apply(X = data, MARGIN = 1, FUN = temp_func)
  # Probably should have this be in a different function...
  #y_pred[y_pred >= .5] = 1
  #y_pred[y_pred < .5] = 0

  return(y_pred)
}


#' @description Makes the column become 1's and 0's based on cutoff
#' @param  y_pred Column or list like object of elements on interval (0,1)
#' @param  cutoff Default is 0.5. value of entry in ouput column is 1 if y_pred[i] >= cutoff. 0 Otherwise
#' @return Returns a column of 1's and 0's same length as y_pred
#' @author Noah Heckenlively
#' @export
make_ones_and_zeroes = function(y_pred, cutoff = .5) {
  y_pred[y_pred >= cutoff] = 1
  y_pred[y_pred < cutoff] = 0

  return(y_pred)
}


#' This function will take in a training dataset x_train and its targets y_train.
#' It will return the vector Beta that it determines using gradient descent on dataset
#' Can specify number of epochs, but defaults to 20
#' Returns Beta
#' @description Makes the column become 1's and 0's based on cutoff
#' @param  y_pred Column or list like object of elements on interval (0,1)
#' @param  cutoff Default is 0.5. value of entry in ouput column is 1 if y_pred[i] >= cutoff. 0 Otherwise
#' @return Returns a column of 1's and 0's same length as y_pred
#' @author Noah Heckenlively
#' @export
logistic_regression_trainer <- function(x_train, y_train, num_epochs = 20, lr = 1) {
  # Takes in dataframe and returns beta for best fit.
  data = as.matrix(x_train) # Cast to matrix so things work
  targs = as.matrix(y_train)

  beta_init = solve(t(data) %*% data) %*% t(data) %*% targs

  beta_cur = beta_init



  for (i in 1:num_epochs){
    beta_cur = logistic_regression_trainer_helper(beta_cur, data, targs, lr = lr)

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
predict_row = function(beta_cur, row) {
  vec = c()
  for (i in 1:length(row)){
    vec = c(vec,row[[i]])
  }
  sigmoid(beta_cur, vec)
}

#' Yeah, this feels a bit redundant
#' Why not just call this instead of predict_row?
#'     A: Because I could change predictor function if I really wanted by adding an optional
#'        variable to predict_row and letting user specify function
#' Returns p_i from the loss function from Final_Project html
sigmoid = function(beta_cur, x_vec) {
  1/(1 + exp(-(as.vector(x_vec) %*% as.vector(beta_cur))))
}








####Here is the code of making plot and boostrap
#'Bootstrap
#'@param B number of bootstraps which by default will be 20
#'@param alpha is the significance level α to obtain for the 1−α confidence intervals for beta
#'@param x_train the matrix for independent variable in dataset
#'@param y_train the matrix for dependent variable in dataset
ConI<-function(B=20,alpha,x_train,y_train){
  B<-20
  rownumber<-ncol(x_train)
  beta_mat<-matrix(NA,B,rownumber+1)
  for (i in 1:B) {
    smp<-sample(1:nrow(x_train), replace = T)
    beta_mat[i,]<-logistic_regression_trainer(x_train[smp,], y_train[smp])
  }
  beta_ci<-matrix(NA,rownumber,2)
  for (i in 1:rownumber) {
    beta_ci[i,]<-quantile(beta_mat[i,], c(alpha/2, 1 - alpha/2))
  }
  row.names(beta_ci)<-colnames(x_train)
  return(beta_ci)
}
#'Plot of the fitted logistic curve to the actual values
#'@param x_train the matrix for independent variable in dataset
#'@param y_train the matrix for dependent variable in dataset
#'@param color color code or name, see colors, palette. Here NULL means colour "steelblue".
#'@param line_width line width, also used for (non-filled) plot symbols, see lines and points.
logiregPlot<-finction(x_train,y_train,color="steelblue",line_width=2){


  colnumber<-ncol(x_train)
  rownumber<-rownumber(x_train)
  new_x_train<-matrix(1,colnumber,rownumber)
  new_x_train[1,]<-x_train[1,]
  beta<-logistic_regression_trainer(new_x_train,y_train)
  p_hat<-logistic_reg_predict_dataset(x_train,beta)
  p_hat<-ifelse(p_hat>0.5, 1, 0)
  plot(p_hat ~ x_train[1,], col=color)
  lines(p_hat ~ x_train[1,], newdata, lwd=line_width)

}



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


#### Let the user to plot of Accuracy over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.
#'This function will provide you a plot of accuracy over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.
#'@param x_train the matrix for independent variable in data set
#'@param y_train the matrix for dependent variable in data set
library(ISLR)
library(caret)
library(lattice)
Make_table<-function(x_train,y_train){

  cut_off_value<-seq(0.1,0.9,by=0.1)
  beta_h<-logistic_regression_trainer(new_x_train,y_train)
  p_h<-p_hat<-logistic_reg_predict_dataset(x_train,beta)
  Accuracy<-matrix(NA,9,1)
  row.names(Accuracy)<-cut_off_value
  for (i in 1:9) {
    cutt<-cut_off_value[i]
    p_hat<-ifelse(p_hat>cutt, 1, 0)
    a<-confusionMatrix(as.factor(p_h),as.factor(y_train))
    a<-as.matrix(a)
    TP<-a[1,1]
    FN<-a[1,2]
    FP<-a[2,1]
    TN<-a[2,2]
    Accuracy[i,]<-(TP+TN)/(TP+TN+FP+FN)
  }
  return(Accuracy)
}
































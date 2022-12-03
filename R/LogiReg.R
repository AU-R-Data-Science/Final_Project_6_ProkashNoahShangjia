#'
#'
#'
#
#   Install Package:           'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

# x_train = read.csv2("D:/DDox/ML/Project/ML_FinalProject_Team17/cardio_train.csv", sep = ";")
# y_train = x_train["cardio"]
# x_train <- subset(x_train, select = -c(1, 2, 3, 4, 5, 6, 13))
# x_train = head(x_train, 100)
# y_train = head(y_train, 100)
#
# x_train = as.matrix(x_train)


# uploading personal dataset

#df <- read_excel("C:/Users/proka/OneDrive - Auburn University/Auburn/Research/Data/Fish_demand_DoubleHurdle/New_dataframe/Data/Main_consumption.xlsx")
#df <- df[1:500, c(3:6, 9, 11, 13, 36)]
# x_train <- data.matrix(df[,1:3], rownames.force = NA)
# y_train <- data.matrix(df[,4], rownames.force = NA)
# write.csv(df, file = "df.csv", row.names = F)


#df <- df[1:500, c(9, 11, 13, 36)]

#df = read.csv(file = "df.csv")


#y_train <- df["urban"]
#y_train <- as.vector(y_train)
#y_train <- y_train[["urban"]]
#y_train <- head(y_train, 500)
#
# a <- df[, c(9,11,13)]
#x_train <- subset(df, select = c(1, 2, 3))
#x_train <- head(x_train, 500)



#logistic_regression(x_train,y_train, 20)
#(beta_next = logistic_regression_trainer_helper(beta_next, data, targs, lr = .0001))
#y_pred = logistic_reg_predict_dataset(x_train, beta_next)
#loss(y_train, y_pred)

#' @title Loss Function
#' @description Our loss function for the logistic regression
#' @param  y_pred column of p_i
#' @param  y_train column of 1's and 0's
#' @return Loss, a numeric type
#' @export
loss = function(y_pred, y_train) {
  #sum((y_pred - y_train)^2)
  sum((-y_train*log(y_pred) - (1 - y_train)*log(1 - y_pred)))
}



#' @title Logistic_regression
#' @description Runs logistic regression on dataset.
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @param  num_epochs \code{int} number of epochs to train for. Defaults to 20
#' @param  lr \code{double} learning rate of logistic regression. If you are not converging, try lowering it.
#' @param  cutoff A \code{double} on interval (0,1) that says to predict 1 if value >=cutoff. 0 otherwise
#' @return Returns a column of predictions (1's and 0's) with cutoff of .5 by default
#' \describe{
#'     This WILL OVERFIT to data, so it is prefered to get beta first and use your own.
#' }
#' @author Noah Heckenlively
#' @export
logistic_regression = function(x_train, y_train, num_epochs = 20, lr = 1, cutoff = .5) {
logistic_regression = function(x_train, y_train, num_epochs = 100, lr = 1, cutoff = .5) {
  beta_cur = logistic_regression_trainer(x_train, y_train, num_epochs = num_epochs, lr = lr)
  y_pred = logistic_reg_predict_dataset(x_train, beta_cur)
  return(make_ones_and_zeroes(y_pred, cutoff))
}


#' @title Predicts Dataset
#' @description Given a Beta, makes predictions on training data x_train
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  beta_cur Numeric vector of length(number of features) that is found by logistic_regression_trainer
#' @return Returns a column of predictions (the p_i). Every element is on interval (0,1). Use make_ones_and_zeros to get y_hats
#' @author Noah Heckenlively
#' @export
logistic_reg_predict_dataset = function(x_train, beta_cur) {
  y_pred = rep(0,nrow(x_train))
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

#' @title Make Ones and Zeroes
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


#' @title Trains Logistic Regression
#' @description Runs gradient descent to find beta
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @param  num_epochs \code{int} number of epochs to train for. Defaults to 20
#' @param  lr \code{double} learning rate of logistic regression. If you are not converging, try lowering it.
#' @return Returns Beta, a vector of length num_features that gets dotted with each row in sigmoid function during prediction.
#' @author Noah Heckenlively
#' @export
logistic_regression_trainer <- function(x_train, y_train, num_epochs = 20, lr = 1) {
  # Takes in dataframe and returns beta for best fit.
  data = as.matrix(x_train) # Cast to matrix so things work
  targs = as.matrix(y_train)

  beta_cur = make_beta_init(data, targs)

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

#' @title Beta Init
#' @description Gets beta_init
#' @param  data \code{matrix} of training data
#' @param  targs \code{matrix} values of the target
#' @return Returns Beta_init
#' @export
make_beta_init = function(data, targs){
  beta_init = solve(t(data) %*% data) %*% t(data) %*% targs

  return(beta_init)
}

#' @title Logistic Regression Training Helper Function
#' @description Helper function that does 1 step of gradient descent on our Beta vector
#' @param  beta_start Numeric vector of length(number of features) that used to predict target from data
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @param  lr \code{double} learning rate of logistic regression. If loss goes up instead of down, lower learning rate.
#'             You jumped over local minimum.
#' @return Returns next beta vector Should be a better value than beta_start
#' @author Noah Heckenlively
#' @export
logistic_regression_trainer_helper = function(beta_start, x_train, y_train, lr = 1) {
  beta_cur  = beta_start # I do this so I am not changing the variable I pass in
  weight_changes = rep(0, length(beta_cur))

  for (i in 1:length(x_train[,1])){
    row = x_train[i,]
    y_i = y_train[i]

    s_i = predict_row(beta_cur, row)

    #row_grad = (y_i -  s_i) * row
    row_grad = -(y_i/s_i - (1 - y_i)/(1 - s_i))*(s_i * (1 - s_i)) # Gradient

    weight_changes = weight_changes + row_grad
  }

  # Update beta_cur vector
  beta_cur = beta_cur - weight_changes*lr*(1/length(x_train[,1]))

  return(beta_cur)
}


#' @title Predict Row
#' @description  Predicts target from the row. beta_cur and row must be same length because of dot product
#' @param  beta_cur Numeric vector of length(number of features) that used to predict target from data
#' @param  row A row from out dataset that we are trying to predict on.
#' @return P_i
#' @author Noah Heckenlively
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
#' @description  Sigmoid function... what logistict regression uses to predict
#' @param  beta_cur Numeric vector of length(number of features) that used to predict target from data
#' @param  x_vec A row from out dataset that we are trying to predict on.
#' @return P_i
#' @author Noah Heckenlively
sigmoid = function(beta_cur, x_vec) {
  1/(1 + exp(-(as.vector(x_vec) %*% as.vector(beta_cur))))
}








####Here is the code of making plot and boostrap
#' @title Confidence Interval
#' @description The is the function for confident interval by using Bootstrap
#' @param B \code{numeric} of bootstraps which by default will be 20
#' @param alpha \code{numeric} is the significance level to obtain the confidence intervals for beta
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @author Shangjia Li
#' @export
ConI<-function(alpha,x_train,y_train,B=20){
  colnumber<-ncol(x_train)
  beta_mat<-matrix(NA,B,colnumber)
  for (i in 1:B) {
    smp<-sample(1:nrow(x_train), replace = T)
    beta_mat[i,]<-logistic_regression_trainer(x_train[smp,], y_train[smp])
  }
  beta_ci<-matrix(NA,colnumber,2)
  for (j in 1:colnumber) {
    beta_ci[j,]<-quantile(beta_mat[j,], c(alpha/2, 1 - alpha/2))
  }
  row.names(beta_ci)<-colnames(x_train)
  return(beta_ci)
}
#' @title Logistic Regression Plot
#' @description Plot of the fitted logistic curve to the actual values
#' @param  x_train \code{dataframe} or matrix (gets cast to matrix) that is our set of features.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @param color color code or name, see colors, palette. Here NULL means colour "steelblue".
#' @param line_width line width, also used for (non-filled) plot symbols, see lines and points.
#' @param beta \code{dataframe} or matrix (gets cast to matrix) that is calculate by function logistic_regression_trainer.
#' @author  Shangjia Li and Noah Heckenlively
#' @export
logireg_Plot<-function(x_train, y_train, color="steelblue",line_width=2, num_epochs = 200, lr = .01){
  colnumber<-ncol(x_train)
  rownumber<-nrow(x_train)
  new_x_train<-matrix(1,rownumber,1)
  new_x_train<-cbind(x_train[,1],new_x_train)

  plot(x_train[,1],y_train, col=color,xlab="x",ylab="predict")


  beta_est <- logistic_regression_trainer(x_train = new_x_train, y_train = y_train, num_epochs = num_epochs, lr= lr)


  inc_list = seq(min(x_train[,1]), max(x_train[,1]), .05)
  new_inc_train<-matrix(1,length(inc_list),1)
  liners = cbind(inc_list,new_inc_train)
  lin_pred = logistic_reg_predict_dataset(as.matrix(liners), beta_est)

  lines(inc_list, lin_pred, lwd=line_width)
}

#### This is the IDEA of how we can find the Confusion Matrix.


#' Title
#' @title Confusion Matrix
#' @param y_pred (this is the predicted value of target variable)
#' @param y_train (this is the actual target variable)
#' @param cutoff_value this is the cut off value. The default is 0.5
#' @return Confusion matrix, prevalence, accuracy, sensitivity, specificity, False Discovery Rate, Diagnostic Odds Ratio
#' @export
confusion_matrix <- function(y_pred, y_train, cutoff_value=0.5){
  y_pred = ifelse(y_pred>cutoff_value, 1, 0)

matrix_table=table(y_pred, y_train)

  prevalence = matrix_table[4]/sum(matrix_table[1:4])
  accuracy = sum(matrix_table[1], matrix_table[4])/ sum(matrix_table[1:4])
  sensitivity = matrix_table[4] / sum(matrix_table[4], matrix_table[3])
  specificity = matrix_table[1] / sum(matrix_table[1], matrix_table[2])
  fscore = (2 * (sensitivity * prevalence))/(sensitivity + prevalence)
  DOR = (matrix_table[4]/matrix_table[3])/(matrix_table[2]/matrix_table[1])

  print(c("confusion matrix" = print.table(matrix_table),
        "prevalence" = prevalence,
        "accuracy" = accuracy,
        "sensitivity" = sensitivity,
        "specificity" = specificity,
        "fscore" = fscore,
        "DOR" = DOR))

}


#### Let the user to plot of Accuracy over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.
#' @title Accuracy Plots over Cutoff Values
#' @description This function will provide you a plot of accuracy over a grid of cut-off values for prediction going from 0.1 to 0.9 with steps of 0.1.Partial Function is build up based on previous confusion_matrix function.
#' @param  y_pred \code{dataframe} or matrix (gets cast to matrix) that is our set of predictions.
#' @param  y_train \code{dataframe} value of the target. Gets cast to matrix
#' @author Shangjia Li
#' @export
Make_table<-function(y_pred,y_train){
  cut_off_value<-seq(0.1,0.9,by=0.1)
  Accuracy<-matrix(NA,9,1)
  row.names(Accuracy)<-cut_off_value
  for (i in 1:9) {
    cutt<-cut_off_value[i]
    cm=c_matrix(y_pred,y_train,cutt)
    Accuracy[i,]<-get_accuracy(cm)
  }
  return(Accuracy)

}

get_accuracy<-function(matrix_table){
  accuracy = sum(matrix_table[1], matrix_table[4])/ sum(matrix_table[1:4])
  return(accuracy)
}

c_matrix <- function(y_pred, y_train, cutoff_value=0.5){
  y_pred = ifelse(y_pred>cutoff_value, 1, 0)
  matrix_table=table(y_pred, y_train)
  if (nrow(matrix_table)==1) {
    if(row.names(matrix_table)=='1'){
      add<-c(0,0)
      matrix_table<-rbind(add,matrix_table)
    }else{
      add<-c(0,0)
      matrix_table<-rbind(matrix_table,add)
    }
  }
  return(matrix_table)
}





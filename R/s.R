
#y_pred <- logistic_regression(x_train, y_train, 40)

#lr <- 0.01


#beta_1 <- logistic_regression_trainer(x_train, y_train, 1,lr=lr)
#beta_2 <- logistic_regression_trainer(x_train, y_train, 2, lr=lr)
#beta_3 <- logistic_regression_trainer(x_train, y_train, 40, lr=lr)

#y_pred_1 = logistic_reg_predict_dataset(x_train, beta_1)
#y_pred_2 = logistic_reg_predict_dataset(x_train, beta_2)
#y_pred_3 = logistic_reg_predict_dataset(x_train, beta_3)

#plot(y_pred_1)
#plot(y_pred_2)
#plot(y_pred_3)


#a <- c(1,1,0,0,1,0,1,0,1,0)
#b <- c(1,0,0,0,1,1,1,0,1,1)

#matrix_table <- table(a, b)
#matrix_table




#y_train <- c(1, 1, 0, 1, 0, 0, 0, 1, 1)
#y_pred <- c(1, 0, 0, 1, 0, 1, 0, 1, 1)


st <- read.csv("student.csv")
y <- ifelse(st[,24]=="yes", 1, 0)
x <- st[,c(8, 17, 16, 30)]
x <- data.matrix(x, rownames.force = NA)




















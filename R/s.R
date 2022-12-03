
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



logireg_Plot<-function(x_train,y_train,beta,color="steelblue",line_width=2){
  colnumber<-ncol(x_train)
  rownumber<-nrow(x_train)
  new_x_train<-matrix(1,rownumber,colnumber-1)
  new_x_train<-cbind(x_train[,1],new_x_train)
  new_x_train[,1]<-x_train[,1]
  p_hat<-logistic_reg_predict_dataset(x_train,beta)
  p_hat1<-ifelse(p_hat>0.5, 1, 0)
  datause<-cbind(p_hat1,x_train[,1],p_hat)
  plot(datause[,2],y_train, col=color,xlab="x",ylab="predict")
  lines(sort(datause[,2], decreasing = T), sort(datause[,3], decreasing = T), lwd=line_width)
}
























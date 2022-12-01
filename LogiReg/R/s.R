

y_pred <- logistic_regression(x_train, y_train, 40)


beta_1 <- logistic_regression_trainer(x_train, y_train, 1, lr = .1)
beta_2 <- logistic_regression_trainer(x_train, y_train, 2, lr = .1)
beta_3 <- logistic_regression_trainer(x_train, y_train, 3, lr = .1)

y_pred_1 = logistic_reg_predict_dataset(x_train, beta_1)
y_pred_2 = logistic_reg_predict_dataset(x_train, beta_2)
y_pred_3 = logistic_reg_predict_dataset(x_train, beta_3)

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/LogiReg.R
\name{logistic_regression_trainer_helper}
\alias{logistic_regression_trainer_helper}
\title{Logistic Regression Training Helper Function}
\usage{
logistic_regression_trainer_helper(beta_start, x_train, y_train, lr = 1)
}
\arguments{
\item{beta_start}{Numeric vector of length(number of features) that used to predict target from data}

\item{x_train}{\code{dataframe} or matrix (gets cast to matrix) that is our set of features.}

\item{y_train}{\code{dataframe} value of the target. Gets cast to matrix}

\item{lr}{\code{double} learning rate of logistic regression. If loss goes up instead of down, lower learning rate.
You jumped over local minimum.}
}
\value{
Returns next beta vector Should be a better value than beta_start
}
\description{
Helper function that does 1 step of gradient descent on our Beta vector
}
\author{
Noah Heckenlively
}

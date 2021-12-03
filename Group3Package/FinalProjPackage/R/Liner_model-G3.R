#' @title Function for Beta Estimator
#'
#' @description We create a function of the OLS estimator of the coefficients called coeff_beta.
#' @param Dependent A \code{vector} with the values for the dependent variable (or outcome variable).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @return A \code{numeric}.
#'  \describe{
#'      \item{beta}{Estimated coefficients, Linear Regression Model.}
#' }
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_beta(USArrests$Murder,USArrests$UrbanPop)
coeff_beta <- function (Dependent, Independent) {

  #-------------------------------- preamble -------------------------------------
  # in case user gets something that is not a vector for 'Dependent'
  #  or a matrix for 'Independent', coerce data:
  Dependent <- as.vector(Dependent)
  Independent <- as.matrix(cbind(1,Independent))

  beta <- solve(t(Independent)%*%Independent)%*%t(Independent)%*%Dependent

  return(beta)

}

#' @title Linear Model Function-group3
#'
#' @description We create a function of the OLS estimator of the coefficients called beta_hats.
#' @param Dependent A \code{vector} with the values for the dependent variable (or outcome variable).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @param alpha A \code{numeric} (double) that sets the alpha coefficient to be used. Has to be between 0 and 1.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{ci.beta}{confidence interval for the estimated beta.}
#' }
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_ci(USArrests$Murder,USArrests$UrbanPop)

coeff_ci <- function (Dependent, Independent, alpha = 0.05) {

  beta <- coeff_beta(USArrests$Murder, USArrests$UrbanPop)
  b <- beta
  Dependent <- as.vector(USArrests$Murder)
  Independent <- as.matrix(USArrests$UrbanPop)
  n <- length(Dependent)
  p <- dim(Independent)[2]
  degrees_freedom <- n - p
  Independent <- as.matrix(cbind(1,USArrests$UrbanPop))

  #resid
  resid <- Dependent - Independent%*%as.matrix(b)

  #sigma2.hat
  sigma2.hat <- (1/degrees_freedom) * t(resid) %*% resid

  #var.beta
  var.beta <- as.numeric(sigma2.hat)*solve(t(Independent)%*%Independent)

  #quant
  quant <- 1 - alpha/2

  a <- beta - qnorm(p = quant)
  a <- as.numeric(a)

  c <- sqrt(var.beta)
  c <- t(b)
  c <- as.numeric(b)

  d <- beta + qnorm(p = quant)
  d <- as.matrix(d)
  d <- as.numeric(d)
  #nonconforming arrays?
  ci.beta <- c(a%*%c, d%*%c)

  return(ci.beta)
}

#' @title Function for R Squared
#'
#' @description Function that calculates the R squared value
#' @param Dependent A \code{vector} with the values for the dependent variable (also called outcome).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{r_squared}{gets the r squared value}
#' }
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_rsquared(USArrests$Murder,USArrests$UrbanPop)
coeff_rsquared <- function (Dependent, Independent) {

  #--------------------------------------------------------------------------
  #-------------------------------- preamble --------------------------------

  # in case user gets something that is not a vector for 'Dependent'
  #  or a matrix for 'Independent', coerce data:
  #
  beta <- coeff_beta(Dependent, Independent)
  b <- beta
  Dependent <- as.vector(Dependent)
  Independent <- as.matrix(Independent)
  n <- length(Dependent)
  p <- dim(Independent)[2]
  degrees_freedom <- n - p
  sample_mean <- mean(Dependent)
  Ind <- cbind(1,Independent)
  y_hat <- Ind%*% as.matrix(b)
  fit <- Ind%*%as.matrix(b)

  #--------------------------------------------------------------------------
  # Define base and dummy parameters:

  SSE <- sum((Dependent - y_hat)^2)
  SST <- sum((Dependent - sample_mean)^2)

  r_squared <- 1 - SSE/SST

  return(r_squared)

}



#' @title Function for Mallows Cp-group3
#'
#' @description Function that calculates Mallows Cp
#' @param Dependent A \code{vector} with the values for the dependent variable (also called outcome).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{mallows}{value for Mallows Cp}
#' }
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_mallows_Cp(USArrests$Murder,USArrests$UrbanPop)
coeff_mallows_Cp <- function (Dependent, Independent) {

  #--------------------------------------------------------------------------
  #-------------------------------- preamble --------------------------------

  # in case user gets something that is not a vector for 'Dependent'
  #  or a matrix for 'Independent', coerce data:
  #
  beta <- coeff_beta(Dependent, Independent)
  b <- beta
  Dependent <- as.vector(Dependent)
  Independent <- as.matrix(Independent)
  n <- length(Dependent)
  p <- dim(Independent)[2]
  degrees_freedom <- n - p
  sample_mean <- mean(Dependent)
  Ind <- cbind(1,Independent)
  y_hat <- Ind%*% as.matrix(b)
  fit <- Ind%*%as.matrix(b)

  #--------------------------------------------------------------------------
  # Define base and dummy parameters:

  SSE <- sum((Dependent - y_hat)^2)

  #resid
  resid <- Dependent - Ind%*%as.matrix(b)

  #sigma2.hat
  sigma2.hat <- (1/degrees_freedom) * t(resid) %*% resid

  #--------------------------------------------------------------------------
  # check and warnings:
  #
  if(n != dim(Independent)[1])

    stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?coeff_least_sq")
  #--------------------------------------------------------------------------

  mallows <- SSE +2*p*sigma2.hat

  return(mallows)

}


#' @title Function for F-test -group3
#'
#' @description Function that calculates the F-test
#' @param Dependent A \code{vector} with the values for the dependent variable (also called outcome).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @return A \code{list} containing the following attributes:
#' \describe{
#'      \item{f-stat}{value for f_star}
#'      \item{DFM}{Degrees of freedom}
#'      \item{DFE}{p -1}
#'      \item{f_prob}{f's p-value}
#' }
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_ftest(USArrests$Murder,USArrests$UrbanPop)
coeff_ftest <- function (Dependent, Independent) {

  #--------------------------------------------------------------------------
  #-------------------------------- preamble --------------------------------

  # in case user gets something that is not a vector for 'Dependent'
  #  or a matrix for 'Independent', coerce data:
  #
  beta <- coeff_beta(Dependent, Independent)
  b <- beta
  Dependent <- as.vector(Dependent)
  Independent <- as.matrix(Independent)
  n <- length(Dependent)
  sample_mean <- mean(Dependent)
  Ind <- cbind(1,Independent)
  y_hat <- Ind%*% as.matrix(b)
  fit <- Ind%*%as.matrix(b)
  p <- dim(Ind)[2]
  degrees_freedom <- n - p

  DFM <- p - 1
  DFE <- n - p
  SSM <- sum((y_hat - sample_mean)^2)

  MSM <- SSM/DFM

  SSE <- sum((Dependent - y_hat)^2)

  MSE <- SSE/DFE
  #--------------------------------------------------------------------------

  # check and warnings:
  #
  if(n != dim(Independent)[1])

    stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?coeff_least_sq")
  #--------------------------------------------------------------------------

  f_stat <- MSM/MSE

  f_prob <- pf(f_stat, DFM, DFE, lower.tail = FALSE)
  f_info <- rbind(f_stat, DFM, DFE, f_prob)
  rownames(f_info) <- c("F Stat: ","Lower DF: ","Higher DF:","P-Value: ")
  return(f_info)

}


#' @title Plot function for package-group3
#'
#' @description Function that creates the 3 required plots
#' @param Dependent A \code{vector} with the values for the dependent variable (also called outcome).
#' @param Independent A \code{matrix} with the values for the independent variable.
#' @return A \code{list} containing the following attributes:
#' @author John M. Musah & Alan R. Galloway
#' @importFrom
#' @export
#' @examples
#' data("USArrests")
#' coeff_plot(USArrests$Murder,USArrests$UrbanPop)
coeff_plot <- function (Dependent, Independent) {
  #--------------------------------------------------------------------------
  #-------------------------------- preamble --------------------------------

  # in case user gets something that is not a vector for 'Dependent'
  #  or a matrix for 'Independent', coerce data:
  #
  beta <- coeff_beta(Dependent, Independent)
  Dependent <- as.matrix(Dependent)
  Independent <- as.matrix(cbind(1,Independent))
  #--------------------------------------------------------------------------
  # Define base and dummy parameters:
  #
  n <- length(Dependent)
  p <- dim(Independent)[2]
  degrees_freedom <- n - p
  ci_beta <- rep(NA, p)
  size <- 1000
  residual <- Dependent - Independent%*%as.matrix(beta)
  fit <- Independent%*%beta
  #--------------------------------------------------------------------------
  # check and warnings:
  #
  if(n != dim(Independent)[1])

    stop("Can you check the correspondence between x and y values? It seems we do not have proper pairs! If in doubt, check the documentation using ?coeff_least_sq")
  #--------------------------------------------------------------------------
  #

  par(mfrow=c(1,3),oma = c(1,0,1,0))
  plot(residual~fit, xlab="Residual Values",ylab="Fitted Values",main="Residuals vs Fitted")
  qqnorm(residual)
  HistResVFit=hist(residual, xlab = "Residual Values", ylab="Frequency")
  title(line = -2)

}

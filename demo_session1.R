#################################################################
# Demonstration R Session
# Taken from Venables et al., 
# An Introduction to R, at 
# https://cran.r-project.org/doc/manuals/r-release/R-intro.pdf
################################################################

help.start() # Start the HTML interface toon-line help (using a web browser available at your machine). 

x <- rnorm(50)
y <- rnorm(x) # Generate two pseudo-random normal vectors of x- and y-coordinates.

plot(x, y) # Plot the points in the plane. A graphics window will appear automatically.

ls() # See which R objects are now in the R workspace.

rm(x, y) # Remove objects no longer needed. 

x <- 1:20 # Make x=(1,2,...,20).

w <- 1 + sqrt(x)/2 # A ‘weight’ vector of standard deviations.

dummy <- data.frame(x=x, y= x + rnorm(x)*w) 
dummy # Make a data frame of two columns, x and y, and look at it.

fm <- lm(y ~ x, data=dummy)
summary(fm) # Fit a simple linear regression and look at the analysis. With y to the left of the tilde, we are modelling y dependent on x.

fm1 <- lm(y ~ x, data=dummy, weight=1/w^2)
summary(fm1) # Since we know the standard deviations, we can do a weighted regression.

attach(dummy) # Make the columns in the data frame visible as variables.

lrf <- lowess(x, y) # Make a nonparametric local regression function.

plot(x, y) # Standard point plot.

lines(x, lrf$y) # Add in the local regression.

abline(0, 1, lty=3) # The true regression line: (intercept 0, slope 1).

abline(coef(fm)) # Unweighted regression line.

abline(coef(fm1), col = "red") # Weighted regression line.

detach() # Remove data frame from the search path.

plot(fitted(fm), resid(fm), xlab="Fitted values", ylab="Residuals", main="Residuals vs Fitted") # A standard regression diagnostic plot to check for heteroscedasticity. Can you see it?

qqnorm(resid(fm), main="Residuals Rankit Plot") # A normal scores plot to check for skewness, kurtosis and outliers. (Not very useful here.)

rm(fm, fm1, lrf, x, dummy) # Clean up again.


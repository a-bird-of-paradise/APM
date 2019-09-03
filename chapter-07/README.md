# Chapter 7

# Question 7.1

This is about Support Vector Machines - which are an interesting non linear method which is sensitive to "support vectors" (i.e., "observations") that are close to trend. Data points "close" (within epsilon) of the prediction get no weight in the penalty function which has delta = 1 outwith [-epsilon,epsilon]. The sum of penalties is multiplied by the overall cost (another parameter C), and finally the sum of squared coefficients is added.

So SVMs will prefer models that are close to the prediction and have small parameters, with the relative balance controlled by C. 

The form of parameters is set using a kernel. Linear models have the kernel based on the dot product (e.g., linear regression is basically estimating a vector of coefficients beta such that beta inner producted with predictors x gives you the estimate y). In this instance we look at the radial basis function which is exp(sigma ||x-u||^2); x is a record from the training set and u is the observation to predict. 

We make some fake data based on a noisy sin(x) curve:

![fake](7.1/simPlot.png)


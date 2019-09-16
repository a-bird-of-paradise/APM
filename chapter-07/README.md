# Chapter 7

# Question 7.1

This is about Support Vector Machines - which are an interesting non linear method which is sensitive to "support vectors" (i.e., "observations") that are close to trend. Data points "close" (within epsilon) of the prediction get no weight in the penalty function which has delta = 1 outwith [-epsilon,epsilon]. The sum of penalties is multiplied by the overall cost (another parameter C), and finally the sum of squared coefficients is added.

So SVMs will prefer models that are close to the prediction and have small parameters, with the relative balance controlled by C. 

The form of parameters is set using a kernel. Linear models have the kernel based on the dot product (e.g., linear regression is basically estimating a vector of coefficients beta such that beta inner producted with predictors x gives you the estimate y). In this instance we look at the radial basis function which is exp(sigma ||x-u||^2); x is a record from the training set and u is the observation to predict. 

We make some fake data based on a noisy sin(x) curve:

![fake](7.1/simPlot.png)

Look at how the parameters interplay. Epsilon first:

![epsilon](7.1/svm-e-inv.png)

then cost

![Cost](7.1/svm-c-inv.png)

and then finally sigma in the kernel

![sigma](7.1/svm-s-inv.png)

# Question 7.2 

So we have some fake data and we want to fit some models to it and have a look. Fine. Train models on some data and test them on some other data. 

## k nearest neighbours

![knn](7.2/knn-pred-plot.png)

Doesn't look too great. Look at all that bias. Could be fitting to noise, so (looking ahead and discovering via MARS that only the first five predictors matter) we try fitting to a truncated dataset.

![knn-subset](7.2/knn-subset-pred-plot.png)

Much better. So KNN needs you to trim uninformative predictors. 

## MARS

![mars](7.2/mars-pred-plot.png)

Looks good and reasonably tight too. Get interesting curve shapes for informative predictors:

![mars-v3](7.2/mars-var-3.png)

and flat for uninformative ones: 

![mars-v7](7.2/mars-var-7.png)

useful for trimming predictors to improve KNN!

## SVM

The machines look good too:

![SVM](7.2/svm-pred-plot.png)

## Neural Network

Not great, not terrible. Needed a very long computation too.... 

![AvNNet](7.2/av-nnet-pred-plot.png)

## Summary

|      MAE|     RMSE|  Rsquared|what    |
|--------:|--------:|---------:|:-------|
| 2.586877| 3.214339| 0.6575355|knn     |
| 1.874548| 2.370481| 0.7946572|knn_sub |
| 1.710491| 2.206143| 0.8012192|avNNet  |
| 1.646055| 2.116799| 0.8221731|SVM     |
| 1.413212| 1.798394| 0.8708211|MARS    |

Table says use MARS. Note how KNN got much better when removing uninformative predictors, to the point where it became competitive with SVM and averaged neural networks. 

# 7.3 

Usual routine. Cut data 70/30 training/testing, fit models, apply to testing and see if any good. Also try PCAing data first. Get the following outcomes 

![Comparison](7.3/comparison.png)

PCA is a red herring here. I am astonished at how good neural networks are here - I imagine they are homing in on the absorption lines associated with fats. Similarly with MARS (not surprising, to the extent that MARS is a restricted neural network). KNN bad. SVM OK ish I suppose but would want better.

# 7.4

Trying on permeability data. Honestly all of these are bad. Neural network took days to try out (I need a new machine!) and was basically just as good as SVM which took seconds. 

![Comparision](7.4/comparison.png)

![Log Comparison](7.4/comparison-log.png)

|      MAE|     RMSE|  Rsquared|what   |
|--------:|--------:|---------:|:------|
| 9.798620| 14.55145| 0.2600800|KNN    |
| 8.792173| 13.36471| 0.3614898|SVM    |
| 8.818195| 13.93060| 0.3030681|MARS   |
| 9.461853| 13.32190| 0.3610646|AvNNet |

None of these models beat penalised linear regression!

# 7.5

And now the chemical process. SVM and MARS beat my penalised linear fits on the RMSE measure but not R-squared. Margins are low and MARS is simple and interpretable so go with that. 

![Comparison](7.5/comparison.png)

|       MAE|     RMSE|  Rsquared|what   |
|---------:|--------:|---------:|:------|
| 1.0410348| 1.294035| 0.5133929|KNN    |
| 0.8700358| 1.112553| 0.6429436|SVM    |
| 0.9192446| 1.151877| 0.6144647|AvNNet |
| 0.9642796| 1.128642| 0.6413178|Mars   |

So SVM wins but not by much, really. Looking at predictor importance we find only MARS has meaningful outputs here:

|rowname                |    AvNNet|       KNN|      MARS|       SVM|
|:----------------------|---------:|---------:|---------:|---------:|
|ManufacturingProcess32 | 100.00000| 100.00000| 100.00000| 100.00000|
|ManufacturingProcess09 |  59.60029|  59.60029|  60.37660|  59.60029|
|BiologicalMaterial11   |  43.87980|  43.87980|  33.10388|  43.87980|
|ManufacturingProcess17 |  54.71772|  54.71772|  33.10388|  54.71772|
|BiologicalMaterial01   |  42.51454|  42.51454|   0.00000|  42.51454|
|BiologicalMaterial03   |  66.63877|  66.63877|   0.00000|  66.63877|
|BiologicalMaterial05   |  26.73735|  26.73735|   0.00000|  26.73735|
|BiologicalMaterial06   |  76.05336|  76.05336|   0.00000|  76.05336|
|BiologicalMaterial08   |  32.53688|  32.53688|   0.00000|  32.53688|
|BiologicalMaterial09   |  30.41880|  30.41880|   0.00000|  30.41880|

Comparing to the linear fits, MARS also picks up BiologicalMaterial11 but focusses on the process still. Together they give useful insight!


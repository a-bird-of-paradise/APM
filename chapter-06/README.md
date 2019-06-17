# Chapter 6

# 6.1 

Trying to fit a bunch of models and seeing which one to fit. First thing to do is look at the data;
this has a lot of multicollinearity. Check out this correlation plot: 

![corr plot](tecator-corr.png)

Fitting some models and naively choosing the parameter that makes the lowest RMSE gives the following.

Linear regression: ![LM](lm_plot.png)

PLS: ![PLS](pls_plot.png)

Ridge: ![Ridge](ridge_plot.png)

Lasso: ![Lasso](lasso_plot.png)

Elastic net: ![ENET](enet_plot.png)

Some patterns emerge: Linear regression seems to overfit very easily given
how noisy the fits are. Adding bias markedly reduces variance. Ridge penalties seem to help out a lot. 

Summary of performance is: 

|model |stat |    value| ncomp|   key| fraction| loglambda|
|:-----|:----|--------:|-----:|-----:|--------:|---------:|
|lm    |RMSE | 3.803016|    NA|    NA|       NA|        NA|
|pls   |RMSE | 2.105607|    13|    NA|       NA|        NA|
|ridge |RMSE | 2.142189|    NA| 4e-05|       NA|        NA|
|lasso |RMSE | 2.909513|    NA| 1e-03|       NA|        NA|
|enet  |RMSE | 2.091352|    NA|    NA|     0.16|        -8|

which would indicate choosing elastic net, although pls and ridge also defensible. linear regression performs pooryl, lasso is OK but outclassed by other models. 

### caret approach

We can use caret package to do this better. The caret estimate of RMSE for linear regression is 4.42. 

k-fold cross validated outputs for other models follow: 

PLS: ![PLS](kcv_pls_plot.png)

Lasso: ![Lasso](kcv_lasso_plot.png)

Ridge: ![Ridge](kcv_ridge_plot.png)

Elastic net: ![Enet](kcv_enet_plot.png)

Applying the models to the held out validation set gives the following estimates of performance: 

|id    |     RMSE|  Rsquared|      MAE|
|:-----|--------:|---------:|--------:|
|enet  | 2.457840| 0.9641659| 1.638748|
|lasso | 2.423299| 0.9651549| 1.631413|
|lm    | 3.803016| 0.9209281| 2.561548|
|pls   | 2.959833| 0.9487768| 1.827181|
|ridge | 2.403127| 0.9657451| 1.649992|

From the above looks like ridge regression gives the best performance although could go with elastic net or ridge. PLS and LM have slightly too poor performance at high realisations.

![caret](caret_plot.png) 

Linear regression gives huge parameter estimates: ![linear](coef_plot_lm.png)

Lasso: ![lasso](coef_plot_lasso.png)

Ridge: ![ridge](coef_plot_ridge.png) 

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

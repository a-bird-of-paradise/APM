# Chapter 19

## 19.1 

So initially there is loads of predictors:
![img](19.1/all_plot.png)

Chucking away anything with a correlation of >99.9% reduces this massively: 
![img](19.1/some_plot.png)

Redoing all the RFE stuff gets us this: 
![img](19.1/conv_plot.png)

So get better performance in many instances by throwing away uselessly redundant predictors. But some models are able to use the slightly different predictors meaningfully.
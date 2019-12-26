# Chapter 12

## 12.1 Linear classifiers on hepatic data

Well after a lot of faffing didn't really get any great models out of this. Class balance is a bit uneven but not too horrendous, really: 

![class](12.1/histo-plot.png)

Used accuracy to tune the models. To look at the final judgements I used negative predictive value for none, i.e. correctly predicts "is damaging" with no regard to magnitude of damage. Table of results is: 

|        LR|       LDA|     PLSDA|      NSC|    GLMPen|what    |
|---------:|---------:|---------:|--------:|---------:|:-------|
| 0.6296296| 0.6034483| 0.6415094| 0.626506| 0.6219512|Bio     |
| 0.7407407| 0.6923077| 0.6896552| 0.626506| 0.6265060|Chem    |
| 0.7000000| 0.6444444| 0.7111111| 0.626506| 0.6265060|BioChem |

So the best overall is the logistic regression (really multinomial) model applied to the chemical data only however all of them are kind of rubbish. 

## 12.2 Linear classifers on phat data

So the first question is whether we need to split data into training and test. The answer is yes - how else do you propose to validate the model? Some cunning is needed to deal with such imbalance. I am going to go for a resampling technique. So I will cut the data into training and test, and then upsample the training data so each class has the same number of observations. 

Using accuracy on this fake data to tune the models should be OK. To evaluate the test set a different statistic will probably be better; let's go with "average accuracy" i.e. for each class in the test data calculate the accuracy of that class and then average the accuracies. So if performance on the rare classes is bad the measure will be poor. 

Plugging in the models, tuning them, and plotting the hit/miss matching of predicted values against truth gives us:

![class](12.2/oil_fit_plot.png)

so most of the models are actually OK. PLSDA fails terribly with group G (not surprising - PLS doesn't like single representatives). But nothing to choose between the penalised GLM and the NSC. 

Values of my custom metric are:

|Model  |     Score|
|:------|---------:|
|GLM    | 0.9841270|
|LDA    | 0.9365079|
|LinReg | 0.9206349|
|NSC    | 0.9841270|
|PLSDA  | 0.7936508|

so have to use something else as the tie breaker. Why not `ties.method = 'first'`? GLM it is!

Well, no. We can have a look at the probability calibration plots (so: form ten buckets [0%,10%), [10%, 20%), ..., [90%,100%]; map each training observation into a bucket based on its predicted probability by class; compute obserevd probability versus predicted probability). GLM does poorly here, so go for NSC. 

![class](12.2/glm_probs.png)

![class](12.2/nsc_probs.png)

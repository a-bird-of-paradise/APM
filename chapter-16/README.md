# Chapter 16
## 16.1

Some income data. Lots of it has `NA` for the outcome so strip those out. Not many NAs left so will use KNN etc to fill in gaps (or maybe just have NA as a level?)

First up: how does the data look? so only looking at the training set here. 

![img](16.1/age.png) 

Non-linear relationship with age here. Old and young less likely to have high income than those in the middle / end of their careers

 ![img](16.1/workclass.png) 
 
 Some biases here
 
 ![img](16.1/education.png) 
 
 Higher education level = more income, generally 
 
 ![img](16.1/marital-status.png) 
 
 Marriage helps 
 
 ![img](16.1/occupation.png) 
 
 Some biases in here too
 
 ![img](16.1/relationship.png) 
 
 Gonna be very confounded with relationship 
 
 ![img](16.1/race.png) 
 
 This is America, so it seems being white helps 
 
 ![img](16.1/sex.png) 
 
 This is a patriarchy, so being male helps 
 
 ![img](16.1/hours-per-week.png) 
 
 More hours = more money, but likely to be confounded
 
 ![img](16.1/native-country.png) 
 
 Americans vs everyone else. Need a finer look 
 
 ![img](16.1/cap_gain_bin.png) 
 
 Perhaps not surprisingly, if you have any capital gains at all you probably have a high income.
 
 ![img](16.1/cap_loss_bin.png) 
 
 Capital losses seem to imply some higher income... but not that strong. 
 
 ![img](16.1/origin.png) 
 
 Binned into US, Mexico (biggest other one) and Everyone Else. 
 
 ### Model fits
 
 I fitted some models. One of them (PLS) turned out to have terrible calibration so I used the sigmoid filter to reweight the probabilities. 
 
 First up is a table of Kappa statistics on the test set. Clearly XGBoost wins (but takes forever to train). 
 
|Model          |     Kappa|
|:--------------|---------:|
|PLS            | 0.5309859|
|LDA            | 0.5344823|
|GLM            | 0.5372423|
|PLS-Calibrated | 0.5472139|
|FDA            | 0.5488965|
|XGBoost        | 0.5658689|
 
Interestingly the calibration exercise (which takes seconds) gets another 1.7% of Kappa. 

ROC curves also show this - including a zoom on the interesting bit

![ROC](16.1/roc_curves.png)
![ROC](16.1/roc_curves_zoom.png)

Histograms of class probabilities by model, and also on a log scale:

![Histo](16.1/histo_plots.png)
![Histo-Log](16.1/histo_plots_log.png)

These show that the best models really do a good job of picking out samples with small income, but the distribution for large income is very uniform. So are the models being biased by the initial data volume? 

Finally a calibration plot to prove we have well calibrated probabilities.

![calibration](16.1/calibration_plot.png)
 
 ### Kappa investigation 
 
 So one thing we could do is strike a balance between sensitivity and specificity. In other words we could goal seek the threshold probability between 'small' and 'large' to get a bigger kappa. Probably means accepting a few more false positives in order to get many more true positives until the ratio peaks. 
 
 Easily done. Well kind of. Turns out we can improve kappa by some few % by choosing the threshold appropriately. As won't be surprising from earlier investigations, sliding the threshold from the default 50% to the right a bit get a better result. There is no a priori reason why 50% is the right number for the default, beyond some vaguely Bayesian reasoning, so we can monkey this as we want. (This is actually quite a significant improvement, c. 5%-10% on a relative basis which is really pretty good). 
 
 XGBoost does very well which it should given how long it takes to train. FDA is a lot quicker and not far off! 
 
 |what    | DefaultKappa| BestCutoff| BestKappa|
|:-------|------------:|----------:|---------:|
|XGBoost |    0.5658689|  0.6035340| 0.5902983|
|FDA     |    0.5488965|  0.6743684| 0.5818072|
|PLS     |    0.5309859|  0.5749232| 0.5732821|
|GLM     |    0.5372423|  0.6656615| 0.5669398|
|LDA     |    0.5344823|  0.7325691| 0.5660098|

### Downsampling

OK so the main issue seems to be that large incomes are rare so models are biased. We can work around this by downsampling i.e. chucking away data until large and small are balanced. We actually get marginally better results most of the time - some few % more kappa. Models fit faster too. So if you have huge data sets with serious imbalance issues then try resampling the training data. 

(Didn't redo XGBoost fit as that takes hours; also didn't calibrate probabilites and there are some issues there too it seems!) 

Tuning for Kappa gets e.g. a PLS model that's almost as good as the (un-downsampled!!!) XGBoost model. Default performance is a bit better for most too. 

|what    | DefaultKappa| BestCutoff| BestKappa|
|:-------|------------:|----------:|---------:|
|XGBoost |    0.5658689|  0.6035340| 0.5902983|
|PLS     |    0.5466449|  0.4605979| 0.5843185|
|FDA     |    0.5449138|  0.2816574| 0.5839429|
|GLM     |    0.5581132|  0.3783177| 0.5722413|
|LDA     |    0.5347545|  0.3394593| 0.5624107|

Here are some more plots. Note the calibration issues. 

![ROC](16.1/roc_curves_DS.png)
![ROC](16.1/roc_curves_zoom_DS.png)
![Histo](16.1/histo_plots_DS.png)
![Histo-Log](16.1/histo_plots_log_DS.png)
![calibration](16.1/calibration_plot_DS.png)
 
The models which admit costs take too long to run so not tried those. I can imagine e.g. c5.0 doing well if we penalise the 'right' kind of mistake more than the others. 
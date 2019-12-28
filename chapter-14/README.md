# Chapter 14

## 14.1 Variable importance 

### Common factors

#### Bagging

* Contract Value Band
* A.CI

#### Random Forests 

* Contract Value Band 
* Unsuccess.CI
* Success.CI
* Day
* Month (kinda) 
* Weekday (kinda) 
* Sponsor (kinda) 
* Category Code (kinda) 

#### Boosting

* Contract Value Band 
* Sponsor Code
* Unsuccess.CI
* Success.CI 
* NumCI
* Day
* Weekday (kinda) 

### `PART` prolificism

These were Sponsor Code, Contract Value Band, Unsuccessful CI grants, successful CI grants, and number of chief investigators. Much overlap with boosting, some with random forest, and limited with bagging (only Contract Value). 

## 14.2 Telecom

First Q - do we want split or grouped factors for simple trees? well let us have a look:
![trees](14.2/tree_pred_plot.png)
So the answer is 'it depends'. Next Q: does boosting or bagging help at all? Again, let's have a look:
![trees](14.2/tree_type_pred_plot.png)
Boosting helps quite a bit; bagging not so much. Kappas in the 90s are really good so these are all nice models. 

I fat a bunch of models and got the following stats:

|model           |  Accuracy|     Kappa|
|:---------------|---------:|---------:|
|Boosted.Grouped | 0.9814037| 0.9150992|
|Boosted.Split   | 0.9814037| 0.9150992|
|C50.Split       | 0.9808038| 0.9121846|
|RF.Split        | 0.9796041| 0.9066961|
|Bagged.Grouped  | 0.9790042| 0.9037588|
|Bagged.Split    | 0.9790042| 0.9037588|
|CART.Grouped    | 0.9784043| 0.9012077|
|CART.Split      | 0.9784043| 0.9012077|
|RF.Grouped      | 0.9784043| 0.9008096|
|J48.Grouped     | 0.9772046| 0.8973669|
|C50.Grouped     | 0.9772046| 0.8952990|
|J48.Split       | 0.9760048| 0.8923902|

So they all perform quite similarly. 

Lift plots look a bit strange. Think this is my highly informative predictor getting in the way. First for grouped data:

![lift](14.2/grouped_lift_plot.png)
then for split data
![lift](14.2/split_lift_plot.png)

Also we can have a look at one of the decision trees too:

![tree](14.2/tree_plot.png)
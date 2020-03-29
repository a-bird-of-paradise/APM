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
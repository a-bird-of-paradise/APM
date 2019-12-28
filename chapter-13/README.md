# Chapter 13

## 13.1 Hepatic injuries

Same kind of score as the previous heaptic examination. Moved the pre processing into the caret::train() calls where it belongs.

Couldn't get the QDA and related methods to work. Reckon this is because of the large number of columns vs low sample size. One day I will come back and fix it. 

None of the biological models were any good:

|name     |   FDA|   KNN|      NB|    NN|   SVM|
|:--------|-----:|-----:|-------:|-----:|-----:|
|Accuracy | 0.506| 0.578|  0.4340| 0.554| 0.518|
|Kappa    | 0.119| 0.158| -0.0138| 0.107| 0.000|

Least worst was the KNN thing, altough still nowhere near good enough (would want very high accuracy, esp. on the severe subset). 

Chemical only: the neural network is actually not bad:

|name     |    FDA|   KNN|    NB|    NN|    SVM|
|:--------|------:|-----:|-----:|-----:|------:|
|Accuracy | 0.5300| 0.590| 0.566| 0.699| 0.5540|
|Kappa    | 0.0746| 0.241| 0.146| 0.432| 0.0849|

Still has a poor success rate on the severely damaging substances. Putting them all together results in a degredation in performance:

|name     |   FDA|   KNN|    NB|     NN|    SVM|
|:--------|-----:|-----:|-----:|------:|------:|
|Accuracy | 0.542| 0.494| 0.566| 0.5420| 0.5420|
|Kappa    | 0.148| 0.121| 0.152| 0.0863| 0.0635|

So the chemical-only neural network could maybe be used with low confidence, especially if 'mild' and 'severe' are merged into 'harmful'. I wouldn't want to put it in though. 



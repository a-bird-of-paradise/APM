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

So the chemical-only neural network could maybe be used with low confidence, especially if 'mild' and 'severe' are merged into 'harmful'. Table of stats for the 'none' class of chemical only models is 

|name                 |    FDA|   KNN|     NB|    NN|    SVM|
|:--------------------|------:|-----:|------:|-----:|------:|
|Balanced Accuracy    | 0.5650| 0.614| 0.5390| 0.771| 0.5480|
|Detection Prevalence | 0.1450| 0.373| 0.1450| 0.337| 0.0361|
|Detection Rate       | 0.0843| 0.193| 0.0723| 0.253| 0.0361|
|F1                   | 0.3260| 0.516| 0.2790| 0.712| 0.1760|
|Neg Pred Value       | 0.6620| 0.712| 0.6480| 0.818| 0.6500|
|Pos Pred Value       | 0.5830| 0.516| 0.5000| 0.750| 1.0000|
|Precision            | 0.5830| 0.516| 0.5000| 0.750| 1.0000|
|Prevalence           | 0.3730| 0.373| 0.3730| 0.373| 0.3730|
|Recall               | 0.2260| 0.516| 0.1940| 0.677| 0.0968|
|Sensitivity          | 0.2260| 0.516| 0.1940| 0.677| 0.0968|
|Specificity          | 0.9040| 0.712| 0.8850| 0.865| 1.0000|

So the Chem NN is good at spotting any harm (81.8%) and almost as good at spotting no harm (75.0%). Not quite high enough for me to want to put it in though. Relative importance of variables to this model are:

|rowname |  None| Mild| Severe|
|:-------|-----:|----:|------:|
|C1      | 100.0| 73.3|  100.0|
|C132    |  99.8| 81.8|   99.8|
|C123    |  90.0| 89.2|   90.0|
|C15     |  88.8| 87.8|   88.8|
|C20     |  87.1| 57.7|   87.1|
|C133    |  86.3| 50.2|   86.3|
|C19     |  84.3| 83.9|   84.3|
|C18     |  77.5| 79.7|   79.7|
|C169    |  75.3| 78.0|   78.0|
|C17     |  76.9| 60.3|   76.9|
|C144    |  71.4| 75.6|   75.6|
|C120    |  75.1| 48.6|   75.1|
|C23     |  64.6| 74.9|   74.9|
|C145    |  73.6| 74.6|   74.6|
|C24     |  73.2| 60.9|   73.2|
|C160    |  43.8| 70.4|   70.4|
|C81     |  69.7| 55.6|   69.7|
|C175    |  63.8| 69.3|   69.3|
|C35     |  68.7| 39.1|   68.7|
|C138    |  45.4| 68.4|   68.4|





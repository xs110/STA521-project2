#S TA521 - Project 2: The Prediction of Cloud Detection
##Xinyi Sheng & Yucan Zeng

In this project, we save all R scripts in the R/ folder and all graphs in the graph/ folder. For convenience, we store all models and data results in cache/ folder. 1. Data Summary and EDA.R is the code for part 1. 2. Data Splitting. R and 2(d)CVmaster. R is the code for part 2. so on and so forth. 
We set at least one random seed to facilitate the generation of the exact final paper each time the script is run.

In the 2. Data Splitting. R, we implement the following data splitting methods: one is horizontal block spit, and the other one is k-means split which is realized by split_data() and kmeans_splitting() functions based on training, validation, and test datasets. 

In the 2(d)CVmaster.R, we define a function that takes a generic classifier (Logistic Regression, Random Forest, etc.), method of splitting the data(Horizontal Block, Kmeans), training features, tuning parameters, training labels, number of folds K and a loss function (Accuracy, Precision, etc.) as inputs and outputs the K-fold CV loss on the training set. To apply this function, We implement 5 classifiers on two data splitting methods - Horizontal Block Split and k-means Split. 

In the 3.modeling.R, we apply the CVmaster function from 2(d) to implement 5 classifiers. When the model fit finishes, we use the ROC curve to evaluate the accuracy and get the optimal threshold. Also, we produce the metrics that are Precision, Recall, F1 score, test AUC, and ROC cutoff value (Threshold) to show model performance.

In the 4.diagnostic.R, we discuss the model convergence, stability, and robustness by feature importance plot, confusion matrix, error-complexity plot, and perturb plot. Then we also use boxplots, and misclassification prediction maps to discuss the pattern of misclassification. Meanwhile, we put forward some suggestions to improve and optimize the model and data split method.

We produce the paper project2_cloud_data.pdf knitted by project2_cloud_data.rmd, in which we use latex to write the whole paper. All graphs used in the .rmd file are stored in the graph/ folder.

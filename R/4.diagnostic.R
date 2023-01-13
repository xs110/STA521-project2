library(tidyverse)
library(caret)
library(magrittr)
library(ggpubr)
metric_rf_block = read_rds("cache/metric_rf_block.rds")
metric_rf_horizon = read_rds("cache/metric_rf_horizon.rds")
metric_rf_kmeans = read_rds("cache/metric_rf_kmeans.rds")

#models
model_rf_block = read_rds("cache/model_rf_block.rds")
model_rf_horizon = read_rds("cache/model_rf_horizon.rds")
model_rf_kmeans = read_rds("cache/model_rf_kmeans.rds")

#feature importance of random forest
#feature importance
rfimp_block = varImp(model_rf_block, scale = FALSE)
rfimp_horizon = varImp(model_rf_horizon, scale = FALSE)
rfimp_kmeans = varImp(model_rf_kmeans, scale = FALSE)

#block
rfimp_block <- data.frame(cbind(variable = rownames(rfimp_block$importance), score = rfimp_block$importance[,1]))
rfimp_block$score <- as.double(rfimp_block$score)

rfimp_block[order(rfimp_block$score,decreasing = TRUE),]
rfimp_block$set = "Block"
#horizon
rfimp_horizon <- data.frame(cbind(variable = rownames(rfimp_horizon$importance), score = rfimp_horizon$importance[,1]))
rfimp_horizon$score <- as.double(rfimp_horizon$score)

rfimp_horizon[order(rfimp_horizon$score,decreasing = TRUE),]
rfimp_horizon$set = "Horizon"
#kmeans
rfimp_kmeans <- data.frame(cbind(variable = rownames(rfimp_kmeans$importance), score = rfimp_kmeans$importance[,1]))
rfimp_kmeans$score <- as.double(rfimp_kmeans$score)

rfimp_kmeans[order(rfimp_kmeans$score,decreasing = TRUE),]
rfimp_kmeans$set = "Kmeans"

df_roc_imp = bind_rows(rfimp_horizon,rfimp_kmeans)


varimp = ggplot(df_roc_imp, aes(x=reorder(variable, score), y=score,fill=set)) +
  #  geom_point() +
  geom_bar(stat = 'identity', position = position_dodge(0.5),width = 0.5)+
  scale_fill_manual(values=c('black','lightgray','darkgray'))+
  #geom_segment(aes(x=variable,xend=variable,y=0,yend=score,color = set,alpha = 0.7,linewidth = 5)) +
  ylab("Variable Importance") +
  xlab("Variable Name") +
  coord_flip()+  theme_bw()
ggsave(
  "graphs/rf_varimp_image.png",
  varimp,
  width = 15,
  height = 12,
  units = "cm"
)

#confusion matrix------------------------------------------------------------
#read test data
test = read_rds("cache/02_test.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
colnames(test) <- make.names(colnames(test))
#block
y_pred_prob = predict(model_rf_block, newdata = test, type ="prob")
roc_obj=roc(test[["expert_label"]] ~ y_pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
y_pred <- as.factor(ifelse(y_pred_prob[,"1"] > thres, "1", "-1"))

test_set = data.frame(obs = as.factor(c(test[, "expert_label"])), pred =as.factor( c(y_pred)))
block_confusion = confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")
#horizon
y_pred_prob = predict(model_rf_horizon, newdata = test, type ="prob")
roc_obj=roc(test[["expert_label"]] ~ y_pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_obj, "best", "threshold")['threshold'])
y_pred <- as.factor(ifelse(y_pred_prob[,"1"] > thres, "1", "-1"))

test_set = data.frame(obs = as.factor(c(test[, "expert_label"])), pred =as.factor( c(y_pred)))
horizon_confusion = confusionMatrix(data = test_set$pred, reference = test_set$obs, mode = "prec_recall")

#prediction graph------------------------------------------------------------------
train = read_rds("cache/image.rds")
train_set = train%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-SD)%>%
  mutate(across(expert_label, factor))
colnames(train_set) <- make.names(colnames(train_set))


#kmeans
y_pred_kmeans = predict(model_rf_kmeans, newdata = train_set, type ="prob")
roc_kmeans=roc(train_set[["expert_label"]] ~ y_pred_kmeans[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_kmeans, "best", "threshold")['threshold'])
y_pred_b <- as.factor(ifelse(y_pred_kmeans[,"1"] > thres, "1", "-1"))

#preparation for graph
train$pred_label = y_pred_b
rf_pixel_kmeans = train%>%
  select(expert_label,pred_label,x_coordinate,y_coordinate,class)%>%
  mutate(pred_label = case_when(
    pred_label == expert_label ~ expert_label,
    expert_label == 0 ~ 0 ,
    TRUE ~ 2
  ))%>%
  mutate(
    across(
      pred_label,
      ~ case_when(
        .x == 1 ~ "Cloud",
        .x == -1 ~ "No cloud",
        .x == 0 ~ "Unlabelled",
        .x == 2 ~ "Misclassified"
      )
    )
  )%>%ggplot(aes(x = x_coordinate, y = y_coordinate, color = factor(pred_label))) +
  geom_point() +
  scale_color_manual(values = c( "Cloud" = "#F4EDCA","Unlabelled" = "#C4961A","No cloud" = "#FFDB6D",
                                 "Misclassified" = "red"),
                     name = "pred_label") +
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_bw()  + facet_grid(~class)
#save image
ggsave(
  "graphs/04_rf_pixel_kmeans.png",
  rf_pixel_kmeans,
  width = 18,
  height = 7,
  units = "cm"
)


box_data_kmeans = train%>%
  select(expert_label,pred_label,x_coordinate,y_coordinate,class)%>%
  mutate(pred_label = case_when(
    pred_label == expert_label ~ expert_label,
    expert_label == 0 ~ 0 ,
    TRUE ~ 2
  ))%>%mutate(prediction = case_when(
    pred_label == 2 ~ "misclassification",
    TRUE ~ "correct classification"
  ))%>%bind_cols(train%>%select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class))%>%
  # bind_cols(train%>%mutate('log(SD)' = log(SD))%>%
  #                  select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class,-SD))%>%
  select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class)

p1 = ggplot( box_data_kmeans, aes(prediction, NDAI))+
  geom_boxplot()
p2 = ggplot( box_data_kmeans, aes(prediction, SD))+
  geom_boxplot()
p3 = ggplot( box_data_kmeans, aes(prediction, CORR))+
  geom_boxplot()
p4 = ggplot( box_data_kmeans, aes(prediction, Rad_CF))+
  geom_boxplot()
p5 = ggplot( box_data_kmeans, aes(prediction, Rad_BF))+
  geom_boxplot()
p6 = ggplot( box_data_kmeans, aes(prediction, Rad_AF))+
  geom_boxplot()
p7 = ggplot( box_data_kmeans, aes(prediction, Rad_AN))+
  geom_boxplot()
p8 = ggplot( box_data_kmeans, aes(prediction, Rad_DF))+
  geom_boxplot()

boxplot_kmeans = ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4,nrow=2)

ggsave(
  "graphs/04_boxplot_kmeans.png",
  boxplot_kmeans,
  width = 30,
  height = 15,
  units = "cm"
)


#horizon
y_pred_horizon = predict(model_rf_horizon, newdata = train_set, type ="prob")
roc_horizon=roc(train_set[["expert_label"]] ~ y_pred_horizon[,"1"], smoothed=TRUE, plot=FALSE)
# Loss computation
thres = as.numeric(coords(roc_horizon, "best", "threshold")['threshold'])
y_pred_h <- as.factor(ifelse(y_pred_horizon[,"1"] > thres, "1", "-1"))

#preparation for graph
train$pred_label = y_pred_h
rf_pixel_horizon = train%>%
  select(expert_label,pred_label,x_coordinate,y_coordinate,class)%>%
  mutate(pred_label = case_when(
    pred_label == expert_label ~ expert_label,
    expert_label == 0 ~ 0 ,
    TRUE ~ 2
  ))%>%
  mutate(
    across(
      pred_label,
      ~ case_when(
        .x == 1 ~ "Cloud",
        .x == -1 ~ "No cloud",
        .x == 0 ~ "Unlabelled",
        .x == 2 ~ "Misclassified"
      )
    )
  )%>%ggplot(aes(x = x_coordinate, y = y_coordinate, color = factor(pred_label))) +
  geom_point() +
  scale_color_manual(values = c( "Cloud" = "#F4EDCA","Unlabelled" = "#C4961A","No cloud" = "#FFDB6D",
                                 "Misclassified" = "red"),
                     name = "pred_label") +
  labs(x = "X Coordinate", y = "Y Coordinate") +
  theme_bw()  + facet_grid(~class)
#save image
ggsave(
  "graphs/04_rf_pixel_horizon.png",
  rf_pixel_horizon,
  width = 18,
  height = 7,
  units = "cm"
)

box_data_horizon = train%>%
  select(expert_label,pred_label,x_coordinate,y_coordinate,class)%>%
  mutate(pred_label = case_when(
    pred_label == expert_label ~ expert_label,
    expert_label == 0 ~ 0 ,
    TRUE ~ 2
  ))%>%mutate(prediction = case_when(
    pred_label == 2 ~ "misclassification",
    TRUE ~ "correct classification"
  ))%>%bind_cols(train%>%select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class))%>%
  # bind_cols(train%>%mutate('log(SD)' = log(SD))%>%
  #                  select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class,-SD))%>%
  select(-expert_label,-pred_label,-x_coordinate,-y_coordinate,-class)

p1 = ggplot( box_data_horizon, aes(prediction, NDAI))+
  geom_boxplot()
p2 = ggplot( box_data_horizon, aes(prediction, SD))+
  geom_boxplot()
p3 = ggplot( box_data_horizon, aes(prediction, CORR))+
  geom_boxplot()
p4 = ggplot( box_data_horizon, aes(prediction, Rad_CF))+
  geom_boxplot()
p5 = ggplot( box_data_horizon, aes(prediction, Rad_BF))+
  geom_boxplot()
p6 = ggplot( box_data_horizon, aes(prediction, Rad_AF))+
  geom_boxplot()
p7 = ggplot( box_data_horizon, aes(prediction, Rad_AN))+
  geom_boxplot()
p8 = ggplot( box_data_horizon, aes(prediction, Rad_DF))+
  geom_boxplot()

boxplot_horizon = ggarrange(p1,p2,p3,p4,p5,p6,p7,p8,ncol=4,nrow=2)

ggsave(
  "graphs/04_boxplot_horizon.png",
  boxplot_horizon,
  width = 30,
  height = 15,
  units = "cm"
)
#learning curve------------------------------------------------------------------
#read train data
train = read_rds("cache/02_train.rds")%>%
  mutate('log_SD' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
levels(train$expert_label) = c("Class1","Class2")

rd_data <- learning_curve_dat(dat = train,
                               outcome = "expert_label",
                               test_prop = 1/4,
                               ## `train` arguments:
                               method = "rf",
                               metric = "ROC",
                               trControl = trainControl(classProbs = TRUE,
                                                        summaryFunction = twoClassSummary),
                               tuneLength = 1,
                               tuneGrid = data.frame(mtry = 8)
                               )

rf_learningcurve_block = ggplot(rd_data, aes(x = Training_Size, y = ROC, color = Data)) +
  geom_smooth(method = loess, span = .8)
write_rds(rd_data,"cache/04_rd_data.rds")

#save image
ggsave(
  "graphs/04_rf_learningcurve_block.png",
  rf_learningcurve_block,
  width = 18,
  height = 7,
  units = "cm"
)

rd_data_h = read_rds('cache/04_rd_data_h.rds')
rd_data_h <- learning_curve_dat(dat = train,
                              outcome = "expert_label",
                              test_prop = 1/4,
                              ## `train` arguments:
                              method = "rf",
                              metric = "ROC",
                              trControl = trainControl(classProbs = TRUE,
                                                       summaryFunction = twoClassSummary),
                              tuneLength = 1,
                              tuneGrid = data.frame(mtry = 6)
)
write_rds(rd_data_h,"cache/004_rd_data_h.rds")

rf_learningcurve_horizon = ggplot(rd_data_h, aes(x = Training_Size, y = ROC, color = Data)) +
  geom_smooth(method = loess, span = .8)

#save image
ggsave(
  "graphs/04_rf_learningcurve_horizon.png",
  rf_learningcurve_horizon,
  width = 18,
  height = 7,
  units = "cm"
)

#test robustness------------------------------------------------------------------
train = read_rds("cache/02_train.rds")%>%
  mutate('log_SD' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)

test = read_rds("cache/02_test.rds")%>%mutate('log_SD' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)
 # mutate(across(expert_label, factor))

p = sort(seq(0, 0.5, 0.05), decreasing = T)
p
p_acc_kmeans_rf = c()
p_acc_horizon_rf = c()
p_acc_kmeans_lr = c()
p_acc_horizon_lr = c()
for (i in 1:length(p)) {
  p_i = p[i]
  print(p_i)
  size_perturb = as.integer(nrow(train)*p_i)
  perturb_index_kmeans = sample(1:nrow(train), size_perturb)
  perturbed_kmeans = train
  perturbed_kmeans[perturb_index_kmeans, "expert_label"] =
    ifelse(train[perturb_index_kmeans,"expert_label"] == 1, -1, 1)
  perturbed_kmeans = perturbed_kmeans%>%mutate(across(expert_label, factor))
  ##random forest
  # kmeans
  kmeansFinal_rf = train(
    form = as.factor(expert_label) ~.,
    data = perturbed_kmeans,
    method = "rf",
    tuneGrid=data.frame(mtry=1),
    tuneLength = 1,
    trControl = trainControl(method="none")
  )
  # vert
  horizonFinal_rf = train(
    form = as.factor(expert_label)~.,
    data = perturbed_kmeans,
    method = "rf",
    tuneGrid=data.frame(mtry=6),
    tuneLength = 1,
    trControl = trainControl(method="none")
  )
  #logistic rregression
  # kmeans
  tune_kmeans = data.frame(alpha=1,lambda=0.008)
  kmeansFinal_lr = train(
    form = as.factor(expert_label) ~.,
    data = perturbed_kmeans,
    method = "glmnet",
    family = "binomial",
    preProcess = c("center","scale"),
    tuneLength = 1,
    tuneGrid = tune_kmeans,
    trControl = trainControl(method="none")
  )
  # horizon
  tune_horizon = data.frame(alpha=1,lambda=0.009)
  horizonFinal_lr = train(
    form = as.factor(expert_label) ~.,
    data = perturbed_kmeans,
    method = "glmnet",
    family = "binomial",
    preProcess = c("center","scale"),
    tuneLength = 1,
    tuneGrid = tune_horizon,
    trControl = trainControl(method="none")
  )

  predict_accuracy = function(data,model){
    #kmeans random forest
    pred_prob = predict(model, newdata=data, type="prob")
    roc_pred=roc(data[["expert_label"]] ~ pred_prob[,"1"], smoothed=TRUE, plot=FALSE)
    # Loss computation
    thres = as.numeric(unlist(coords(roc_pred, "best", "threshold")['threshold']))
    y_pred <- as.character(ifelse(pred_prob[,"1"] > thres[1], "1", "-1"))
    testData = data.frame(obs = as.factor(c(data[, "expert_label"])), pred =as.factor( c(y_pred)))
    #accuracy,precision,recall,f1,auc
    accuracy = confusionMatrix(data = testData$pred, reference = testData$obs, mode = "prec_recall")$overall['Accuracy'][[1]]
    return(accuracy)
  }

  acc_rf_kmeans = predict_accuracy(test,kmeansFinal_rf)
  acc_rf_horizon = predict_accuracy(test,horizonFinal_rf)
  acc_lr_kmeans = predict_accuracy(test,kmeansFinal_lr)
  acc_lr_horizon = predict_accuracy(test,horizonFinal_lr)

  print(acc_rf_kmeans)
  print(acc_rf_horizon)
  print(acc_lr_kmeans)
  print(acc_lr_horizon)

  p_acc_kmeans_rf = c(p_acc_kmeans_rf, acc_rf_kmeans)
  p_acc_horizon_rf = c(p_acc_horizon_rf, acc_rf_horizon)
  p_acc_kmeans_lr = c(p_acc_kmeans_lr, acc_lr_kmeans)
  p_acc_horizon_lr = c(p_acc_horizon_lr, acc_lr_horizon)
}
df = data.frame("Perturb" = rep(p,2), "Accuracy_RF" = c(p_acc_kmeans_rf, p_acc_horizon_rf),
                "Accuracy_Logi" = c(p_acc_kmeans_lr, p_acc_horizon_lr),
                "Split" = c(rep("kmeans", length(p)), rep("Horizon", length(p))),
                "Model" = c(rep("Random Forest", 2*length(p)), rep("Logistic Regression", 2*length(p))))
stability = ggplot(df, aes(x=Perturb, color=Split))+
  geom_point(aes(y=Accuracy_RF),alpha = 0.5) +
  geom_line(aes(y=Accuracy_RF, linetype="Random Forest"),alpha = 0.5) +
  geom_point(aes(y=Accuracy_Logi),alpha = 0.5) +
  geom_line(aes(y=Accuracy_Logi, linetype="Logistic Regression"),alpha = 0.5) +
  xlab("Perturbed Proportion") + ylab("Test Accuracy") +
  scale_linetype_manual(name="Model",
                        values=c("Random Forest"=1,"Logistic Regression"=2))
#save image
ggsave(
  "graphs/04_stability.png",
  stability,
  width = 18,
  height = 7,
  units = "cm"
)

#trade off------------------------------------------------------------------
#read training data
train_horizon = read_rds("cache/02_train_block_1_10.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
train_kmeans = read_rds("cache/02_train_kmeans.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
colnames(train_horizon) <- make.names(colnames(train_horizon))
colnames(train_kmeans) <- make.names(colnames(train_kmeans))

#test data
test = read_rds("cache/02_test.rds")%>%
  mutate('log(SD)' = log(SD))%>%
  select(-x_coordinate,-y_coordinate,-class,-id,-SD)%>%
  mutate(across(expert_label, factor))
colnames(test) <- make.names(colnames(test))


error_rf = function(train,test,ntree, mtry,thres){

  train = train_horizon
  ntree = 1
  mtry = 6
  thres = thress_rf$horizon
  rf_ntree = train(expert_label ~ ., data = train,
                           method = "rf",
                           ntree = ntree,
                           trControl = trainControl(method = "none"),
                           tuneGrid = data.frame(mtry = mtry))

  pred_prob_train = predict(rf_ntree, newdata=train, type="prob")
  pred_prob_test = predict(rf_ntree, newdata=test, type="prob")
  y_pred_train <- as.character(ifelse(pred_prob_train[,"1"] > thres[1], "1", "-1"))
  y_pred_test <- as.character(ifelse(pred_prob_test[,"1"] > thres[1], "1", "-1"))
  train_pred = data.frame(obs = as.factor(c(train[, "expert_label"])), pred =as.factor( c(y_pred_train)))
  test_pred = data.frame(obs = as.factor(c(test[, "expert_label"])), pred =as.factor( c(y_pred_test)))
  erro_train = 1 - confusionMatrix(data = train_pred$pred, reference = train_pred$obs, mode = "prec_recall")$overall[[1]]
  erro_test = 1 - confusionMatrix(data = test_pred$pred, reference = test_pred$obs, mode = "prec_recall")$overall[[1]]
  return(list("erro_train" = erro_train,
              "erro_test" = erro_test))
}
##convergence

ntree = seq(1, 2001, by=50)
error_horizon_train = c()
error_horizon_test = c()
error_kmeans_train = c()
error_kmeans_test = c()

for(params in ntree){
  print(params)
  error_horizon_train = c(error_horizon_train, error_rf(train_horizon,test,params, mtry = 3,thres = thress_rf$horizon)$erro_train)
  error_horizon_test = c(error_horizon_test, error_rf(train_horizon,test,params, mtry = 3,thres = thress_rf$horizon)$erro_test)
  error_kmeans_train = c(error_kmeans_train, error_rf(train_kmeans,test,params, mtry = 2,thres = thress_rf$kmeans)$erro_train)
  error_kmeans_test = c(error_kmeans_test, error_rf(train_kmeans,test,params, mtry = 2,thres = thress_rf$kmeans)$erro_test)
 print(sprintf("finish %d tree", params))

}
df_error = data.frame("complexity" = ntree, "error_test" = c(error_kmeans_test,error_horizon_test),
                "error_train" = c(error_kmeans_train,error_horizon_train ),
                "Split" = c(rep("kmeans", length(ntree)), rep("Horizon", length(ntree))),
                "Set" = c(rep("test", 2*length(ntree)), rep("train", 2*length(ntree))))
error_test =
  ggplot(df_error, aes(x=complexity, color=Split))+
  geom_smooth(aes(y=error_test),alpha = 0.5) +
  geom_smooth(aes(y=error_test),alpha = 0.5) +
   xlab("Complexity") + ylab("Test Error")
error_train =
  ggplot(df_error, aes(x=complexity, color=Split))+
  geom_smooth(aes(y=error_train),alpha = 0.5) +
  geom_smooth(aes(y=error_train),alpha = 0.5) +
  xlab("Complexity") + ylab("Train Error")

ggsave(
  "graphs/04_error_test.png",
  error_test,
  width = 18,
  height = 14,
  units = "cm"
)

ggsave(
  "graphs/04_error_train.png",
  error_train,
  width = 18,
  height = 14,
  units = "cm"
)

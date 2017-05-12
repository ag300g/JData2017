# 交叉验证评价函数
recall <- function(preds, dtrain) {
    labels <- getinfo(dtrain, "label")
    labels_pred <- preds > 0.5
    recall <- sum(labels_pred * labels)/sum(labels)
    return(list(metric = "recall", value = recall))
}

# 加载数据
gbdt_data_preparation <- function() {
    
    online_train <- fread('../data/online_train_20170512085041.txt', na.strings = c('NULL'))
    online_prediction_x <- fread('../data/online_prediction_20170512085121.txt')
    
    # 补充特征
    supp <- fread('../data/dev_ftp_user_feacture_zhjs_01_20170511134601.csv', na.strings = c('NULL'))
    supp$flag <- NULL
    
    online_train <- left_join(online_train, supp, by='user_id')
    online_prediction_x <- left_join(online_prediction_x, supp, by='user_id')
    
    label <- online_train$label
    user_id <- online_train$user_id
    
    online_train$user_id <- NULL
    online_train$sku_id <- NULL
    online_train$label <- NULL
    online_train$stat_day_cnt <- NULL
    
    result <- list(user_id, label, online_train, online_prediction_x)
    
    return(result)
    
}

# GBDT训练
gbdt_train <- function(online_train, label) {
    
    # 构建均衡训练集
    train_sample_true <- which(label == 1)
    train_sample_false <- which(label == 0)
    balanced_sample_size <- ceiling(length(train_sample_true) * 2)
    true_index <- train_sample_true
    false_index <- train_sample_false[sample(1:length(train_sample_false), balanced_sample_size)]
    balanced_index <- c(true_index, false_index)
    
    dtrain_balanced <- xgb.DMatrix(data = as.matrix(1.0*online_train[balanced_index, ]), 
                                   label=label[balanced_index])
    
    # 设置参数
    params <- list(objective="binary:logistic", nthread=5, max_depth=10, eta=0.02, subsample=0.6)
    
    # 交叉验证
    train.xgboost.cv <- xgb.cv(data = dtrain_balanced, params=params, nrounds=300, nfold=5)
    
    # # 交叉验证（recall）
    # train.xgboost.cv <- xgb.cv(data = dtrain_balanced, params=params, nrounds=400, nfold=5, feval=recall)
    
    # 训练模型（均衡样本）
    train.xgboost.balanced <- xgb.train(data=dtrain_balanced, params=params, nrounds=300)
    
    # 训练误差分析
    dtrain <- xgb.DMatrix(data = as.matrix(1.0*online_train), label=label)
    pred <- predict(model, dtrain)
    tmp <- data.frame(label, pred)
    tmp <- tmp %>% arrange(desc(pred))
    
    precision <- sum(tmp$label[1:12000]) / 12000
    recall <- sum(tmp$label[1:12000]) / sum(tmp$label)
    f11 <- 6*recall*precision / (5*recall + precision)
    
    return(train.xgboost.balanced)
    
}
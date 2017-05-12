library(data.table)
library(dplyr)
library(xgboost)

source('./utils.R')








# 提交预测
user_id <- online_prediction_x$user_id
online_prediction_x$user_id <- NULL
online_prediction_x$stat_day_cnt <- NULL

dtrain_online <- xgb.DMatrix(data = as.matrix(1.0*online_prediction_x))
pred <- predict(model, dtrain_online)

# 根据概率排序
result <- data.frame(user_id, pred)
result <- result %>% arrange(desc(pred))

# 取前12000个作为提交结果
result <- result[1:12000, ]

# 匹配SKU
load('E:/JData2017/action_all.RData')
product <- fread('E:/JData2017/JData_Product.csv')

sku_prediction <- action_all %>% filter(user_id %in% result$user_id & sku_id %in% product$sku_id)

rm(list = c('action_all')); gc()

sku_prediction <- sku_prediction %>% dplyr::arrange(user_id, desc(time)) %>% group_by(user_id) %>% dplyr::mutate(id=row_number()) %>% filter(id == 1)

result <- left_join(result, sku_prediction, by='user_id')
idx <- which(is.na(result$sku_id))
result$sku_id[idx] <- 162344

output <- result %>% select(user_id, sku_id)
write.csv(output, file='output.csv', row.names=FALSE, quote=FALSE)

# # 特征分析
# imp <- xgb.importance(model=model)
# feature_rank <- as.numeric(imp$Feature) + 1
# print(colnames(online_prediction_x)[feature_rank])
# 
# 特征选择
num_of_features <- 30
imp <- xgb.importance(model=model)
feature_rank <- as.numeric(imp$Feature) + 1
feature_select_index <- feature_rank[1:num_of_features]


mydata <- fread('D:/sku_select_20170512115540.csv', na.strings=c("NULL"))
mydata <- mydata[!is.na(mydata$sku_id), ]
mydata <- mydata %>% group_by(user_id) %>% summarise(sku_id=max(sku_id))
submit <- fread('./output1224774.csv')
result <- left_join(submit, mydata, by='user_id')

idx <- which(is.na(result$sku_id.y))
result$sku_id.y[idx] <- result$sku_id.x[idx]
result$sku_id.x <- NULL
colnames(result) <- c('user_id', 'sku_id')


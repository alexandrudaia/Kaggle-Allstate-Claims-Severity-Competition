train=read.csv('/media/machine_learning/A80C461E0C45E7C01/all/train.csv',heade=T,sep=',')
test=read.csv('/media/machine_learning/A80C461E0C45E7C01/all/test.csv',heade=T,sep=',')
library(xgboost)
ID = 'id'
TARGET = 'loss'
SEED = 0
SHIFT = 200
y_train = log(train[,TARGET] + SHIFT)

train[, c(ID, TARGET) = NULL]
test[, c(ID) = NULL]

ntrain = nrow(train)

features=names(train)[c(2:131,133:135)]

x_train=train[,features]
x_test=test[,features]
features_to_drop <- c("cat67","cat21","cat60","cat65", "cat32", "cat30",
                      "cat24", "cat74", "cat85", "cat17", "cat14", "cat18",
                      "cat59", "cat22", "cat63", "cat56", "cat58", "cat55",
                      "cat33", "cat34", "cat46", "cat47", "cat48", "cat68",
                      "cat35", "cat20", "cat69", "cat70", "cat15", "cat62")
x_train=x_train[,-which(names(x_train)%in%features_to_drop)]
x_test=x_test[,-which(names(x_test)%in%features_to_drop)]
rounds=10
for( i in  c(1:rounds))
{
print(i)
h<-sample(nrow(train),20000)


dtrain = xgb.DMatrix(as.matrix(x_train[-h,]), label=y_train[-h])
dtest = xgb.DMatrix(as.matrix(x_test[-h,]))

xgb_params = list(
  seed = 0,
  colsample_bytree = 0.5,
  subsample = 0.8,
  eta = 0.01,
  objective = 'reg:linear',
  max_depth = 14,
  alpha = 1,
  gamma = 2,
  min_child_weight = 1,
  base_score = 7.76
)

xg_eval_mae <- function (yhat, dtrain) {
  y = getinfo(dtrain, "label")
  err= mae(exp(y),exp(yhat) )
  return (list(metric = "error", value = err))
}

SEED = 0
 
gbdt = xgb.train(xgb_params, dtrain, nrounds = 545,verbose= 50 )

prediction = exp(predict(gbdt,xgb.DMatrix(as.matrix(x_test)))) - SHIFT
assign(paste0("pred",i),prediction)


                      


}
m=matrix(,nrow=length(pred1),ncol=rounds)
for( i in c(1:rounds))
{
  m[,i]=eval(parse(text=paste0("pred",i)))
}
loss=apply(m,1,mean)

submission = read.csv('/media/machine_learning/A80C461E0C45E7C01/all/sample_submission.csv', header=T,sep=",")
submission$loss=loss




write.csv(submission,'/media/machine_learning/A80C461E0C45E7C01/all/xgb500.csv',row.names = FALSE)

 


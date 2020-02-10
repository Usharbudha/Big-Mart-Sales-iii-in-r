#REPLACING MISSING VALUES
apply(is.na(data), 2, which) #Item_weight and Outlet_size has na

#REPLACING NA OF ITEM WEIGHT WITH MEAN
data$Item_Weight[is.na(data$Item_Weight)]=mean(data$Item_Weight, na.rm = TRUE)

#REPLACING NA OF OUTLET SIZE WITH MOST OCCURED CATEGORICAL VARIABLE USING KNN
count(data$Outlet_Size)#plyr

data=kNN(data,variable = "Outlet_Size",k=5)
sum(is.na(data$Outlet_Size))

#combi$Item_Fat_Content[combi$Item_Fat_Content == "LF"] = "Low Fat"
#combi$Item_Fat_Content[combi$Item_Fat_Content == "low fat"] = "Low Fat"
#combi$Item_Fat_Content[combi$Item_Fat_Content == "reg"] = "Regular"

#REPLACING MISSING VALUES
apply(is.na(data1), 2, which) #Item_weight and Outlet_size has na

#REPLACING NA OF ITEM WEIGHT WITH MEAN
data1$Item_Weight[is.na(data1$Item_Weight)]=mean(data1$Item_Weight, na.rm = TRUE)

#REPLACING NA OF OUTLET SIZE WITH MOST OCCURED CATEGORICAL VARIABLE USING KNN
count(data1$Outlet_Size)#plyr

data1=kNN(data1,variable = "Outlet_Size",k=5)
sum(is.na(data1$Outlet_Size))
train=data
test=data1


#STANDARDIZING THE DATA
train=scale(train)
test=scale(test)

#USING AS.FACTOR IN TRAIN DATASET
train$Item_Fat_Content=as.factor(train$Item_Fat_Content)
train$Item_Fat_Content=as.integer(train$Item_Fat_Content)

train$Item_Type=as.factor(train$Item_Type)
train$Item_Type=as.integer(train$Item_Type)

train$Outlet_Identifier=as.factor(train$Outlet_Identifier)
train$Outlet_Identifier=as.integer(train$Outlet_Identifier)

train$Outlet_Size=as.factor(train$Outlet_Size)
train$Outlet_Size=as.integer(train$Outlet_Size)

train$Outlet_Location_Type=as.factor(train$Outlet_Location_Type)
train$Outlet_Location_Type=as.integer(train$Outlet_Location_Type)

train$Outlet_Type=as.factor(train$Outlet_Type)
train$Outlet_Type=as.integer(train$Outlet_Type)

train=train[,-13]


#USING AS.FACTOR IN TEST DATASET
test$Item_Fat_Content=as.factor(test$Item_Fat_Content)
test$Item_Fat_Content=as.integer(test$Item_Fat_Content)

test$Item_Type=as.factor(test$Item_Type)
test$Item_Type=as.integer(test$Item_Type)

test$Outlet_Identifier=as.factor(test$Outlet_Identifier)
test$Outlet_Identifier=as.integer(test$Outlet_Identifier)

test$Outlet_Size=as.factor(test$Outlet_Size)
test$Outlet_Size=as.integer(test$Outlet_Size)

test$Outlet_Location_Type=as.factor(test$Outlet_Location_Type)
test$Outlet_Location_Type=as.integer(test$Outlet_Location_Type)

test$Outlet_Type=as.factor(test$Outlet_Type)
test$Outlet_Type=as.integer(test$Outlet_Type)

test=test[,-12]

test=test%>%mutate(Item_Outlet_Sales=0)
combi = rbind(train, test) # combining train and test datasets
dim(combi)

zero_index = which(combi$Item_Visibility == 0)
for(i in zero_index){
  
  item = combi$Item_Identifier[i]
  combi$Item_Visibility[i] = mean(combi$Item_Visibility[combi$Item_Identifier == item], na.rm = T)
}

zero_indextr = which(train$Item_Visibility == 0)
for(i in zero_indextr){
  
  item = train$Item_Identifier[i]
  train$Item_Visibility[i] = mean(train$Item_Visibility[train$Item_Identifier == item], na.rm = T)
}

zero_indexte = which(test$Item_Visibility == 0)
for(i in zero_indexte){
  
  item = test$Item_Identifier[i]
  test$Item_Visibility[i] = mean(test$Item_Visibility[test$Item_Identifier == item], na.rm = T)
}
x=model.matrix(train$Item_Outlet_Sales~.,data=train)[,-1]
y=train$Item_Outlet_Sales

reg.fit=glmnet(x,y,alpha = 1)
plot(reg.fit)

lambda_seq=10^seq(2, -2, by = -.1)

set.seed(123)
split = sample.split(train$Item_Outlet_Sales,SplitRatio =  0.8)

train1=subset(train,split == TRUE)
test1 = subset(train,split == FALSE)

#train1=sample(1:nrow(x),nrow(x)/2)
#test1=setdiff(1:nrow(x),train1)

#x_test = (-train1)
#y_test = y[test1]

cv_output=cv.glmnet(x, y, alpha = 1, lambda = lambda_seq)
best_lam <- cv_output$lambda.min
best_lam

lasso_best=glmnet(x,y,alpha = 1,lambda = best_lam)
pred=predict(lasso_best,s=best_lam,newx=x)

final=cbind(y,pred)
head(final)

mean((pred-y)^2)
rsme=sqrt(mean((pred-y)^2))
rsme




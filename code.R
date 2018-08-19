set.seed(1)
setwd("C:\\R Programming\\Women's Clothing Reviews")
data<-read.csv("data.csv",stringsAsFactors = FALSE,na.strings = c(""," "))

library(tm)
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(ngram)
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre-10.0.2')
library(RWeka)


data$notNull<-ifelse(is.na(data$Title)==TRUE,0,1)
data$Title[is.na(data$Title)]<-"Missing"


corpus = VCorpus(VectorSource(data$Title)) 
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removePunctuation)

exceptions   <- c("not","too","bad","just","no","but")
my_stopwords <- setdiff(stopwords("en"), exceptions)
corpus = tm_map(corpus, removeWords, my_stopwords)
corpus <- tm_map(corpus, lemmatize_strings)
corpus <- tm_map(corpus, PlainTextDocument)

corpus = tm_map(corpus, stemDocument)


#BigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 3))
#frequencies <- TermDocumentMatrix(corpus, control = list(tokenize = BigramTokenizer))

#frequencies = DocumentTermMatrix(corpus)
frequencies = DocumentTermMatrix(corpus,control = list(weighting = function(x) weightTfIdf(x, normalize = TRUE)))


sparse = removeSparseTerms(frequencies, 0.98)

TitleSparse = as.data.frame(as.matrix(sparse))
colnames(TitleSparse) = make.names(colnames(TitleSparse))
TitleSparse$Rating = as.factor(data$Rating)
TitleSparse$Recommended.IND = as.factor(data$Recommended.IND)



###Exploratory Data Analysis
library(ggplot2)
library(dplyr)

df<-TitleSparse %>% select(-Recommended.IND) %>% group_by(Rating) %>% summarise_all(mean)
df<-as.data.frame(df)

a<-apply(df[,-1], 1, sum)

##Calculating Ratios
df[,-1]<-df[,-1]/a

##Converting to percentage
df[,-1]<-df[,-1]*100

library(tidyr)

dfr<- df %>% gather(Key,Value,-Rating)

library(ggplot2)
ggplot(dfr, aes(x = Key, y = log(Value)+3,color = as.factor(Rating))) + 
  geom_boxplot()



df<-TitleSparse %>% select(-Rating) %>% group_by(Recommended.IND) %>% summarise_all(mean)
df<-as.data.frame(df)

a<-apply(df[,-1], 1, sum)

##Calculating Ratios
df[,-1]<-df[,-1]/a

##Converting to percentage
df[,-1]<-df[,-1]*100


dfr<- df %>% gather(Key,Value,-Recommended.IND)

ggplot(dfr, aes(x = Key, y = log(Value)+3,color = as.factor(Recommended.IND))) + 
  geom_boxplot()

##People who do not add a title tend to hate a product and give bad ratings.
##If the title contains perfect and comfort, the product is recommended and rated better.



TitleSparse$Rating<-NULL
TitleSparse$Recommended.IND<-NULL
TitleSparse$Positive.Feedback.Count<-cut(data$Positive.Feedback.Count,breaks = c(0,1,5,20,50,200),include.lowest = TRUE)
levels(TitleSparse$Positive.Feedback.Count)<-c("Very less","less","Above Average","High","Very High")
df<-TitleSparse %>% group_by(Positive.Feedback.Count) %>% summarise_all(mean)
df<-as.data.frame(df)

a<-apply(df[,-1], 1, sum)

##Calculating Ratios
df[,-1]<-df[,-1]/a

##Converting to percentage
df[,-1]<-df[,-1]*100


dfr<- df %>% gather(Key,Value,-Positive.Feedback.Count)

ggplot(dfr, aes(x = Key, y = Value,color = as.factor(Positive.Feedback.Count))) + 
  geom_boxplot()

ggplot(data, aes(x = Age, fill=as.factor(Rating))) + geom_histogram(stat="bin",bins=15)
ggplot(data, aes(x = Age, fill=as.factor(Recommended.IND))) + geom_histogram(stat="bin",bins=15)
ggplot(data, aes(x = Age, fill=as.factor(Division.Name))) + geom_histogram(stat="bin",bins=15)
ggplot(data, aes(x = Age, fill=as.factor(Department.Name))) + geom_histogram(stat="bin",bins=15)


explore<-data %>% group_by(Rating) %>% summarise(median(Age))
print(explore)
explore<-data %>% group_by(Recommended.IND) %>% summarise(median(Age))
print(explore)
explore<-data %>% group_by(Division.Name) %>% summarise(median(Age))
print(explore)
explore<-data %>% group_by(Department.Name) %>% summarise(median(Age))
print(explore)

explore<-data %>% group_by(Department.Name) %>% summarise(median(Age))
print(explore)



TitleSparse$Positive.Feedback.Count<-NULL
TitleSparse$Recommended.IND<-as.factor(data$Recommended.IND)
TitleSparse$containsdots<-ifelse(grepl("\\...",data$Title),1,0)
TitleSparse$containsexclamation<-ifelse(grepl("\\!",data$Title),1,0)
TitleSparse$containsquestionmark<-ifelse(grepl("\\?",data$Title),1,0)

explore<-TitleSparse %>% group_by(containsdots) %>% summarise(mean(as.numeric(Recommended.IND)-1))
print(explore)

explore<-TitleSparse %>% group_by(containsexclamation) %>% summarise(mean(as.numeric(Recommended.IND)-1))
print(explore)

explore<-TitleSparse %>% group_by(containsquestionmark) %>% summarise(mean(as.numeric(Recommended.IND)-1))
print(explore)




library(caTools)
splits<-sample.split(TitleSparse,SplitRatio = 0.8) 
train<-subset(TitleSparse,splits==TRUE)
test<-subset(TitleSparse,splits==FALSE)

library(party)
library(caret)
output.tree <- train(factor(Recommended.IND)~.,data=train,method="ctree",trControl=trainControl(method="cv",number = 5))
predictionsctree<-predict(output.tree,test,type="prob")[,2]


predictionsctreetrain<-predict(output.tree,train,type="prob")[,2]
plot(output.tree)
print(output.tree)


modelglm<-train(factor(Recommended.IND)~.,data=train,method="glm",trControl=trainControl(method="cv",number = 5))
predictionsGLM<-data.frame(predict(modelglm,test,type="prob"))
predictionsGLM<-predictionsGLM[,2]


predictionsGLMtrain<-data.frame(predict(modelglm,train,type="prob"))
predictionsGLMtrain<-predictionsGLMtrain[,2]
print(table(predictionsGLM>0.5,test$Recommended.IND))
print(modelglm)



modelgbm<-modelglm<-train(factor(Recommended.IND)~.,data=train,method="gbm",trControl=trainControl(method="cv",number = 5))
predictionsGBM<-predict(modelgbm,test,type="prob")[,2]
predictionsGBMtrain<-predict(modelgbm,train,type="prob")[,2]
plot(modelgbm)
print(modelgbm)


library("xgboost")
library("Matrix")
library(dplyr)
train$Recommended.IND<-as.numeric(train$Recommended.IND)-1

data_variables <- as.matrix(train %>% select(-Recommended.IND))
data_label <- train[,"Recommended.IND"]
data_matrix <- xgb.DMatrix(data = data_variables, label = data_label)

numberOfClasses <- length(unique(train$Recommended.IND))
xgb_params <- list(booster = "gbtree","objective" = "binary:logistic",eta=0.1,gamma=5,max_depth=10)
xgbcv <- xgb.cv( params = xgb_params, data = data_matrix, nrounds = 100, nfold = 10, showsd = T, stratified = T, print.every.n = 10, early.stop.round = 20, maximize = F)


nround    <- xgbcv$best_iteration # number of XGBoost rounds
cv.nfold  <- 10

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
bst_model <- xgb.train(params = xgb_params,
                       data = data_matrix,
                       nrounds = nround)

test$Recommended.IND<-as.numeric(test$Recommended.IND)-1


test_matrix<-xgb.DMatrix(data = as.matrix(test %>% select(-Recommended.IND)))
predictionsXGBoost<-predict(bst_model,newdata=test_matrix)

predictionsXGBoosttrain<-predict(bst_model,newdata=data_matrix)
print(table(predictionsXGBoost>0.5,test$Recommended.IND))



#Stacking model modelGLM,bst_model(XGBOOST) and ctree and using linear model on top
df<-data.frame(train,a=predictionsGLMtrain,b=predictionsGBMtrain,c=predictionsXGBoosttrain,d=predictionsctreetrain)


library(class)

testdf<-data.frame(test,a=predictionsGLM,b=predictionsGBM,c=predictionsXGBoost,d=predictionsctree)


model<-lm(Recommended.IND~.,data=df)


predictions<-predict(model,testdf)
table(predictions>0.5,test$Recommended.IND)

##Accuracy increased by stacking models


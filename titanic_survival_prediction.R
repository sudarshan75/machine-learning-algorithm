#### Loading packages
library(ggplot2)
library(dplyr)
library(stringr)
library(mice)
library(randomForest)
library(xgboost)


#### Reading data
titanic_train<-read.csv("E:/kaggle/titanic/titanic_train.csv")
titanic_test<-read.csv("E:/kaggle/titanic/titanic_test.csv")


#### Combining the data
full=bind_rows(titanic_train,titanic_test)


#### Checking the str of data
str(full)


#### Summary statistics of the data
summary(full)


#### Creating a new column named title  from the name column and cheking the table
full$title<-gsub("(.*,)|(\\..*)","",full$Name)
table(full$title)

#### Repalcing the wrong input title and creating a rare_title to keep the titles with lowest frequency in one
full$title<-str_replace_all(full$title,"Mlle","Miss")

rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

full$title<-str_replace_all(full$title,"Ms","Miss")

full$title<-str_replace_all(full$title,"Mme","Mrs")

for (i in rare_title) {
  full$title<-str_replace_all(full$title,i,"rare")
}

table(full$Sex,full$title)



#### Splitting name column we get surname column
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])


#### Calculating the family size by adding SibSp(Siblings/Spouse) and Parch(Parent/Child) and the member himself(i.e=1)
full$Fsize <- full$SibSp + full$Parch + 1
full$Family <- paste(full$Surname, full$Fsize, sep='_')


#### Creating a new column which species either the family size is single(i.e,1),
#### Small(i.e,<5 & >1), Large(i.e,>5)
full$FsizeD[full$Fsize == 1] <- 'single'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'


####Taking out the deck name from the cabin column
full$Deck<-factor(sapply(full$Cabin, function(x) strsplit(x, "")[[1]][1]))



#### Missing data imputation using mice package 
factor_vars <- c('PassengerId','Pclass','Sex','Embarked',
                 'title','Surname','Family','FsizeD')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))
set.seed(129)
mice_mod <- mice(full[, !names(full) %in% c('PassengerId','Name','Ticket','Cabin','Family','Surname','Survived')], 
                 method='rf')
mice_outpuy<-complete(mice_mod)
full$Age<-mice_outpuy$Age


#### Categorizing age with 2 levels ,i.e, child and adult and creating a new column named mother 
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'
full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)



#### Removing the titles that are not important anymore
coln<-c("PassengerId","Name","Ticket","Cabin","Fsize","Family","Deck","Surname")
full[coln]<-lapply(full[coln],function(x) x<-NULL)


#### Splitting the data into training and testing data
titanic_train <- full[1:891,]
titanic_test <- full[892:1309,]



#### Preparing the model and predicting using randomforest model
rf_model<-randomForest(factor(Survived)~.,data = titanic_train)
rf_predict<-as.data.frame(predict(rf_model,titanic_test))
write.csv(rf_predict,file="E:/kaggle/titanic/rf_predict.csv")



#### Preparing the model and predicting the output using xgboost model 
y=as.factor(titanic_train$Survived)
titanic_train$Survived<-NULL
titanic_test$Survived<-NULL
colnames(train)
xgb <- xgboost(data = data.matrix(titanic_train), 
               label = y, 
               eta = 0.1,
               max_depth = 15, 
               nround=25, 
               subsample = 0.5,
               colsample_bytree = 0.5,
               seed = 1,
               eval_metric = "merror",
               objective = "multi:softmax",
               num_class=3,
               nthread = 3
)
xgb_p<-predict(xgb,data.matrix(titanic_test))
xgb_p=ifelse(xgb_p==1,0,1)
write.csv(xgb_p,file = "E:/kaggle/titanic/xg_p.csv")
colnames(titanic_train)


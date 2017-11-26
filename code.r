library(MMST)
#install.packages("klaR")
library(klaR)
#install.packages("GGally")
library(ggplot2)
library(GGally)

setwd("C:/Users/kanne/Desktop/STA545/HomeWorks/HW3")

d = read.table("diabetes.txt", 
               sep="", 
               fill=FALSE, 
               strip.white=TRUE)

diab<-d[,-1:-3]

colnames(diab)<-c("obs_number","glucose.area","insulin.area","SSPG","rel.wgt","fast.plas.glu","class")

names(diab)

unique(diab$class)


#####################################################
## a)GG Plot
#####################################################


set.seed(1)

train <- sample(1:nrow(diab), .66*nrow(diab))

diab_train <- diab[train,]

diab_test <- diab[-train,]

diab$class<-as.factor(diab$class)

ggpairs(diab, columns = 2:6)

ggpairs(diab, columns = 2:6, ggplot2::aes(colour=class))


#####################################################
##    LDA
#####################################################
                                                                  
lda.fit<-lda(class~.,data=diab_train)

lda.pred.test<-predict(lda.fit,newdata = diab_test)


y_hat_test <- as.numeric(lda.pred.test$class)

y_true_test=diab_test$class

y_true_test<-as.numeric(y_true_test)
lda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)

lda_test_error


#####################################################
## QDA
#####################################################

qda.fit<-qda(class~.,data=diab_train)

qda.pred.test<-predict(qda.fit,newdata = diab_test)

y_hat_test <- as.numeric(qda.pred.test$class)

y_true_test=diab_test$class

y_true_test<-as.numeric(y_true_test)

qda_test_error <- sum(abs(y_true_test - y_hat_test))/length(y_true_test)

qda_test_error

#####################################################
## c)
#####################################################

X<-data.frame(0,0.98,122,544,186,184,NA)

colnames(X)<-c("obs_number","glucose.area","insulin.area","SSPG","rel.wgt","fast.plas.glu","class")

lda.pred.y<-predict(lda.fit,newdata = X)

lda.pred.y

qda.pred.y<-predict(qda.fit,newdata = X)

qda.pred.y



#######################################################################################################################
##################################################     test & tarin partition
########################################################################################################################



#######################################################################################################################
##################################################     train

Selected<-read.csv("../HagitGadot/Selected.csv")

nn<-names(Selected)

t3<-Table1(data = Selected, x=nn,y="AgeStartAll")
tab1 <- train_test(data = Selected, train_name = "train", test_name = "test",prop = 0.8, seed = 123, tableone=T)
dim(train)
dim(test)

names(tab1)<-c("Variables","Measure","Population","train","test","Pval")
tab1 %>% filter('Pval' < 0.05)

##################################################     train/dev

tab2 <- train_test(data = train, train_name = "train2", test_name = "dev",prop = 0.8, seed = 123, tableone=T)

options(repr.plot.width = 10, repr.plot.height = 8)

ggplot() +
  geom_density(aes(x=train$AgeStartAll),color="red",alpha=0.3) +
  geom_density(aes(x=dev$AgeStartAll),color="red",alpha=0.3) +
  geom_density(aes(x=test$AgeStartAll),color="green",alpha=0.3)



#######################################################################################################################
##################################################    Dataset Inbalance (sampling)
########################################################################################################################
if(!require(imbalance)){install.packages("imbalance");require(imbalance)}
### ROSE = Random Over-Sampling Examples
if(!require(ROSE)){install.packages("ROSE");require(ROSE)}

table(train$AgeStartAll)
table(train$AgeStartAll)/length(test$AgeStartAll)*100

numPositive <- length(which(train$AgeStartAll == 1))
numNegative <- length(which(train$AgeStartAll == 0))
nInstances <- numNegative - numPositive
cbind(numPositive=numPositive,numNegative=numNegative,nInstances=nInstances)

##################################################    Under sampling
######################################################################

data_balanced_under <- ovun.sample(AgeStartAll ~ ., data = train, method = "under",N = numPositive*2)$data
table(data_balanced_under$AgeStartAll)

##################################################    over sampling
######################################################################

data_balanced_over <- ovun.sample(AgeStartAll ~ ., data = train, method = "over",N = numNegative*2)$data
table(data_balanced_over$AgeStartAll)

##################################################   over-Under sampling
######################################################################
data_balanced_both <- ovun.sample(AgeStartAll ~ ., data = train, method = "both", p=0.5,N = 4062, seed = 1)$data
table(data_balanced_both$AgeStartAll)
write.csv(data_balanced_both, "../HagitGadot/train_balanced_both.csv")

##################################################   Rose
######################################################################

data.rose <- ROSE(AgeStartAll ~ ., data = train, seed = 1)$data
table(data.rose$AgeStartAll)
write.csv(data.rose, "../HagitGadot/train_Rose.csv")

##Export Data##
#=-----------------------
train21<-train2
train2[1:1]
ncol(train2)
names(train2)

write.csv(train2, "../HagitGadot/train.csv")
write.csv(dev,"../HagitGadot/dev.csv")
write.csv(train,"../HagitGadot/TheTrain.csv",row.names=F)
write.csv(test,"../HagitGadot/TheTest.csv",row.names=F)

#######################################################################################################################
##################################################    Clustering method
########################################################################################################################

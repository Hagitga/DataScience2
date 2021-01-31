library(dplyr)
library(corrgram)
library(Hmisc)
library(ggplot2)
library(Hmisc)
library(graphics)
source("C:/Users/Oren Gadot/HagitGadot/DataScience/DataScience2/project/mechkar.R")
dev.off()
##########################################################
##########functions################################
##########################################################

outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}


missingMatrix <- function(data) {
  vn <- names(data)
  missdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    mv <- ifelse(is.na(data[[v]]),1,0)
    missdata[v] <- mv
  }
  missdata$row1 <- NULL
  return(missdata)
}

getMissingness <- function (data, getRows = FALSE) {
  require(dplyr)
  l <- nrow(data)
  vn <- names(data)
  nadf <- data
  cnt <- NULL
  miss <- function(x) return(sum(is.na(x)))
  for (n in vn) {
    nadf[[n]] <- ifelse(is.na(nadf[[n]]) == T, 1, 0)
    cnt <- rbind(cnt, data.frame(n, sum(nadf[[n]])))
  }
  names(cnt) <- c("var", "na.count")
  cnt$rate <- round((cnt$na.count/nrow(nadf)) * 100, 1)
  nadf$na.cnt <- 0
  nadf$na.cnt <- rowSums(nadf)
  cnt <- cnt %>% dplyr::arrange(desc(na.count)) %>% dplyr::filter(na.count > 
                                                                    0)
  totmiss <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::tally()
  idx <- NULL
  msg <- (paste("This dataset has ", as.character(totmiss), 
                " (", as.character(round(totmiss/nrow(data) * 100, 1)), 
                "%)", " complete rows. Original data has ", nrow(data), 
                " rows.", sep = ""))
  if (getRows == TRUE & totmiss != 0) {
    nadf$rn <- seq_len(nrow(data))
    idx <- nadf %>% dplyr::filter(na.cnt == 0) %>% dplyr::select(rn)
  }
  print(list(head(cnt, n = 10), msg))
  return(list(missingness = cnt, message = msg, rows = idx$rn))
}


miss.value.plot <- function(x, y, ...) {
  plot(x, y, ...)
  rug(jitter(x[is.na(y)], amount=.1), side=1, lwd=1.2)
  rug(jitter(y[is.na(x)], amount=.1), side=2, lwd=1.2)
}

##############between ctegorical - function #######

cv.test = function(x,y) {
  dd = chisq.test(x, y, correct=FALSE)
  CV = sqrt(dd$statistic /
              (length(x) * (min(length(unique(x)),length(unique(y))) - 1)))
  #print.noquote("Cramér V / Phi:")
  return(cbind('Cramér V'=as.numeric(CV),'p-value'=dd$p.value))
}

##########################################################
##########################################################
##########################################################


##Import Data##
#=-----------------------
Flat<-read.csv("../HagitGadot/Flat.csv")

summary(Flat)


##Category loop (factor) 
#---------------------------------------------------------------

#numeric/categorical data
numlist1 <- NULL
catlist1<-NULL
for(i in names(Flat)) {
  if((is.numeric(Flat[[i]])==TRUE)&(length(unique(Flat[[i]]))>15)) {
    numlist1 <- c(numlist1, i)
  }else
  {
    catlist1 <- c(catlist1, i) 
  }
}

FlatX<-Flat
rm(Flat)

for (i in catlist1) {
  FlatX[[i]]<-factor(FlatX[[i]])
}

rm(i)
summary(FlatX)

#####New Variables
#######################################

FlatX$AgeStopUseMar<-FlatX$Age-(FlatX$LastUseMar/365)
FlatX$HowLongStopeMar<-FlatX$Age-FlatX$AgeStopUseMar
FlatX$HowLongUseMar<-ifelse((FlatX$AgeStopUseMar-FlatX$FirstAgeMar>0),(FlatX$AgeStopUseMar-FlatX$FirstAgeMar),0)

FlatX$AgeStopUseCoc<-FlatX$Age-(FlatX$LastTimeUseCoc/365)
FlatX$HowLongStopCoc<-FlatX$Age-FlatX$AgeStopUseCoc
FlatX$HowLongUseCoc<-ifelse((FlatX$AgeStopUseCoc-FlatX$AgeStartCoc>0),(FlatX$AgeStopUseCoc-FlatX$AgeStartCoc),0)

FlatX$AgeStopUseMeth<-FlatX$Age-(FlatX$LastTimeUseMeth/365)
FlatX$HowLongStopMeth<-FlatX$Age-FlatX$AgeStopUseMeth
FlatX$HowLongUseMeth<-ifelse(FlatX$AgeStopUseMeth-FlatX$AgeStartMeth>0,FlatX$AgeStopUseMeth-FlatX$AgeStartMeth,0)

FlatX$AgeStopUseHer<-FlatX$Age-(FlatX$LastTimeUseHer/365)
FlatX$HowLongStopHer<-FlatX$Age-FlatX$AgeStopUseHer
FlatX$HowLongUseHer<-ifelse(FlatX$AgeStopUseHer-FlatX$FirstAgeHer>0,FlatX$AgeStopUseHer-FlatX$FirstAgeHer,0)


summary(FlatX$HowLongUseMar)
summary(FlatX$HowLongUseCoc)
summary(FlatX$HowLongUseHer)
summary(FlatX$HowLongUseMeth)


##############################################################
######## outcome - y= Starts Heavy Drud before age 24#########
#---------------------------------------------------
FlatX<-FlatX%>% mutate(AgeStartAll= ifelse((AgeStartMeth<24|AgeStartCoc<24|FirstAgeHer<24),1,0))
FlatX$AgeStartAll[is.na(FlatX$AgeStartAll)]<-0

table(FlatX$AgeStartAll)

#####Checking outcome

#######"used" variables
Used_var=c("X.1","X","SEQN","LastTimeUseMeth","LastTimeUseCoc","LastTimeUseHer","AgeStartMeth","AgeStartCoc","FirstAgeHer", "HowLongUseMeth","AgeStopUseHer","HowLongStopHer",
           "HowLongUseHe","AgeStopUseCoc","HowLongStopCoc","HowLongUseCoc","AgeStopUseMeth","HowLongStopMeth","HowLongUseMeth","AgeStopUseHer","HowLongStopHer","HowLongUseHer",
           "AgeStopUseCoc","HowLongStopCoc","HowLongUseCoc","AgeStopUseMeth","HowLongStopMeth")


#######################EDA 
FlatDrug<-FlatX
rm(FlatX)

################################################################################
########   EDA  #################################################################
################################################################################
### select only the relevant columns in the Flat file (remove the cols that calculated the outcome variable(y))

FlatDrugNames<-setdiff(names(FlatDrug),Used_var)
FlatDrug<-select(FlatDrug,FlatDrugNames)
nn<-names(FlatDrug)


########1. table1 shows diffrences between the vars to Y #######################

FlatDrug$AgeStartAll<-factor(FlatDrug$AgeStartAll)
t1<-Table1(data = FlatDrug[,1:length(FlatDrug)], x=nn,y="AgeStartAll")

T1<-data.frame(t1)
T1[T1$Var.2=="Median (IQR)",]
##- most of the numeric vars has p value distinct -  the diffrancess are significant

T1[T1$Var.2=="2",]
##- most of the categoric vars has p value distinct -  the diffrancess are significant



######2. Distribution , Descriptive Statistics and ottliiers statistics - mechkar::exploreData()


summary(FlatDrug)
FlatDrug$AgeStartAll<-factor(FlatDrug$AgeStartAll)
exploreData(data=FlatDrug,x=names(FlatDrug),y="AgeStartAll")


#####################################################################################-------------

####################  categoric varaibles
numlist <- NULL
catlist<-NULL
for(i in names(FlatDrug)) {
  if((is.numeric(FlatDrug[[i]])==TRUE)&(length(unique(FlatDrug[[i]]))>10)) {
    numlist <- c(numlist, i)
  }else
  {
    catlist <- c(catlist, i) 
  }
}

summary(FlatDrug)

for (i in catlist) {
  FlatDrug[[i]]<-factor(FlatDrug[[i]])
}



######## EDA - corrlation ##################2222222222222222222222222222222222222222222222222222222222222222222222
#-----------------------
### 1. visual - between numeric variables#######
options(repr.plot.width = 15, repr.plot.height = 12)
dfcormat <- cor(FlatDrug[,numlist], use = "pairwise.complete.obs",method = "spearman")
corrgram::corrgram(dfcormat)

###2'nd way (matrix)
##############################################


library(Hmisc)

cormatrix <- rcorr(as.matrix(FlatDrug[,numlist]))

## correlation matrix
cormatrix$r
cormatrix$n%>%filter(cormatrix$P<0.05)

## p-values
round(cormatrix$P,3)

## visual impression of the correlation
library(corrplot)
corrplot(cormatrix$r,type = "upper",method = "color")


#####################################################################################################
#-Categoric Correlation---------------------------------------------------------------------------------

cv.test(FlatDrug$Gender,FlatDrug$TimesInMonth)



'd<-setdiff(CatNames,names(FlatDrug))

'

cats_corr <- NULL

for (c in catlist){
  for(c2 in catlist[-1]){
    tryCatch({cats_corr <- rbind(cats_corr, cbind(c,c2,(cv.test(FlatDrug[[c]],FlatDrug[[c2]]))))},
    error=function(cond){print(paste ("vars:",c,c2,"not working together"))})
    #cats_corr <- rbind(cats_corr, cbind(c,c2,(cv.test(FlatDrug[[c]],FlatDrug[[c2]]))))
  }
}

dev.off()

CatCor<-data.frame(cats_corr)

CatCor$Cramér.V <- round(as.numeric(CatCor$Cramér.V),5)
CatCor$p.value <- round(as.numeric(CatCor$p.value),5)


ggplot(data = CatCor, aes(x = c, y = p.value)) +
  geom_point() +
  labs(title = "Vars",subtitle = "missing data accounted for")+ geom_abline(slope =  0,intercept = 0.05,colour = "red")



#######################################################################################
####Outliers
#######################################################################################


##numeric Vars - Distribution test:###########################


outAll<-outlierMatrix(FlatDrug,threshold=1.5)

for (i in names(outAll)){
  outAll[[i]]<-ifelse(is.na(outAll[[i]]),0,outAll[[i]])
}

outList<-names(outAll)[colSums(outAll==1)>0]


res1 <- NULL
for (n in outList) {
  out <- FlatDrug[[n]]
  non <- FlatDrug[[n]][which(outAll[[n]]==0)]
  #outnum <- round((length(out) - length(non))/(length(out) * 100),3)
  outnum <- length(out) - length(non)
  pval <- suppressWarnings(ks.test(out, non)$p.value)
  res1 <- rbind(res1, cbind(var=n, outliers_num=outnum, distribution_changed=ifelse(pval<0.05,"+","-")))
}
res1
#################################################################################
############Outliers correlation test
##---------------------------------------------------- a list with outliers


library(cocor)

res2 <- NULL
for (n in outList) {
  out <- FlatDrug[[n]]
  non <- FlatDrug[[n]][which(outAll[[n]]==0)]
  DrugY_out <- ifelse(FlatDrug$AgeStartAll==1,1,0)
  DrugY_non <- DrugY_out[which(outAll[[n]]==0)]
  outdf <- data.frame(x_out=out,y_out=DrugY_out)
  nondf <- data.frame(x_non=non,y_non=DrugY_non)
  cr <- cocor(~ x_out + y_out | x_non + y_non, data=list(outdf,nondf))
  pval <- cr@fisher1925$p.value
  res2 <- rbind(res2, cbind(var=n, correlation_changed=ifelse(pval<0.05,"+","-")))
}
res2

###########################Outliers conclusion###############################

res <- inner_join(data.frame(res1),data.frame(res2),by="var")
res$drop <- ifelse(res$distribution_changed=="+" & res$correlation_changed == "+","No","Yes")
res
outListDel<- res$var[res$drop=="Yes"]

rm(res1,res2)
###########################Outliers implementation (=NA) ###########################################################
FlatDrug2<-FlatDrug
for (n in outListDel) {
  FlatDrug2[[n]][outAll[[n]]==1]<-NA
}

rm(FlatDrug)
#######################################################################################################################
##################################################     Missings
########################################################################################################################

################################HEAT MAP

library(naniar)
options(repr.plot.width = 15, repr.plot.height = 8)
vis_miss(FlatDrug2,warn_large_data=F)

df_na <- missingMatrix(FlatDrug2)

dfna_sum <- df_na
dfna_sum$pct <-rowSums(df_na)/length(df_na)

#df_na%>%filter(df_na==1)%>%tally()

#1.####################### removing missing rows (over 70% missing in a row) - meaning 1025 rows
dfna_sum%>%filter(dfna_sum$pct >= 0.7)%>%tally()


FlatDrug3<-NULL
FlatDrug2$drop <- ifelse(dfna_sum$pct >= 0.7,1,0)
FlatDrug3 <- FlatDrug2 %>%filter(FlatDrug2$drop==0)
FlatDrug3$drop <- NULL

rm(FlatDrug2)



#2.###missing data - over 40% missing-> Categoy:###########################

mt <- getMissingness(FlatDrug3,getRows = TRUE)
over40miss<-mt$missingness$var[mt$missingness$rate>40]
under40miss<-mt$missingness$var[mt$missingness$rate<=40]

FlatDrug4<-FlatDrug3
rm(FlatDrug3)


summary(FlatDrug4[over40miss])
summary(FlatDrug4[under40miss])

for (n in over40miss){
  #declare borders for the numeric data(+-sd)
  if(length(unique(FlatDrug4[[n]]))>15){
    FlatDrug4[[n]]<-as.numeric(FlatDrug4[[n]])
    t0<-mean(FlatDrug4[[n]],na.rm = T)
    t1<-t0+sd(FlatDrug4[[n]],na.rm = T)
    t2<-t0+2*sd(FlatDrug4[[n]],na.rm = T)
    t3<-t0+3*sd(FlatDrug4[[n]],na.rm = T)
    tm1<-t0-sd(FlatDrug4[[n]],na.rm = T)
    tm2<-t0-2*sd(FlatDrug4[[n]],na.rm = T)
    tm3<-t0-3*sd(FlatDrug4[[n]],na.rm = T)
    
    #divide to categoric
    FlatDrug4[[n]][FlatDrug4[[n]]<tm3]<-"-30"
    FlatDrug4[[n]][FlatDrug4[[n]]>=tm3 & FlatDrug4[[n]]<tm2]<-"-25"
    FlatDrug4[[n]][FlatDrug4[[n]]>=tm2 & FlatDrug4[[n]]<tm1]<-"-15"
    FlatDrug4[[n]][FlatDrug4[[n]]>=tm1 & FlatDrug4[[n]]<t0]<-"-5"
    FlatDrug4[[n]][FlatDrug4[[n]]>=t0 & FlatDrug4[[n]]<t1]<-"5"
    FlatDrug4[[n]][FlatDrug4[[n]]>=t1 & FlatDrug4[[n]]<t2]<-"15"
    FlatDrug4[[n]][FlatDrug4[[n]]>=t2 & FlatDrug4[[n]]<t3]<-"25"
    FlatDrug4[[n]][FlatDrug4[[n]]>t3]<-"30"
    #numToCat40 <- c(numToCat40,n)
     }
    FlatDrug4[[n]]<-ifelse((is.na(FlatDrug4[[n]])==TRUE),"999",FlatDrug4[[n]])
    FlatDrug4[[n]]<-factor(FlatDrug4[[n]])
   }


summary(FlatDrug4[over40miss])


#3.######### categoric vars with less then 40% missing- remain categoric, the NA replaced with 999

under40miss

under40num<-NULL
for (m in under40miss){
  if((length(unique(FlatDrug4[[m]]))>15)==TRUE){
  under40num<-c(under40num,m)  
  }else{
    FlatDrug4[[m]]<-ifelse((is.na(FlatDrug4[[m]])==TRUE),"999",FlatDrug4[[m]])
    FlatDrug4[[m]]<-factor(FlatDrug4[[m]])
  }
}
summary(FlatDrug4[under40miss])

under40num

#####---No varibles left

#write.csv(FlatDrug4,"../HagitGadot/FlatDrugDS.csv")

summary(FlatDrug4)

#######################################################################################################################
##################################################     End of flat file
########################################################################################################################


#========================================================================================================================
#========================================================================================================================
#========================================================================================================================

#######################################################################################################################
##################################################     feature engineering
########################################################################################################################

#####New Variables were done before the EDA: 
#######################################


one_hot_encode <- function(df,var) {
  df[[var]] <- factor(df[[var]])
  for (l in levels(df[[var]])) {
    df[[paste(var,l,sep="_")]] <- ifelse(df[[var]]==l,1,0)
  }
  return(df)
}



dummy_encode <- function(df,var) {
  df[[var]] <- factor(df[[var]])
  for (l in setdiff(levels(df[[var]]),"999") ){
    df[[paste(var,l,sep="_")]] <- ifelse(df[[var]]==l,1,0)
  }
  return(df)
}



minmax <- function(x) {
  return(((x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))))
}
######################################################################################################
##########################Feature transfpormation ####################################################
######################################################################################################
DummyDF<-FlatDrug4[,1:ncol(FlatDrug4)-1]


vars<-NULL
for (o in names(DummyDF)) {
  if(is.factor(DummyDF[[o]])){
    vars<-c(vars,o)
    DummyDF<-dummy_encode(DummyDF,o)
  }
}

vars

notDummy<-setdiff(names(FlatDrug4),c(vars,"SEQN","AgeStartAll"))



###############numeric minmax trasformation


for (o in notDummy) {
  DummyDF[[paste(o,"num",sep="_")]]<-(minmax(DummyDF[[o]]))
}

hist(DummyDF$HHPersonAge)
hist(DummyDF$HHPersonAge_num)

####remove the originals Variables

TransDFnm<-setdiff(names(DummyDF),names(FlatDrug4))


#############featured DataFrame


FeatureDF<-select(DummyDF,TransDFnm)
FeatureDF<-cbind(FeatureDF,FlatDrug4[names(FlatDrug4)[length(FlatDrug4)]])
summary(FeatureDF)

#######################################################################################################################
##################################################    to  feature selection
########################################################################################################################

names(FeatureDF)
write.csv(FeatureDF,"../HagitGadot/FeatureDF.csv")

#######################################################################################################################
##################################################     feature selection
########################################################################################################################
nm<-names(FeatureDF)
varsel<-data.frame(nm)
#str(T2)
#######################################################################################################################
##################################################     univariate - couldn'n do it in Python (table1)


t2<-Table1(data = FeatureDF[,1:length(FeatureDF)], x=nm,y="AgeStartAll")
t2$Pval<-gsub("<", "", t2$`p-value`, fixed=TRUE)
T2<-data.frame(t2)
summary(T2)

T2$Pval<-as.numeric(T2$Pval)
T2$uni<-ifelse((is.numeric(T2$Pval)&T2$Pval<0.05),1,NA)


T3<-T2%>%filter(is.na(T2$uni)==FALSE)

T3

varsel2<-merge(varsel,T3,by.x ="nm",by.y = "Variables", all.x = TRUE )
varsel2

varsel<-select(varsel2,"nm","uni")
varsel$uni<-ifelse(is.na(varsel$uni),0,varsel$uni)

write.csv(varsel,"../HagitGadot/VarSelect.csv")
#######################################################################################################################
##################################################     continue with "Feature selection-Drugs.py" 
